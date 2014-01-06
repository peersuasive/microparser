--[[----------------------------------------------------------------------------

microparser.lua

LUCE C++ (micro)parser

    @alias meta

    @author Christophe Berbizier (cberbizier@peersuasive.com)
    @license GPLv3
    @copyright 

(c) 2013, Peersuasive Technologies

------------------------------------------------------------------------------]]

-- TODO: fully reimplement this with smarter rules to be more generic and less verbose

local mt = {}
mt.__index = mt

local lpeg = require"lpeg"
local re   = require"re"

local C, P, R, S, V = lpeg.C, lpeg.P, lpeg.R, lpeg.S, lpeg.V
local Carg, Cc, Cp, Ct, Cs, Cf, Cg, Cb, Cmt
    = lpeg.Carg, lpeg.Cc, lpeg.Cp, lpeg.Ct, lpeg.Cs, lpeg.Cf, lpeg.Cg, lpeg.Cb, lpeg.Cmt
local match = lpeg.match

-- Single platform independant line break which increments line number

-- Line continuation
line_extend      = P[[\]] * S"\r\n"
local spaces     = S" \t\r\n" + line_extend
local whitespaces = spaces^0

local letter     = R("az", "AZ")
local idchar     = letter + P"_"
local zero       = P"0"
local nonzero    = R"19"
local digit      = R"09"
local integer    = P"-"^-1 * (zero + nonzero*digit^0)

local f_exponent = S"eE" + S"+-"^-1 * R"09"^1
local f_terminator = S"fFlL"
local float      = R"09"^1 * f_exponent * f_terminator^-1
                   + R"09"^0 * P"." * R"09"^1 * f_exponent^-1 * f_terminator^-1
                   + R"09"^1 * P"." * R"09"^0 * f_exponent^-1 * f_terminator^-1

local number     = float + integer

local keywords   = (P"const" + P"static" + P"noexcept" +P"override" + P"friend"
                   + P"break" + P"case" + P"continue" + P"default" + P"do" 
                   + P"register" + P"return" + P"signed" + P"sizeof" 
                   + P"else" + P"extern" + P"for" + P"goto" + P"if" 
                   + P"switch" + P"typedef" + P"unsigned"
                   + P"volatile" + P"while"
                   + P"explicit" 
                   + P"virtual") * #(spaces + P";" + P"{" + P[[\]])

local reserved   = ((P"public" + P"private" + P"protected") * spaces^0 * P":") 
                    + ((P"class" + P"struct" + P"enum" + P"union"
                    + P"break" + P"case" + P"const" + P"continue" + P"default" + P"do" 
                    + P"register" + P"return" + P"signed" + P"sizeof" 
                    + P"else" + P"extern" + P"for" + P"goto" + P"if" 
                    + P"switch" + P"typedef" + P"unsigned"
                    + P"volatile" + P"while") * #(spaces + P";" + P"{" + P[[\]]) )

local identifier = idchar * (idchar+digit)^0 - keywords
local string     = S([["']]) * (spaces^0 * identifier * spaces^0)^0 * S([["']])

local single_line_comment = P"//" * (1 - P"\n")^0
local multi_line_comment = P"/*" * (1 - P"*/")^0 * P"*/"
local comment    = single_line_comment + multi_line_comment

local ignore     = whitespaces + comment

local juce_macros = P"JUCE_" * identifier
local juce_macros_f = juce_macros * spaces^0
                     * P{ "(" * spaces^0 * ((1-S"()")+V(1))^0 * spaces^0 * P")" }^-1 * spaces^0 * P";"^0

local main_class = nil
local constructors = {}
local callbacks = {}
local methods   = {}
local vars      = {}
local classes   = {}

local grammar = P{ "grammar";

    include     = P"#" * S" \t"^0 * P"include" * S" \t"
                  * S([['"<]]) * (1 - S([['">]]))^1 * S([['">]]),
    
    prewords    = (P"#" * S" \t"^0 * (P"define" + P"elif" + P"else" + P"endif" + P"#" +
                       P"error" + P"ifdef" + P"ifndef" + P"if" + P"import" +
                       P"include" + P"line" + P"pragma" + P"undef" + P"using"
                  ) * #spaces),

    preprocessor = Cg(Cs(V"include" + V"prewords"
                   * S" \t"^0
                   * V"id"^0 * S" \t"^0
                   * ( V"value" )^0
                   )/"", "preprocessor"),

    visibility  = Cg(P"public" + P"private" + P"protected", "visibility") 
                  * spaces^0 * P":" * (P" ")^0 * (P"\r"+P"\n")^1,

    id          = (identifier - keywords - V"prewords") * #spaces^0,
    class_meth  = P"::" * spaces^0 * V"id",

    value       = (P"=" * spaces^0)^0 
                  * Cg( ( 
                        Ct((Cg(number, "vvalue") * Cg(Cc"number", "vtype"))) 
                      + Ct((Cg(string, "vvalue") * Cg(Cc"string", "vtype")))
                      + Ct((Cg(V"block","vvalue") * Cg(Cc"array" , "vtype")))
                      + Ct((Cg(P"true" + P"false", "vvalue") * Cg(Cc"bool", "vtype")))
                      + Ct((Cg(V"object", "vvalue") * Cg(Cc"object", "vtype")))
                    ), "value"),

    object      = (P"new" * spaces)^0 
                  --* Ct( Cg( (( V"id_type_t" - reserved ) + (V"id_type" - reserved)), "type" ) 
                  * C( (( V"id_type_t" - reserved ) + (V"id_type" - reserved)) ) 
                  * V"args"^0,

    id_type     = Cg(V"id" * (V"class_meth")^0, "return_type") * V"pointers"^0,
    id_name     = Cg(V"id" * (V"class_meth")^0, "name"),
    id_type_t   = Cg(V"id" * (V"class_meth")^0, "return_type") * spaces^0 
                  * V"template_call" * spaces^0 
                  * (V"class_meth")^0 * spaces^0 
                  * V"pointers"^0,

    pointer     = spaces^0 * C(P"*" + P"&"),
    pointers    = spaces^0 * Cg( Ct( V"pointer" * V"pointer"^0) , "pointers"),

    tmpl_call_arg = Ct( 
                     Cg(V"id" * spaces^0 * V"template_call"^0 * spaces^0,"type") 
                     * V"pointers"^0 * spaces^0 
                     * #(P"," + P">") ),

    template_call = Cg( Ct(
                    P"<" * spaces^0
                    * (V"tmpl_call_arg" * spaces^0 * (P"," * spaces^0 * V"tmpl_call_arg")^0)^0
                    * spaces^0 * P">"
                  ), "template_call"),

    vars_a      = Cg(
                  (V"id_type_t" + V"id_type") 
                  * spaces^0 * P"const"^-1 * spaces^0
                  * (V"id_type_t" + V"id_type")
                  , "vars")
                  * #spaces^0 
                  * #(P":" + P"," + P";"),

    --bool selected : 1;
    bitfield    = Ct(Cg(
                    (V"id_type_t" + V"id_type") * spaces^0 * (V"id_type_t" + V"id_type")
                    * spaces^0 * P":" * spaces^0 * R"19" 
                    * spaces^0
                    * P";"
                  , "vars")
                  * Cg(Cc"var", "type")
                  ) / function(a)
                      vars[#vars+1] = a
                  end
                  ,

    vars        = V"bitfield" + Ct(
                    Cg(
                        V"vars_a" * spaces^0 
                        * (P"," * spaces^0 * (V"id_type_t"+V"id_type"))^0 
                        * spaces^0 
                        * P";"
                    , "vars")
                    * Cg(Cc"var", "type")
                  )
                  / function(a)
                    vars[#vars+1] = a
                  end
                  ,

    tmpl_arg    = (
                    (P"class"+P"typename") * spaces^0
                    * C(V"id") * spaces^0 
                    * V"pointers"^0 * spaces^0 
                    * #(P"," + P">") 
                    ),
    template    = Ct( P"template" * spaces^0
                  * P"<" * spaces^0
                  * (V"tmpl_arg" * spaces^0 * (P"," * spaces^0 * V"tmpl_arg")^0)^0
                  * spaces^0 * P">"
                  ),

    kwc         = Cg(keywords, "is_const") * spaces^0,
    arg_m       = Ct( V"kwc"^0 * spaces^0 * (V"id_type_t" + V"id_type") * spaces^0 
                      * V"id_name" * spaces^0 
                      * V"value"^0 * spaces^0 
                      * #(P"," + P")")
                    ),
    arg_s       = Ct( V"kwc"^0 * spaces^0 * (V"id_type_t" + V"id_type") * spaces^0 * #(P"," + P")")),
    arg_v       = Ct( V"kwc"^0 * spaces^0 * (V"value") * spaces^0 * #(P"," + P")") ),
    arg         = V"arg_m" + V"arg_s" + V"arg_v",

    --args_list   = keywords^0 * spaces^0 * V"arg" * spaces^0,
    args_list   = V"arg" * spaces^0,
    args        = Cg ( Ct(
                    P"(" * spaces^0
                    * (V"args_list" * spaces^0 * (P"," * spaces^0 * V"args_list")^0)^0 
                    * spaces^0 * P")"
                  ) , "args" ),

    pure        = spaces^0 * P"=" * spaces^0 * P"0" * spaces^0 * #P";",

    method_a    = Ct( 
                    Cg( V"template" * spaces^0, "template" )^0
                    * (V"id_type_t" + V"id_type") * spaces^0 
                    * (V"id_name" * spaces^0)^1 * spaces^0 
                    * V"args"
                  ) * spaces^0
                  * ( (keywords + juce_macros) * spaces^0)^0
                  * ( V"block" + P";" + V"pure" )
                  ,

    method      = Ct(
                    Cg( V"method_a" , "method" )
                    * Cg( Cc"method", "rule" ) 
                    * Cg(Cb("visibility"), "visibility") 
                  ) / function(v)
                      methods[#methods+1] = v
                  end
                  ,

    callback    = P"virtual" * spaces * #P"void"
                  * Ct( 
                        Cg( V"method_a", "callback" ) 
                        * Cg( Cc"callback", "rule" )
                        * Cg( Cb("visibility"), "visibility" )
                  ) / function(v)
                      callbacks[#callbacks+1] = v
                  end
                  ,

    -- skip
    operators   = Cg(Cs(
                  (V"id_type_t" + V"id_type")^0 * spaces^0
                  * P { "operator" * (1 - (P";" + P"{"))^0 }
                  * ( P";" + V"block" )
                  )/"", "operators"),

    -- skip, except when used as default value
    block       = P{ "{" * spaces^0 * ((1- S"{}") + V(1))^0 * spaces^0 * P"}" },

    -- TODO: parse class to get methods/functions and ihnerited ones
    union       = P"union" * spaces^0 * V"block" * P";",
    enum        = P"enum" * spaces^0 * V"id"^0 * spaces^0 * V"block" * spaces^0 * V"id"^0 * spaces^0 * P";",

    class_f     = ( (P"class" * Cg(Cc"class", "rtype")) + (P"struct" * Cg(Cc"struct", "rtype")) ) * spaces
                  * Cg( C(V"id_type") * spaces^0, "class_name" )^1 * spaces^0
                  * P{ ":" * spaces^0 * ((1-S"{;") + V(1))^0 * #S"{;" }^0 * spaces^0
                  * #S"{;"
                  * Ct( Cg(Cb("class_name"), "name") * Cg(Cb("rtype"), "rtype") )
                  ,

    -- class and struct set default visibility
    class_d     = Cg( #(P"class"  * spaces) * Cc"private", "visibility"),
    struct_d    = Cg( #((P"struct") * spaces) * Cc"public", "visibility"),
    class       = V"template"^0 * spaces^0
                  * (V"class_d" + V"struct_d")
                  * Ct( Cg( V"class_f", "class" ) * spaces^0
                      * (V"block")^0 *spaces^0
                      * P";"
                      * Cg(Cc"class", "type")
                  ) / function(v)
                      classes[#classes+1] = v
                  end,

    closing = P"}" * spaces^0 * P";",

    main_class  = V"template"^0 * spaces^0 * (V"class_d" + V"struct_d") * Cg(
                  Cmt( V"class_f" , function(s, i, c, ...)
                        local a = {...}
                        if ( main_class ) then
                            return false
                        else
                            main_class = c
                            return i+1, c
                        end
                  end)
                  , "main_class"),

    constructor_a   = P"virtual"^0 * spaces^0 * Ct( 
                        Cg( V"id", "name" ) * spaces^0 
                        * V"args" * spaces^0 
                        * ( (keywords + juce_macros) * spaces^0)^0
                        * ( V"block" + P";")
                      ),
    constructor     = Ct( 
                        Cg( V"constructor_a", "constructor" ) 
                        * Cg( Cc"constructor", "rule" ) 
                        * Cg( Cb("visibility"), "visibility")
                       ) / function(v)
                           constructors[#constructors+1] = v
                       end
                       ,

    destructor      = P"virtual"^0 * spaces^0 
                      * P"~" * spaces^0 * V"id" * spaces^0 * P"(" * spaces^0 * P")" * spaces^0
                      * ( (keywords+juce_macros) * spaces^0 )^0
                      * (V"block" + P";"),
    
    eoc             = P";",
    statements =
        comment
        + juce_macros_f
        + V"preprocessor"
        + V"main_class"
        + V"callback"
        + V"visibility"
        + V"destructor"
        + V"constructor"
        + keywords
        + V"operators"
        + V"union"
        + V"enum"
        + V"class"
        + V"method"
        + V"vars"
        + spaces
        + V"closing" -- remaining };, left by class
        + V"eoc"
        ,

    error = (Cp() * C(P(1) ^ -8)) / function(pos, where)
      error(("Tokenising error near '%s'")
        :format(where))
    end,

    finish = -P(1) + V"error",

    grammar = Ct(V"statements"^0) * V"finish",
}

local function parse(self, file)
    assert(file, "Missing file to parse")
    local file = io.open(file)
    local data = file:read("*a")
    file:close()
    local r = grammar:match( data )
    return r and {
        class        = main_class,
        total        = #constructors + #callbacks + #methods + #vars + #classes,
        constructors = constructors,
        callbacks    = callbacks,
        methods      = methods,
        vars         = vars,
        classes      = classes,
    }
end
mt.parse = parse

mt.__call = function(self, file)
    if(file) then
        return self:parse(file)
    end
end
local mt = setmetatable( {}, mt )

module(...)

return mt
