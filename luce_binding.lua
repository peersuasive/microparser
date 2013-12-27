#!/usr/bin/env luajit

--[[----------------------------------------------------------------------------

bind_class.lua

Bind a JUCE class to LUCE

    @alias meta

    @author Christophe Berbizier (cberbizier@peersuasive.com)
    @license GPLv3
    @copyright 

(c) 2013, Peersuasive Technologies

------------------------------------------------------------------------------]]

package.path = "microparser/?.lua;"..package.path

--
-- prepare
--
local mp = require"microparser"

local code = mp:parse( assert(arg and arg[1], "Missing input class") )
assert( code, "Parsing failed" )

local lclass = arg[2] or "L"..code.class.name
local do_inherit = (not arg[3] and true) or (arg[3]=="false")
local no_file = arg[4] and (arg[4]=="true")

local class, sep
if ( do_inherit ) then
    class = code.class.name
    sep = "::"
else
    class = "child"
    sep = "->"
end

local methods, callbacks = code.methods, {}
-- some setters are captured as callbacks
for _,c in next, code.callbacks do
    if c.callback.name:match("^set") or c.callback.name:match("^add") then
        methods[#methods+1] = {
            rule = "method",
            visibility = c.visibility,
            method = c.callback
        }
    else
        callbacks[#callbacks+1] = c
    end
end

--
-- utils
--

local h = "h"
local cpp = "cpp"
local default_output = nil

local function testFile(f)
    local i = io.open(f, "r")
    local is = i and true
    if i then i:close() end
    if ( is ) then
        error("File '"..f.."' already exists -- won't overwrite")
    end
end

local function setO( o )
    if no_file then return end
    if ( default_output ) then
        error("Output file already open", 2)
    end
    if ( o == cpp ) then
        testFile(lclass..".cpp")
        default_output = io.open( lclass..".cpp", "wb" )
    elseif ( o == h ) then
        testFile(lclass..".h" )
        default_output = io.open( lclass..".h", "wb" )
    else
        error("Unknown output: "..(o or "<nil>"), 2)
    end
end

local function unsetO()
    if no_file then return end
    if ( default_output ) then
        default_output:close()
    end
    default_output = nil
end

local function write(f, o)
    local o = o or default_output
    if ( o ) then
        o:write( f )
    else
        io.write(f) 
    end
end

local tab_ = 0
local itab = ""
local tab = function()
    tab_ = tab_+4
    itab = string.rep(" ", tab_)
end
local dtab = function()
    tab_ = tab_ - 4
    tab_ = (tab_ < 0) and 0 or tab_
    itab = string.rep(" ", tab_)
end

local function format(f, ...)
    if ( f ) then write(string.format(itab..f, ...)) end
end

local function nformat(f, ...)
    if ( f ) then format(f.."\n", ...) end
end

local function iformat(f, ...)
    format(f, ...)
    tab()
end
local function dformat(f, ...)
    dtab()
    format(f, ...)
end
local function nl()
    format("\n")
end

local types = {
    String = "string",
    char   = "string",
    ["std::string"] = "string",
    string = "string",

    float  = "number",
    int    = "number",
    double = "number",
    number = "number",

    bool   = "boolean",

    ["std::map"] = "table",
    map    = "table",
    ["std::list"] = "table",
    list   = "table",
    Array  = "table",
    HashMap = "table",
    array  = "table",

    object = "object",
}

local function getType(c)
    return types[c] or "object"
end
local function getReturnMethod(c)
    local t = types[c] or "object"
    if ( t == "number" ) then
        return "LUA::returnNumber"
    elseif ( t == "string" ) then
        return "LUA::returnString"
    elseif ( t == "boolean" ) then
        return "LUA::returnBoolean"
    end
    --- do something with udata
    return "LUA::TODO_RETURN_OBJECT_"..c
end

local function getGetMethod(c, opt, n)
    local t = types[c] or "object"

    local pre = "LUA::"..(opt and "checkAndGet%s(" or "get%s(")
                .. (n and n or "") 
                .. (opt and ", "..opt or "") 
                ..")"

    if ( t == "number" ) then
        return pre:format("Number")
    elseif ( t == "string" ) then
        return pre:format("String")
    elseif ( t == "boolean" ) then
        return pre:format("Boolean")
    elseif ( t == "table" ) then
        return "LUA::getList(" .. (n and n or "") .. ")"
    end
    --- do something with udata
    return "LUA::TODO_OBJECT_"..c
end

local prefix = function()
    if not do_inherit then
        local o_itab = itab
        tab()
        return string.format("%sif (child) {", o_itab)
    end
end
local postfix = function()
    if not do_inherit then
        dtab()
        return string.format("%s} else return 0;", itab)
    end
end

local postfix_s = function(i)
    local xitab = i
    if not do_inherit then
        dtab()
        return string.format("%s}\n%sreturn 0;", itab, xitab)
    end
end

local function prefix_c()
    if not do_inherit then
        local o_itab = itab
        tab()
        return string.format("%sif (child)", o_itab)
    end
end

local postfix_c = function()
    if not do_inherit then
        dtab()
    end
end


local lua_call = "int %s::%s ( lua_State* ) {"
local camel = function(s) return s:gsub("(%a)([%w_]*)", function(a,b) return a:lower()..b end) end

local cc = [[
/************************************************************

 %s.%s

    @author Christophe Berbizier (cberbizier@peersuasive.com)
    @license GPLv3
    @copyright 


(c) %d, Peersuasive Technologies

*************************************************************/
]]

--
-- main --
--

local lcode = {}
local lmethods = {}
local lgetsetters = {}
local llgetsetters = {getters = {}, setters = {}}

local lgetters = {}
local lsetters = {}
local lcallbacks = {}

local function build_arg(a, i, do_simple)
    --local i = i or -1
    local rt = a.return_type
    local rt_ = a.name or rt:lower().."_"
    local value = a.value and a.value.vvalue
    local p = a.pointers and (a.pointers[1]:match("*") and table.concat(a.pointers,""))
    local tc = nil
    if ( a.template_call ) then
        tc = {}
        for _, t in next, a.template_call do
            tc[#tc+1] = t.type
        end
        tc = "<"..table.concat(tc, ",") .. ">"
    end

    local luce_method = getGetMethod( a.return_type, value, i )
    local todo = luce_method:match("TODO") and "// " or ""
    p = p or ""
    tc = tc or ""
    if do_simple then
        return string.format( "%s", luce_method ), (todo ~= "") and true
    else
        return string.format( "%s%s%s%s%s %s = %s;", itab, todo, rt, tc, p, rt_, luce_method ), 
               rt_, (todo ~= "") and true
    end
end

-- prepare methods
for k, v in next, methods do
    --if ( v.visibility == "public" or ( do_inherit and v.visibility == "protected") ) then
    if ( v.visibility ~= "private" ) then
    -- setters
    if ( v.method.return_type == "void" ) then
        local todo = false
        local body = {}
        body[#body+1] = prefix()
        local args = ""
        if ( #v.method.args>1) then
            args = {}
            -- pass as parameter if only one arg required
            for i,a in next, v.method.args do
                local arg, rt, todo_ = build_arg(a, i+1)
                todo = todo or todo_
                body[#body+1] = arg
                args[#args+1] = rt
            end
            args = " "..table.concat(args, ", ").." "
        elseif( #v.method.args == 1 ) then
            args, todo = build_arg( v.method.args[1], _, true )
        end

        body[#body+1] = string.format( "%s%s%s%s%s(%s);", itab, todo and "// " or "", class, sep, v.method.name, args )
        if ( todo ) then
            body[#body+1] = string.format( '%s%s( "%s, %s" );', itab, "LUA::TODO_OBJECT", v.method.name, args )
            body[#body+1] = string.format( "%slua_settop(LUA::Get(), 1); // added by TODO", itab )
        end
        body[#body+1] = postfix_s(itab) or string.format("%sreturn 0;",itab)

        local real_name = v.method.name
        local name = real_name:gsub("^set","") -- :gsub("^add","")
        if ( lsetters[name] ) then
            table.insert(body, 1, "// override")
            if ("table" == type(lsetters[name].body[1]) ) then
                lsetters[name].body[#lsetters[name].body+1] = body
            else
                table.insert( lsetters[name].body, 1, "// override" )
                lsetters[name].body = { lsetters[name].body, body, }
            end
        else
            lsetters[name] = { name = real_name, body = body }
        end

    -- getters
    else
        local luce_method = getReturnMethod( v.method.return_type )
        local todo = luce_method:match("TODO") and true
        local body = {}
        body[#body+1] = prefix()
        local args = ""
        if ( #v.method.args > 0 ) then
            args = {}
            for i,a in next, v.method.args do
                local arg, rt, todo_ = build_arg(a, i)
                todo = todo or todo_
                body[#body+1] = arg
                args[#args+1] = rt
            end
            args = " "..table.concat(args, ", ").." "
        end
        local meth = string.format("%s%s%s", class, sep, v.method.name)
        body[#body+1] = string.format( "%s%sreturn %s( %s(%s) );", itab, todo and "// " or "", luce_method, meth, args )
        if ( todo ) then
            body[#body+1] = string.format( "%slua_settop(LUA::Get(), 1); // added by TODO", itab )
            body[#body+1] = string.format( '%sreturn %s( "%s %s(%s)" );', 
                                itab, "LUA::TODO_OBJECT", v.method.return_type, v.method.name, args )
            -- body[#body+1] = string.format( "%sreturn 0; // added by TODO", itab )
        end
        body[#body+1] = postfix()

        local real_name = v.method.name
        local name = real_name:gsub("^get",""):gsub("^is","")
        if ( lgetters[name] ) then
            table.insert(body, 1, "// override")
            if ("table" == type(lgetters[name].body[1]) ) then
                lgetters[name].body[#lgetters[name].body+1] = body
            else
                table.insert(lgetters[name].body, 1, "// override")
                lgetters[name].body = { lgetters[name].body, body }
            end
        else
            lgetters[name] = { name = real_name, body = body }
        end
    end

    end -- end visibility check
end

-- try to keep getters/setters together
for k, m in next, lgetters do
    if ( lsetters[k] ) then
        lgetsetters[#lgetsetters+1] = 
            string.format( [[{"%s", &%s::%s, &%s::%s}]], camel(k), lclass, m.name, lclass, lsetters[k].name )
        lmethods[#lmethods+1] = string.format("method( %s, %s )", lclass, m.name)
        lmethods[#lmethods+1] = string.format("method( %s, %s )", lclass, lsetters[k].name)
        llgetsetters.setters[#llgetsetters.setters+1] = lsetters[k]
        llgetsetters.getters[#llgetsetters.getters+1] = lgetters[k]
        lsetters[k] = nil
        lgetters[k] = nil
    else
        lmethods[#lmethods+1] = string.format("method( %s, %s )", lclass, m.name)
    end
end
for k, m in next, lsetters do
    lmethods[#lmethods+1] = string.format("method( %s, %s )", lclass, m.name)
end

-- callbacks
for _, c in next, callbacks do
    --if ( c.visibility == "public" or ( do_inherit and c.visibility == "protected") ) then
    if ( c.visibility ~= "private" ) then
        local name = c.callback.name
        local args = ""
        if ( #c.callback.args > 0 ) then
            args = {}
            for _, a in next, c.callback.args do
                local rtype = a.return_type
                local vname = a.name and a.name or camel(rtype).."_"
                local p = a.pointers and table.concat(a.pointers,"") or ""
                local tc = ""
                if ( a.template_call ) then
                    tc = {}
                    for _,t in next, a.template_call do
                        t[#t+1] = t.type
                    end
                    tc = "<" .. table.concat(t, ", ") .. ">"
                end
                local value = a.value and " = "..a.value.vvalue or ""
                local const = a.is_const and "const " or ""
                args[#args+1] = string.format("%s%s%s%s%s %s%s", itab, const, rtype, tc, p, vname, value)
            end
            args = " "..table.concat(args, ",").." "
        end
        -- TODO
        local body = {}
        local lname = do_inherit and name or "l"..name
        local proto = string.format("%s(%s)", lname, args)
        --body[#body+1] = string.format("%s%s {", itab, proto)
        body[#body+1] = string.format("%svoid %s::%s {", itab, lclass, proto)
        tab()
        body[#body+1] = prefix_c()
        body[#body+1] = string.format('%scallback("%s");', itab, name)
        body[#body+1] = postfix_c()
        dtab()
        body[#body+1] = "}"

        body[#body+1] = string.format("%sint %s::%s(lua_State*){", itab, lclass, name)
        tab()
        body[#body+1] = prefix_c()
        body[#body+1] = string.format('%sset("%s");', itab, name)
        body[#body+1] = postfix_c()
        body[#body+1] = string.format("%sreturn 0;", itab)
        dtab()
        body[#body+1] = "}"

        lcallbacks[name] = {
            body = body,
            proto = proto
        }
        lmethods[#lmethods+1] = string.format("method( %s, %s )", lclass, name)
    end
end

---
-- cpp
---

setO(cpp)

nformat(cc, lclass, "cpp", 2013)

nformat('#include "%s_inh.h"', lclass)
nl()

nformat("////// static methods")
nformat('const char %s::className[] = "%s";', lclass, lclass)

-- TODO: if inherited, add LComponent inheritence too, in a separate _inh.h header,
--       plus inherited lcallbacks

nformat("const Luna<%s>::PropertyType %s::properties[] = {", lclass, lclass )
    tab()
    for _, l in next, lgetsetters do
        nformat("%s,", l)
    end
    nformat("%s", "{0,0}")
dformat("};") nl()

nformat("const Luna<%s>::FunctionType %s::methods[] = {", lclass, lclass )
    tab()
    for _,l in next, lmethods do
        nformat("%s,", l)
    end
    nformat("%s", "{0,0}")
dformat("};") nl()
nl()


-- ctor/dtor
nformat("/////// ctor/dtor")
if ( do_inherit ) then
    nformat("%s::%s(lua_State *L)", lclass, lclass)
    nformat("    : LComponent(L, this),")
    nformat("      %s( /* TODO: add args */ )", class)

    iformat("{") nl()

    nformat("%s::setName(myName);", lclass)
    nformat("//%s::addListener(this);", lclass)
    for k, _ in next, lcallbacks or {} do
        nformat('reg("%s");', k)
    end
    dformat("}") nl()
else
    nformat("%s::%s(lua_State *Ls, Component* child_, const String& name_)", lclass, lclass)
    nformat("    : child(child_)")
    iformat("{")
    nl()
    nformat("LUA::Set(Ls);")
    nformat("L = Ls;")
    nl()
    iformat("if ( lua_isstring(L, 2) )") nl()
    nformat("myName = lua_tostring(L, 2);")
    dtab()
    iformat("else") nl()
    nformat("myName = name_;")
    dtab()
    -- add callbacks
    nl()
    for k,_ in next, lcallbacks or {} do
        nformat( 'reg("%s");', k )
    end

    nl()
    dformat("}") nl()
end
nformat("%s::~%s(){}", lclass, lclass)

nl()
format("/////// callbacks")
for k, c in next, lcallbacks do
    nl()
    for _, l in next, c.body do
        nformat(l)
    end
end

nl()
format("/////// getters/setters")
for k, m in next, llgetsetters.getters do
    nl()
    iformat( lua_call, lclass, m.name ) nl()
    for _, l in next, m.body do
        if ("table"==type(l)) then
            nl()
            for _, ll in next, l do
                nformat(ll)
            end
        else
            nformat(l)
        end
    end
    dformat( "}" ) nl()

    iformat( lua_call, lclass, llgetsetters.setters[k].name ) nl()
    for _, l in next, llgetsetters.setters[k].body do
        if ("table"==type(l)) then
            nl()
            for _, ll in next, l do
                nformat(ll)
            end
        else
            nformat(l)
        end
    end
    dformat( "}" ) nl()
end

for i, t in next, { lgetters, lsetters } do
    if ( i == 1 ) then
        nl() format("/////// getters")
    else
        nl() format("/////// setters")
    end
    for k, m in next, t do
        nl()
    iformat( lua_call, lclass, m.name ) nl()
        for _, l in next, m.body do
            if ("table"==type(l)) then
                nl()
                for _, ll in next, l do
                    nformat(ll)
                end
            else
                nformat(l)
            end
        end
    dformat( "}" ) nl()
    lmethods[#lmethods+1] = string.format("method( %s, %s )", lclass, m.name)
    end
end

nl()
unsetO()

---
-- header
---

setO(h)
nformat(cc, lclass, "h", 2013)
nformat("#ifndef __LUCE_%s_H", lclass:upper())
nformat("#define __LUCE_%s_H", lclass:upper())
nl()
if ( do_inherit ) then
    nformat("class %s", lclass)
    nformat("    : public LComponent,")
    format("      public %s", class)

    if ( #callbacks > 0 ) then
        nformat("//, private %s::Listener", class)
    else
        nl()
    end
else
    nformat("class %s", lclass)
                
end
nformat("{")
iformat("public:") nl()
    if ( do_inherit ) then
        nformat("%s(lua_State*);", lclass)
    else
        nformat("%s(lua_State*, Component* child = nullptr, const String& name = String::empty);", lclass )
    end

    nformat("~%s();", lclass)
    nl()

    -- getters/setters
    nformat("//==============================================================================")
    for k, m in next, llgetsetters.setters do
        nformat( "int %s(lua_State*);", m.name )
        nformat( "int %s(lua_State*);", llgetsetters.getters[k].name )
    end
    nl()

    -- setters
    nformat("//==============================================================================")
    for k, m in next, lsetters do
        nformat( "int %s(lua_State*);", m.name )
    end
    nl()

    -- getters
    nformat("//==============================================================================")
    for k, m in next, lgetters do
        nformat( "int %s(lua_State*);", m.name )
    end
    nl()

    -- callbacks
    nformat("//==============================================================================")
    for k,_ in next, lcallbacks do
        nformat( "int %s(lua_State*);", k )
    end
    nl()

    nformat("static const char className[];")
    nformat("static const Luna<%s>::Inheritence inherits[];", lclass)
    nformat("static const Luna<%s>::InheritenceF inheritsF[];", lclass)
    nformat("static const Luna<%s>::PropertyType properties[];", lclass)
    nformat("static const Luna<%s>::FunctionType methods[];", lclass)
    nl()

if not ( do_inherit ) then
dtab()
iformat("protected:") nl()
    nformat("//==============================================================================")
    nformat("String myName;")
    nl()

    nformat("//==============================================================================")
    for k,v in next, lcallbacks do
        local name = "l"..k
        nformat( "void %s;", v.proto )
    end
    nl()
end

dtab()
iformat("private:") nl()
    -- put overrided callbacks here
    if not ( do_inherit ) then
        nformat("//==============================================================================")
        nformat("ScopedPointer<Component> child;")

        nl()
        nformat("//==============================================================================")
        nformat("lua_State *L;")

        nl()
        nformat("//==============================================================================")
        nformat("HashMap<String,int> cb;")
    else
        nl()
        nformat("//==============================================================================")
        nformat("// callbacks")
        for k,v in next, lcallbacks do
            nformat( "virtual void %s override;", v.proto )
        end
        nl()
    end

    nformat("//==============================================================================")
    nformat("JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (%s)", lclass)

dformat("};") nl() -- end class
nl()
nformat("#endif // __LUCE_%s_H", lclass:upper())
unsetO()
