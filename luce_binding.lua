#!/usr/bin/env luajit

--[[----------------------------------------------------------------------------

bind_class.lua

Bind a JUCE class to LUCE

    @alias meta

    @author Christophe Berbizier (cberbizier@peersuasive.com)
    @license GPLv3
    @copyright 

(c) 2013-2014, Peersuasive Technologies

------------------------------------------------------------------------------]]

package.path = "microparser/?.lua;"..package.path

--
-- prepare
--
local mp = require"microparser"

local code = mp:parse( assert(arg and arg[1], "Missing input class") )
assert( code, "Parsing failed" )

local lclass = arg[2] or "L"..code.class.name
if (""==lclass) then lclass="L"..code.class.name end
local do_inherit = (not arg[3] and true) or (arg[3]~="false")
local do_core = false
if (do_inherit == "core") then
    do_core = true
    do_inherit = false
end
local do_base    = arg[4]=="true" or false
local no_file = arg[5] and (arg[5]=="true")

local class, sep
if ( do_inherit ) then
    class = code.class.name
    sep = "::"
else
    class = "child"
    sep = "->"
end

local methods, callbacks, statics = code.methods, {}, {}
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

for _,s in next, code.statics do
    s.static.sname = s.static.name
    s.static.name = "s_"..s.static.name
    statics[#statics+1] = s
end

-- LComponent methods
--
local function Set(t, k)
    t[k] = true
end
lcomponent = {}
if ( do_inherit and not(do_base) ) then
    local inh = io.open("LComponent_inh.list") or io.open("../LComponent_inh.list")
    if(inh)then
        while (true) do
            local f = inh:read("*l")
            if not f then break end
            Set( lcomponent, f)
        end
        inh:close()
    else
        print("WARNING: missing LComponent's list of functions")
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
    write(string.format("\n"))
end

local types = {
    String = "string",
    char   = "string",
    ["std::string"] = "string",
    string = "string",

    number = "number",
    float  = "numberT",
    int    = "numberT",
    double = "numberT",
    int8   = "numberT",
    uint8  = "numberT",
    int16  = "numberT",
    uint16 = "numberT",
    int32  = "numberT",
    uint32 = "numberT",
    int64  = "numberT",
    uint64 = "numberT",

    bool   = "boolean",

    StringRef = "LUA::getString(2)",

    ["std::map"] = "table",
    map    = "table",
    ["std::list"] = "table",
    list   = "table",
    Array  = "table",
    HashMap = "table",
    array  = "table",
    Range     = "LUA::getRange(2)",
    SparseSet = "LUA::getSparseSet(2)",

    object = "object",
}

if not(do_core)then
    types.Rectangle = "LUA::getRectangle(2)"
    types.Point     = "LUA::getPoint(2)"
    types.Justification = "(Justification)LUA::getNumber(2)"
end

local return_types = {
    String = "string",
    char   = "string",
    ["std::string"] = "string",
    string = "string",

    number = "number",
    float  = "number",
    int    = "number",
    double = "number",
    int8   = "number",
    uint8  = "number",
    int16  = "number",
    uint16 = "number",
    int32  = "number",
    uint32 = "number",
    int64  = "number",
    uint64 = "number",

    bool   = "boolean",

    ["std::map"] = "table",
    map    = "table",
    ["std::list"] = "table",
    list   = "table",
    Array  = "table",
    HashMap = "table",
    array  = "table",

    Range     = "LUA::returnTable",
    SparseSet = "LUA::returnTable",

    void      = "void",
    object = "object",
}
if not(do_core)then
    return_types.Rectangle = "LUA::returnTable"
    return_types.StringRef = "LUA::returnTable"
    return_types.Rectangle = "LUA::returnTable"
    return_types.RectangleList = "LUA::returnTable"
    return_types.Point         = "LUA::returnTable"
    return_types.Justification = "LUA::returnNumber"
end

local numbers = {
    float  = true,
    int    = true,
    double = true,
    number = true,
    int8   = true,
    uint8  = true,
    int16  = true,
    uint16 = true,
    int32  = true,
    uint32 = true,
    int64  = true,
    uint64 = true,
}
local function getType(c)
    return types[c] or "object"
end
local function getReturnMethod(c)
    local t = return_types[c] or "object"
    if ( t == "number" or numbers[c] ) then
        return "LUA::returnNumber"
    elseif ( t == "string" ) then
        return "LUA::returnString"
    elseif ( t == "boolean" ) then
        return "LUA::returnBoolean"
    end

    if ( t == "void" ) then
        return "DIRECT",
               "return 0;"
    end
    if ( t ~= "object" ) then
        return t
    end

    --- do something with udata
    --return "LUA::TODO_RETURN_OBJECT_"..c
    if ( lclass == "L"..c ) then
        return "CHECK", 
        string.format(
            [[return LUA::storeAndReturnUserdata<%s>( new %s(L,]],
            lclass, lclass
        ), 
        "));",
        true
    else
        return "CHECK TODO", 
        string.format(
            [[return LUA::storeAndReturnUserdata<%s>( new %s(L,]],
            "L"..c, "L"..c
        ), 
        "));",
        true

        --return "LUA::TODO_RETURN_OBJECT_"..c
    end
end

local function getGetMethod(c, opt, n, p)
    local t = types[c] or "object"

    local pre = "LUA::"..(opt and "checkAndGet%s(" or "get%s(")
                .. (n and n or "") 
                .. (opt and ", "..opt or "") 
                ..")"

    if ( t == "number" ) then
        return pre:format("Number")
    elseif ( t == "numberT" ) then
        return pre:format("Number<"..c..">")
    elseif ( t == "string" ) then
        return pre:format("String")
    elseif ( t == "boolean" ) then
        return pre:format("Boolean")
    elseif ( t == "table" ) then
        return "LUA::getList(" .. (n and n or "") .. ")"
    end

    if ( t ~= "object" ) then
        if(p)then
            return string.format("new %s(%s)", c, t)
        else
            return string.format("%s", t), true
        end
    end

    --- do something with udata
    --return "LUA::TODO_OBJECT_"..c
    if ( lclass == "L"..c) then
        return string.format("%sLUA::from_luce<L%s>(2); // CHECK", not(p) and "*" or "", c)
    else
        return string.format("%sLUA::from_luce<L%s>(2); // TODO", not(p) and "*" or "", c)
    end
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
local lua_call_full = "int %s::%s ( lua_State *L ) {"
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
local lsmethods = {}
local lgetsetters = {}
local llgetsetters = {getters = {}, setters = {}}

local lgetters = {}
local lsetters = {}
local lcallbacks = {}
local lstatics = {}

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

    local luce_method, is_ref = getGetMethod( a.return_type, value, i, p )
    local todo = luce_method:match("TODO") and "// " or ""
    local check = luce_method:match("CHECK")
    p = p or ""
    tc = tc or ""
    if do_simple then
        return string.format( "%s", luce_method ), (todo ~= "") and true
    else
        if ( is_ref ) then
            return string.format( "%s%s%s%s%s %s ( %s );", itab, todo, rt, tc, p, rt_, luce_method ), 
                   rt_, (todo ~= "") and true

        else
            return string.format( "%s%s%s%s%s %s = %s;", itab, todo, rt, tc, p, rt_, luce_method ), 
                   rt_, (todo ~= "") and true
        end
    end
end

-- prepare methods
for k, v in next, methods do
    --if ( v.visibility == "public" or ( do_inherit and v.visibility == "protected") ) then
    if ( v.visibility ~= "private" ) then
    if not( lcomponent[v.method.name] ) then
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
                local arg, rt, todo_ = build_arg(a, 2)
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
        local luce_method, formatted, append, is_full = getReturnMethod( v.method.return_type )
        local todo = luce_method:match("TODO") and true
        local check = luce_method:match("CHECK") and true
        local direct = luce_method:match("DIRECT") and true
        local body = {}
        body[#body+1] = prefix()
        local args = ""
        if ( #v.method.args > 0 ) then
            args = {}
            for i,a in next, v.method.args do
                local arg, rt, todo_ = build_arg(a, 2)
                todo = todo or todo_
                body[#body+1] = arg
                args[#args+1] = rt
            end
            args = " "..table.concat(args, ", ").." "
        end
        if(check)then
            local comm = todo and "// " or ""
            body[#body+1] = string.format("%s// CHECK", itab)
            local meth = string.format("%s%s%s", class, sep, v.method.name)
            body[#body+1] = string.format("%s%s%s", itab, comm, formatted)
            body[#body+1] = string.format("%s%s    %s(%s)", itab, comm, meth, args)
            body[#body+1] = string.format("%s%s%s", itab, comm, append)

        elseif(direct)then
            local comm = todo and "// " or ""
            local meth = string.format("%s%s%s", class, sep, v.method.name)
            body[#body+1] = string.format("%s%s%s(%s);", itab, comm, meth, args)
            body[#body+1] = string.format("%s%s%s", itab, comm, formatted)

        else
            local meth = string.format("%s%s%s", class, sep, v.method.name)
            body[#body+1] = string.format( "%s%sreturn %s( %s(%s) );", itab, todo and "// " or "", luce_method, meth, args )
        end
        if ( todo ) then
            body[#body+1] = string.format( "%slua_settop(LUA::Get(), 1); // added by TODO", itab )
            body[#body+1] = string.format( '%sreturn %s( "%s %s(%s)" );', 
                                itab, "LUA::TODO_OBJECT", v.method.return_type, v.method.name, args )
            -- body[#body+1] = string.format( "%sreturn 0; // added by TODO", itab )
        end
        body[#body+1] = postfix()

        local real_name = v.method.name
        local name = real_name:gsub("^get",""):gsub("^is",""):gsub("^are","")
        if ( lgetters[name] ) then
            table.insert(body, 1, "// override")
            if ("table" == type(lgetters[name].body[1]) ) then
                lgetters[name].body[#lgetters[name].body+1] = body
            else
                table.insert(lgetters[name].body, 1, "// override")
                lgetters[name].body = { lgetters[name].body, body }
            end
        else
            lgetters[name] = { name = real_name, body = body, full = is_full }
        end
    end
    
    end -- end belongs to LComponent
    end -- end visibility check
end

local has_statics = false
for k, v in next, statics do
    if (v.visibility ~= "private") then
        has_statics = true
        
        local luce_method, formatted, append, is_full = getReturnMethod( v.static.return_type )
        local todo = luce_method:match("TODO") and true
        local check = luce_method:match("CHECK") and true
        local direct = luce_method:match("DIRECT") and true
        local body = {}
        body[#body+1] = prefix()
        local args = ""
        if ( #v.static.args > 0 ) then
            args = {}
            for i,a in next, v.static.args do
                local arg, rt, todo_ = build_arg(a, 2)
                todo = todo or todo_
                body[#body+1] = arg
                args[#args+1] = rt
            end
            args = " "..table.concat(args, ", ").." "
        end
        if(check)then
            local comm = todo and "// " or ""
            body[#body+1] = string.format("%s// CHECK", itab)
            local meth = string.format("%s%s%s", class, sep, v.static.sname)
            body[#body+1] = string.format("%s%s%s", itab, comm, formatted)
            body[#body+1] = string.format("%s%s    %s(%s)", itab, comm, meth, args)
            body[#body+1] = string.format("%s%s%s", itab, comm, append)

        elseif(direct)then
            local comm = todo and "// " or ""
            local meth = string.format("%s%s%s", class, sep, v.static.sname)
            body[#body+1] = string.format("%s%s%s(%s);", itab, comm, meth, args)
            body[#body+1] = string.format("%s%s%s", itab, comm, formatted)

        else
            local meth = string.format("%s%s%s", class, sep, v.static.sname)
            body[#body+1] = string.format( "%s%sreturn %s( %s(%s) );", itab, todo and "// " or "", luce_method, meth, args )
        end
        if ( todo ) then
            body[#body+1] = string.format( "%slua_settop(LUA::Get(), 1); // added by TODO", itab )
            body[#body+1] = string.format( '%sreturn %s( "%s %s(%s)" );', 
                                itab, "LUA::TODO_OBJECT", v.static.return_type, v.static.name, args )
            -- body[#body+1] = string.format( "%sreturn 0; // added by TODO", itab )
        end
        body[#body+1] = postfix()

        local name = v.static.name
        local sname = v.static.sname
        if ( lstatics[name] ) then
            table.insert(body, 1, "// override")
            if ("table" == type(lstatics[name].body[1]) ) then
                lstatics[name].body[#lstatics[name].body+1] = body
            else
                table.insert(lstatics[name].body, 1, "// override")
                lstatics[name].body = { lstatics[name].body, body }
            end
        else
            lstatics[name] = { name = name, sname = sname, body = body, full = is_full }
        end

    end
end

-- try to keep getters/setters together
local has_getsetters = false
for k, m in next, lgetters do
    if ( lsetters[k] ) then
        has_getsetters = true
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
local has_setters = false
for k, m in next, lsetters do
    has_setters = true
    lmethods[#lmethods+1] = string.format("method( %s, %s )", lclass, m.name)
end

for k, m in next, lstatics do
    lsmethods[#lsmethods+1] = string.format("smethod( %s, %s )", lclass, m.sname)
end

-- callbacks
local has_callbacks = false
for _, c in next, callbacks do
    --if ( c.visibility == "public" or ( do_inherit and c.visibility == "protected") ) then
    if ( c.visibility ~= "private" ) then
        has_callbacks = true
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
                        tc[#tc+1] = t.type
                    end
                    tc = "<" .. table.concat(tc, ", ") .. ">"
                end
                local value = a.value and " = "..a.value.vvalue or ""
                local const = a.is_const and "const " or ""
                args[#args+1] = string.format("%s%s%s%s%s %s%s", itab, const, rtype, tc, p, vname, value)
            end
            args = " "..table.concat(args, ", ").." "
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

nformat(cc, lclass, "cpp", 2014)

nformat('#include "%s_inh.h"', lclass)
nl()

--nformat("////// static methods")
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

nformat("const Luna<%s>::StaticType %s::statics[] = {", lclass, lclass )
    tab()
    for _,l in next, lsmethods do
        nformat("%s,", l)
    end
    nformat("%s", "{0,0}")
dformat("};") nl()
nl()

-- ctor/dtor
--nformat("/////// ctor/dtor")
if ( do_inherit ) then
    if ( do_base ) then
        nformat("%s::%s(lua_State *L)", lclass, lclass)
        nformat('    : LBase(L, "'..lclass..'", true),')
        nformat("      %s( /* TODO: add args */ )", class)
        iformat("{") nl()
        --iformat("if ( lua_isstring(L, 2) )") nl()
        --nformat("myName( LUA::getString(L, 2) );")
        dtab()
        --nl()
        --nformat("REGISTER_CLASS(%s);", lclass)
        dformat("}") nl() 
        nl()
        nformat("%s::%s(lua_State *L, const %s& class_)", lclass, lclass, class)
        nformat('    : LBase(L, "'..lclass..'", true),')
        nformat("      %s( class_ )", class)
        iformat("{") nl()
        --iformat("if ( lua_isstring(L, 2) )") nl()
        --nformat("myName( LUA::getString(L, 2) );")
        dtab()
        --nl()
        --nformat("REGISTER_CLASS(%s);", lclass)
        dformat("}") nl()

    else
        nformat("%s::%s(lua_State *L)", lclass, lclass)
        nformat("    : LComponent(L, this),")
        nformat("      %s( /* TODO: add args */ )", class)

        iformat("{") nl()

        nformat("%s::setName(myName());", class)
        nformat("//%s::addListener(this);", class)

        nl()
        nformat("REGISTER_CLASS(%s);", lclass)

        dformat("}") nl()
    end
else
    nformat("%s::%s(lua_State *Ls, Component* child_, const String& name_)", lclass, lclass)
    nformat('    : LBase(L, "'..lclass..'", true),')
    nformat("      child(child_)")
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

nl()
nformat("%s::~%s() {}", lclass, lclass)

nl()
if(has_callbacks)then
format("/////// callbacks")
for k, c in next, lcallbacks do
    nl()
    for _, l in next, c.body do
        nformat(l)
    end
end
end

if(do_inherit and not(do_base))then
    for _,e in next, { "mouseMove", "mouseEnter", "mouseExit", 
                       "mouseDown", "mouseDrag", "mouseUp",
                       "mouseDoubleClick", "mouseWheelMove", "mouseMagnify" } do
        nl()
        nformat("void %s::%s(const MouseEvent& e) {", lclass, e)
        nformat("    LComponent::l%s(e);", e)
        dformat("}")
        nl()
    end
end

nl()
if(has_getsetters)then
format("/////// getters/setters")
for k, m in next, llgetsetters.getters do
    nl()
    if(m.full)then
        iformat( lua_call_full, lclass, m.name ) nl()
    else
        iformat( lua_call, lclass, m.name ) nl()
    end
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
end

for i, t in next, { lstatics, lgetters, lsetters } do
    if ( i == 1 ) then
        if(has_statics)then
            nl() format("/////// statics")
        end
    elseif( i == 2 ) then
        nl() format("/////// getters")
    else
        if(has_setters)then
            nl() format("/////// setters")
        end
    end
    for k, m in next, t do
        nl()
        if(m.full)then
            iformat( lua_call_full, lclass, m.name ) nl()
        else
            iformat( lua_call, lclass, m.name ) nl()
        end
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
        if(i==1)then
            --lstatics[#lstatics+1] = string.format("smethod( %s, %s )", lclass, m.sname)
        else
            lmethods[#lmethods+1] = string.format("method( %s, %s )", lclass, m.name)
        end
    end
end

nl()
unsetO()

---
-- header
---

setO(h)
nformat(cc, lclass, "h", 2014)
nformat("#ifndef __LUCE_%s_H", lclass:upper())
nformat("#define __LUCE_%s_H", lclass:upper())
nl()
if ( do_inherit ) then
    if(do_base)then
        nformat("class %s", lclass)
        nformat("    : public LBase,")
        format("      public %s", class)
    else
        nformat("class %s", lclass)
        nformat("    : public LComponent,")
        format("      public %s", class)
    end

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
        if(do_base) then
            nformat("%s(lua_State*, const %s&);", lclass, class)
        end
    else
        nformat("%s(lua_State*, Component* child = nullptr, const String& name = String::empty);", lclass )
    end

    nformat("~%s();", lclass)
    nl()

    if(has_statics) then
     nformat("//==============================================================================")
    for k, m in next, lstatics do
        nformat( "static int %s(lua_State*);", m.name )
    end
    nl()
    end

    if(has_getsetters)then
    -- getters/setters
    nformat("//==============================================================================")
    for k, m in next, llgetsetters.setters do
        nformat( "int %s(lua_State*);", m.name )
        nformat( "int %s(lua_State*);", llgetsetters.getters[k].name )
    end
    nl()
    end

    -- getters
    nformat("//==============================================================================")
    for k, m in next, lgetters do
        nformat( "int %s(lua_State*);", m.name )
    end
    nl()


    if(has_setters)then
    -- setters
    nformat("//==============================================================================")
    for k, m in next, lsetters do
        nformat( "int %s(lua_State*);", m.name )
    end
    nl()
    end

    if(has_callbacks)then
    -- callbacks
    nformat("//==============================================================================")
    for k,_ in next, lcallbacks do
        nformat( "int %s(lua_State*);", k )
    end
    nl()
    end

    nformat("//==============================================================================")
    nformat("static const char className[];")
    nformat("static const Luna<%s>::Inheritence inherits[];", lclass)
    nformat("static const Luna<%s>::InheritenceF inheritsF[];", lclass)
    nformat("static const Luna<%s>::PropertyType properties[];", lclass)
    nformat("static const Luna<%s>::FunctionType methods[];", lclass)
    nformat("static const Luna<%s>::StaticType statics[];", lclass)
    nformat("static const Luna<%s>::Enum enums[];", lclass)
    nl()

if not ( do_inherit ) then
dtab()
iformat("protected:") nl()
    nformat("//==============================================================================")
    nformat("String myName;")
    nl()

    if(has_callbacks)then
    nformat("//==============================================================================")
    for k,v in next, lcallbacks do
        local name = "l"..k
        nformat( "void %s;", v.proto )
    end
    nl()
    end
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
        if(has_callbacks and not(do_core))then
        nl()
        nformat("//==============================================================================")
        nformat("// callbacks")
        for k,v in next, lcallbacks do
            nformat( "virtual void %s override;", v.proto )
        end
        nl()
        end

        if(do_inherit and not(do_base) and not(do_core))then
            for _,e in next, { "mouseMove", "mouseEnter", "mouseExit", 
                               "mouseDown", "mouseDrag", "mouseUp",
                               "mouseDoubleClick", "mouseWheelMove", "mouseMagnify" } do
                nformat("virtual void %s(const MouseEvent&) override;", e)
            end
        nl()
        end
    end

    nformat("//==============================================================================")
    nformat("JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (%s)", lclass)

dformat("};") nl() -- end class
nl()
nformat("#endif // __LUCE_%s_H", lclass:upper())
unsetO()
