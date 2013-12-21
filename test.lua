#!/usr/bin/env luajit

local data_file = arg[1] or "test.data.h"

local code = assert(data_file, "Missing test data")

-- require"pl"
-- local dump = pretty.dump
-- local write= pretty.write

local mp = require"microparser"
local parsed = mp:parse(code)

if not (parsed) then os.exit(1) end

assert( parsed.class.name == "Component" )
assert( parsed.class.rtype == "class" )

print(string.format("Parsed %d items from %s", parsed.total, parsed.class.name))

print( string.format("\tconstructors: %d", #parsed.constructors ) )
print( string.format("\tcallbacks   : %d", #parsed.callbacks ) )
print( string.format("\tmethods     : %d", #parsed.methods ) )
--print( string.format("\tclasses     : %d", #parsed.classes ) )
--print( string.format("\tvars        : %d", #parsed.vars ) )

assert(  2 == #parsed.constructors , "constructores: expected:  2, found: "..#parsed.constructors )
assert(  9 == #parsed.callbacks    , "callbacks    : expected:  9, found: "..#parsed.callbacks )
assert( 47 == #parsed.methods      , "methods      : expected: 47, found: "..#parsed.methods )
--assert( 7 == #parsed.classes      , "classes      : expected:  7, found: "..#parsed.classes )
--assert( 5 == #parsed.vars         , "vars         : expected:  5, found: "..#parsed.vars )

local function getItem( field, name )
    local r = {}
    for _, c in next, parsed[field] do
        local f = c.rule
        local i = c[f]
        if ( i.name and (i.name == name) ) then
            r[#r+1] = c
        end
    end
    r = (#r>1) and r or r[1]
    assert( r, string.format("Item '%s' not found in table '%s'", name, field) )
    return r
end

local function expect( e, r )
    if ( e ~= r ) then
        print( string.format([[Assertion failed: expected "%s", found "%s"]], e, r) )
        assert(false)
    end
end

local function cT(t)
    local r = {}
    for _, v in next, t do
        r[#r+1] = { type = v }
    end
    return r
end

--[[--
    "return type", 
    "visibility", 
    {"pointer"...}, 
    {[C=T], "type"...}, 
    {
        "type/class", 
        {"pointer"...}, 
        {"default type", "default value"}, 
        {"tmpl type"...}
    }...
--]]
local function c(rtype, pointer, vis, tmpl, ...)
    local a = { ... }
    local t = {
        visibility = vis or "public",
        return_type = rtype or "void",
        pointer = pointer,
    }
    if ( tmpl ) then
        if ( tmpl.T ) then
            tmpl.T = nil
            t.template = tmpl
        else
            t.template_call = cT(tmpl)
        end
    end

    local args = {}
    if ( #a > 0 ) then
        for i, v in next, a do
            local n = {
                return_type = v[1],
                pointers = v[2],
                value = v[3] and {
                    vtype = v[3][1],
                    vvalue = v[3][2]
                },
                template_call = v[4] and cT(v[4])
            }
            args[#args+1] = n
        end
    end
    t.args = args
    return t
end
print""
print"********** callbacks"
print""

local cb = {
    visibilityChanged       = c(),
    userTriedToCloseWindow  = c(), 
    childrenChanged         = c(),
    lookAndFeelChanged      = c(),
    mouseDrag               = c("void", nil, "public", nil, { "MouseEvent", {"&"} } ),
    addToDesktop            = c("void", nil, "public", nil, {"int"}, {"void", {"*"}, {"object", "nullptr"}}),
    minimisationStateChanged = c("void", nil, "public", nil, {"bool"}),
    setVisible              = c("void", nil, "public", nil, {"bool"}),
    setName                 = c("void", nil, "public", nil, {"String", {"&"}}),
}

local total = 0
for k, v in next, cb do
    print(" "..k.."...")
    local c = getItem( "callbacks", k )
    print("  ..found")
    assert( c.rule and (c.rule == "callback")                            , "Not a callback" )
    assert( c.visibility                                                 , "Missing visibility field")
    assert( c.callback and ("table"==type(c.callback))                   , "Missing callback field" )
    assert( c.callback.args and ("table"==type(c.callback.args))         , "Missing args field" )
    assert( c.callback.return_type and (c.callback.return_type == "void"), "Wrong return type" )
    assert( c.callback.name                                              , "Missing callback name" )
    expect( v.visibility, c.visibility )
    print("  ..structure")
    local c = c.callback
    expect( #v.args, #c.args )
    if ( #v.args > 0 ) then
        for i, a in next, v.args do
            expect( a.return_type, c.args[i].return_type )
            if ( a.value ) then
                expect( a.value.vtype, c.args[i].value.vtype )
                expect( a.value.vvalue, c.args[i].value.vvalue )
            end
            if ( a.pointers ) then
                expect( #a.pointers, #c.args[i].pointers )
                for j, p in next, a.pointers do
                    expect( p, c.args[i].pointers[j] )
                end
            end
        end
    end
    total = total + 1
    print("  ..OK")
end

expect( #parsed.callbacks, total )

print""
print"********** methods"
print""
local methods = {
    isVisible           = c( "bool" ),
    isShowing           = c( "bool" ),
    removeFromDesktop   = c( "void" ),
    isOnDesktop         = c( "bool" ),
    getDesktopScaleFactor = c( "float" ),
    toBack              = c( "void" ),
    isAlwaysOnTop       = c( "bool" ),
    getRight            = c( "int" ),
    getScreenY          = c( "int" ),
    repaint             = c( "void" ),
    toFront             = c( "void", nil, "public", nil, {"bool"} ),
    hitTest             = c( "bool", nil, "public", nil, {"int"}, {"int"} ),

    setBounds__1        = c( "void", nil, "public", nil, {"int"}, {"int"}, {"int"}, {"int"} ),
    setBounds__2        = c( "void", nil, "public", nil, {"Rectangle", {"&"}, nil, {"int"}} ),
    setBounds__3        = c( "void", nil, "public", nil, {"RelativeRectangle", {"&"}} ),

    setAlwaysOnTop      = c( "void", nil, "public", nil, {"bool"} ),
    setBoundsRelative   = c( "void", nil, "public", nil, {"float"}, {"float"}, {"float"}, {"float"} ),
    setBoundsToFit      = c( "void", nil, "public", nil, 
                            {"int"}, {"int"}, {"int"}, {"int"}, {"Justification"}, {"bool"} ),

    setComponentID      = c( "void", nil, "public", nil, {"String", {"&"}} ),
    toBehind            = c( "void", nil, "public", nil, {"Component", {"*"}} ),
    paintEntireComponent= c( "void", nil, "public", nil, {"Graphics", {"&"}}, {"bool"} ),
    getVisibleArea      = c( "void", nil, "public", nil, {"RectangleList", {"&"}, nil, {"int"}} , {"bool"} ),
    addChildComponent   = c( "void", nil, "public", nil, {"Component", {"*"}}, {"int", nil, {"number", "-1"}} ),

    createComponentSnapshot = c("Image", nil, "public", nil, {"Rectangle", {"&"}, nil, {"int"}}, 
                                {"bool", nil, {"bool", "true"}}, 
                                {"float", nil, {"number", "1.0f"}}),

    enterModalState     = c("void", nil, "public", nil, {"bool", nil, {"bool", "true"}}, 
                            {"ModalComponentManager::Callback", {"*"}, {"object", "nullptr"}}, 
                            {"bool", nil, {"bool", "false"}}),

    getName             = c("String", {"&"}),
    getComponentID      = c("String", {"&"}),
    getChildComponent   = c("Component", {"*"}, "public", nil, {"int"}),
    getLookAndFeel      = c("LookAndFeel", {"&"}),

    getCurrentlyFocusedComponent = c( "Component", {"*"}),

    createFocusTraverser = c("KeyboardFocusTraverser", {"*"}),

    getCurrentlyModalComponent  = c("Component", {"*"}, "public", nil, {"int", nil, {"number", "0"}}),

    getPeer             = c("ComponentPeer",{"*"}),

    getPosition         = c("Point", nil, "public", {"int"}),
    getLocalBounds      = c("Rectangle", nil, "public", {"int"}),
    getScreenPosition   = c("Point", nil, "public", {"int"}),
    getLocalPoint       = c("Point", nil, "public", {"int"}, 
                            {"Component", {"*"}}, {"Point", nil, nil, {"int"}} ),

    getLocalArea        = c("Rectangle", nil, "public", {"int"}, {"Component",{"*"}}, 
                                                    {"Rectangle", {"&"}, nil, {"int"}}),
    localPointToGlobal  = c("Point",nil,"public",{"int"}, {"Point", nil, nil, {"int"}}),
    getMouseXYRelative  = c("Point",nil,"public",{"int"}),

    getBounds           = c("Rectangle", {"&"}, "public", {"int"}),

    findParentComponentOfClass = c("TargetClass", {"*"}, "public", {T=1, "TargetClass"} ),

    -- private
    internalMouseEnter  = c("void", nil, "private", nil,
                                {"MouseInputSource"}, {"Point",nil,nil,{"int"}}, {"Time"} ),

    internalBroughtToFront = c("void", nil, "private"),

    internalChildFocusChange = c("void", nil, "private", nil, {"FocusChangeType"}, 
                                            {"WeakReference",{"&"}, nil, {"Component"}} ),

    internalRepaintUnchecked = c("void", nil, "private", nil,
                                    {"Rectangle", {"&"}, nil, {"int"}}, {"bool"} ),
    
    -- protected
    createNewPeer       = c("ComponentPeer",{"*"}, "protected", nil, {"int"}, {"void",{"*"}}),
}

local total = 0
for k_, v in next, methods do
    local field, fields = "method", "methods"
    local k, n = k_:match( "(.-)__(%d)" )
    k = k or k_
    print(" "..k..(n and "("..n..")" or "").."...")
    print("  ..found")

    local c = getItem( fields, k )
    c = n and c[tonumber(n)] or c

    print("  ..structure")
    assert( c.rule and (c.rule == field)                     , "Not a "..field )
    assert( c.visibility                                     , "Missing visibility field")
    assert( c[field] and ("table"==type(c[field]))           , "Missing "..field.." field" )
    assert( c[field].args and ("table"==type(c[field].args)) , "Missing args field" )
    assert( c[field].name                                    , "Missing "..field.." name" )
    print("  ..visibility")
    expect( v.visibility, c.visibility )
    local c = c[field]
    if ( v.template ) then
        print("  ..template")
        expect( #v.template, #c.template )
        for j, p in next, v.template do
            expect(p, v.template[j])
        end
    end
    if ( v.template_call ) then
        print("  ..template_call")
        expect( #v.template_call, #c.template_call )
        for j, p in next, v.template_call do
            expect( p.type, v.template_call[j].type )
        end
    end
    expect( #v.args, #c.args )
    if ( #v.args > 0 ) then
        print("  ..args ("..#v.args..")")
        for i, a in next, v.args do
            print("   :"..i)
            print("   .return type")
            expect( a.return_type, c.args[i].return_type )
            if ( a.value ) then
                print("   .value")
                expect( a.value.vtype, c.args[i].value.vtype )
                expect( a.value.vvalue, c.args[i].value.vvalue )
            end
            if ( a.pointers ) then
                print("   .pointer")
                expect( #a.pointers, #c.args[i].pointers )
                for j, p in next, a.pointers do
                    expect( p, c.args[i].pointers[j] )
                end
            end
            if ( a.template_call ) then
                print("   .template")
                expect( #a.template_call, #c.args[i].template_call )
                for j, p in next, a.template_call do
                    expect( p.type, c.args[i].template_call[j].type )
                end
            end
        end
    end
    total = total + 1
    print("  ..OK")
end

expect( #parsed.methods, total )

print("ALL OK")
