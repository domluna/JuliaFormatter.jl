# Creates a pretty text version of a CST.
#
# abstract type Leaf end
#
# struct Newline <: Leaf
# end
# Base.length(::Newline) = 1
#
# struct Semicolon <: Leaf
# end
# Base.length(::Semicolon) = 1
#
# struct Whitespace <: Leaf
# end
# Base.length(::Whitespace) = 0
#
# struct Placeholder <: Leaf
# end
# Base.length(::Placeholder) = 0
#
# struct PlaceholderWS <: Leaf
# end
# Base.length(::PlaceholderWS) = 1
#
# const newline = Newline()
# const semicolon = Semicolon()
# const whitespace = Whitespace()
# const placeholder = Placeholder()
# const placeholderWS = PlaceholderWS()

struct PLeaf{T<:CSTParser.LeafNode}
    startline::Int
    endline::Int
    text::String
end
PLeaf(::T, startline::Int, endline::Int, text::String) where T = PLeaf{T}(startline, endline, text)
is_leaf(_) = false
is_leaf(::PLeaf) = true
Base.length(x::PLeaf) = length(x.text)

Newline = PLeaf{CSTParser.LITERAL}(-1, -1, "\n")
Semicolon = PLeaf{CSTParser.PUNCTUATION}(-1, -1, ";")
Whitespace = PLeaf{CSTParser.LITERAL}(-1, -1, " ")
# Used to mark regions for nesting, where it may be replaced with Newline.
Placeholder = PLeaf{CSTParser.LITERAL}(-1, -1, "")
PlaceholderWS = PLeaf{CSTParser.LITERAL}(-2, -2, " ")

is_comma(_) = false
is_comma(x::PLeaf{CSTParser.PUNCTUATION}) = x.text == ","
is_lbrace(_) = false
is_lbrace(x::PLeaf{CSTParser.PUNCTUATION}) = x.text == "{"
is_empty_space(_) = false
is_empty_space(x::PLeaf{CSTParser.LITERAL}) = x.text == "" && x !== Placeholder
is_placeholder(_) = false
is_placeholder(x::PLeaf{CSTParser.LITERAL}) = x === Placeholder || x === PlaceholderWS

mutable struct PTree{T<:CSTParser.AbstractEXPR}
    startline::Int
    endline::Int
    indent::Int
    plength::Int
    nodes::Vector{Union{PTree,PLeaf}}
end
PTree(::T, indent::Int) where T = PTree{T}(-1, -1, indent, 0, Union{PTree,PLeaf}[])
Base.length(x::PTree) = x.plength

#= add_node!(x::PTree, node::Leaf; join_lines=true) = push!(x.nodes, node) =#
function add_node!(x::PTree, node::Union{PTree,PLeaf}; join_lines=false)
    is_empty_space(node) && (return)
    if length(x.nodes) == 0
        x.startline = node.startline
        x.endline = node.endline
        x.plength += length(node)
        push!(x.nodes, node)
        return
    end

    # add newline leaf node
    if x.nodes[end] !== Newline && !join_lines
        x.plength += 1
        push!(x.nodes, Newline)
    end

    (node.startline < x.startline || x.startline == -1) && (x.startline = node.startline)
    (node.endline > x.endline || x.endline == -1) && (x.endline = node.endline)
    x.plength += length(node)
    push!(x.nodes, node)
    return
end

function pretty(x::Union{Vector,CSTParser.AbstractEXPR}, s::State)
    t = PTree(x, nspaces(s))
    for a in x
        n = pretty(a, s)
        add_node!(t, n, join_lines=true)
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.FileH}, s::State)
    t = PTree(x, nspaces(s))
    for a in x
        add_node!(t, pretty(a, s))
    end
    t
end

function pretty(x::CSTParser.IDENTIFIER, s::State)
    loc = cursor_loc(s)
    s.offset += x.fullspan
    PLeaf(x, loc[1], loc[1], x.val)
end

function pretty(x::CSTParser.OPERATOR, s::State)
    loc = cursor_loc(s)
    text = string(CSTParser.Expr(x))
    s.offset += x.fullspan
    PLeaf(x, loc[1], loc[1], text)
end

function pretty(x::CSTParser.KEYWORD, s::State)
    loc = cursor_loc(s)
    text = ""
    text = x.kind == Tokens.ABSTRACT ? "abstract " :
        x.kind == Tokens.BAREMODULE ? "baremodule " :
        x.kind == Tokens.BEGIN ? "begin" :
        x.kind == Tokens.BREAK ? "break" :
        x.kind == Tokens.CATCH ? "catch" :
        x.kind == Tokens.CONST ? "const " :
        x.kind == Tokens.CONTINUE ? "continue" :
        x.kind == Tokens.DO ? " do " :
        x.kind == Tokens.IF ? "if " :
        x.kind == Tokens.ELSEIF ? "elseif" :
        x.kind == Tokens.ELSE ? "else" :
        x.kind == Tokens.END ? "end" :
        x.kind == Tokens.EXPORT ? "export " :
        x.kind == Tokens.FINALLY ? "finally" :
        x.kind == Tokens.FOR ? "for " :
        x.kind == Tokens.FUNCTION ? "function " :
        x.kind == Tokens.GLOBAL ? "global " :
        x.kind == Tokens.IMPORT ? "import " :
        x.kind == Tokens.IMPORTALL ? "importall " :
        x.kind == Tokens.LET ? "let" :
        x.kind == Tokens.LOCAL ? "local " :
        x.kind == Tokens.MACRO ? "macro " :
        x.kind == Tokens.MODULE ? "module " :
        x.kind == Tokens.MUTABLE ? "mutable " :
        x.kind == Tokens.OUTER ? "outer " :
        x.kind == Tokens.PRIMITIVE ? "primitive " :
        x.kind == Tokens.QUOTE ? "quote" :
        x.kind == Tokens.RETURN ? "return" :
        x.kind == Tokens.STRUCT ? "struct " :
        x.kind == Tokens.TYPE ? "type " :
        x.kind == Tokens.TRY ? "try" :
        x.kind == Tokens.USING ? "using " :
        x.kind == Tokens.WHILE ? "while " : ""
    s.offset += x.fullspan
    PLeaf(x, loc[1], loc[1], text)
end

function pretty(x::CSTParser.PUNCTUATION, s::State)
    loc = cursor_loc(s)
    text = x.kind == Tokens.LPAREN ? "(" :
        x.kind == Tokens.LBRACE ? "{" :
        x.kind == Tokens.LSQUARE ? "[" :
        x.kind == Tokens.RPAREN ? ")" :
        x.kind == Tokens.RBRACE ? "}" :
        x.kind == Tokens.RSQUARE ? "]" :
        x.kind == Tokens.COMMA ? "," :
        x.kind == Tokens.SEMICOLON ? ";" :
        x.kind == Tokens.AT_SIGN ? "@" :
        x.kind == Tokens.DOT ? "." : ""
    s.offset += x.fullspan
    PLeaf(x, loc[1], loc[1], text)
end

# TODO: don't escape newlines in TRIPLE_STRING
# this needs a change in CSTParser
function pretty(x::CSTParser.LITERAL, s::State; surround_with_quotes=true)
    loc = cursor_loc(s)
    text = x.kind == Tokens.TRIPLE_STRING && surround_with_quotes ? "\"\"\"" * escape_string(x.val, "\$") * "\"\"\"" :
           x.kind == Tokens.STRING && surround_with_quotes ? "\"" * escape_string(x.val, "\$") * "\"" :
           x.val
    s.offset += x.fullspan
    PLeaf(x, loc[1], loc[1], text)
end

function pretty(x::CSTParser.EXPR{CSTParser.StringH}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, PLeaf{CSTParser.LITERAL}(-1, -1, "\""))
    for a in x
        if a isa CSTParser.LITERAL
            n = pretty(a, s, surround_with_quotes=false)
            n.text == "" && (continue)
            add_node!(t, n)
        else
            add_node!(t, pretty(a, s))
        end
    end
    add_node!(t, PLeaf{CSTParser.LITERAL}(-1, -1, "\""))
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.MacroCall}, s::State)
    # Docstring
    t = PTree(x, nspaces(s))
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        add_node!(t, pretty(x.args[1], s))

        offset = s.offset
        loc1 = cursor_loc(s)
        s.offset += x.args[2].fullspan
        loc2 = cursor_loc(s, s.offset-1)
        #= @info "DOC POSITION START", loc1 =#
        #= @info "DOC POSITION END", loc2 =#

        #= ws = repeat(" ", nspaces(s)) =#
        tq = "\"\"\""
        starts_tq = startswith(s.doc.text[offset:offset+loc1[3]-loc1[2]], tq)
        quote_len = starts_tq ? 3 : 1
        #= @info "STARTS WITH TRIPLE QUOTES", starts_tq =#

        add_node!(t, PLeaf{CSTParser.LITERAL}(loc1[1], loc1[1], tq), join_lines=true)

        if loc1[3] - loc1[2] > quote_len
            sidx = starts_tq ? offset + 3 : offset + 1
            if loc1[1] == loc2[1]
                eidx = starts_tq ? offset+loc1[3]-loc1[2]-4 : offset+loc1[3]-loc1[2]-2
                v = s.doc.text[sidx:eidx]
                #= @info "H1", v =#
            else
                #= eidx = starts_tq ? o+loc1[3]-loc1[2]-1 : o+loc1[3]-loc1[2]-1 =#
                eidx = offset+loc1[3]-loc1[2]-1
                v = s.doc.text[sidx:eidx]
                #= @info "H2", v =#
            end
            #= e = merge(e, Edit(loc1[1], loc1[1], "\n" * ws * v), s) =#
            add_node!(t, PLeaf{CSTParser.LITERAL}(loc1[1], loc1[1], v))
        end

        offset = s.offset
        if loc1[1] == loc2[1]
            #= e = merge(e, Edit(loc2[1]+1, loc2[1]+1, tq), s) =#
            add_node!(t, PLeaf{CSTParser.LITERAL}(loc2[1]+1, loc2[1]+1, tq))
        elseif loc2[3] > quote_len + 1
            v = strip(starts_tq ? s.doc.text[offset-loc2[2]:offset-5] : s.doc.text[offset-loc2[2]:offset-3])
            #= @info "H3", v =#
            if v  != ""
                #= e = merge(e, Edit(loc2[1], loc2[1], v * "\n" * ws * tq), s) =#
                add_node!(t, PLeaf{CSTParser.LITERAL}(loc2[1], loc2[1], v))
                add_node!(t, PLeaf{CSTParser.LITERAL}(loc2[1], loc2[1], tq))
            else
                #= e = merge(e, Edit(loc2[1], loc2[1], tq), s) =#
                add_node!(t, PLeaf{CSTParser.LITERAL}(loc2[1], loc2[1], tq))
            end
        else
            #= e = merge(e, Edit(loc2[1], loc2[1], tq), s) =#
            add_node!(t, PLeaf{CSTParser.LITERAL}(loc2[1], loc2[1], tq))
        end

        add_node!(t, pretty(x.args[3], s))
        return t
    end

    # same as CSTParser.EXPR{CSTParser.CALL} but whitespace sensitive
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        @info "MACROCALL" a

        # i == 1 is probably redundant
        if i == 1 && a isa CSTParser.EXPR{CSTParser.MacroName}
            if a.fullspan - a.span > 0
                add_node!(t, n, join_lines=true)
                add_node!(t, Whitespace, join_lines=true)
            else
                add_node!(t, n, join_lines=true)
                # assumes the next argument is a brace of some sort
            end
        #= elseif a isa CSTParser.KEYWORD =#
        #=     e = merge(e, " " * ei, s; join_lines=true) =#
        elseif a.fullspan - a.span > 0
            add_node!(t, n, join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        elseif CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, n, join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, n, join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Block}, s::State; ignore_single_line=false)
    single_line = ignore_single_line ? false : cursor_loc(s)[1] == cursor_loc(s, s.offset+x.span-1)[1] 
    #= @info "" single_line =#
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if i < length(x) && CSTParser.is_comma(a) && x.args[i+1] isa CSTParser.PUNCTUATION
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a) && i != length(x)
            add_node!(t, n, join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        elseif single_line
            if i == 1 || CSTParser.is_comma(x.args[i-1])
                add_node!(t, n, join_lines=true)
            else
                add_node!(t, n, join_lines=true)
                add_node!(t, Semicolon, join_lines=true)
                add_node!(t, Whitespace, join_lines=true)
            end
        else
            add_node!(t, n)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Abstract}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, pretty(x.args[3], s), join_lines=true)
    add_node!(t, Whitespace, join_lines=true)
    add_node!(t, pretty(x.args[4], s), join_lines=true)
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.FunctionDef}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    if length(x) > 3
        s.indents += 1
        add_node!(t, pretty(x.args[3], s, ignore_single_line=true))
        s.indents -= 1
        add_node!(t, pretty(x.args[4], s))
    else
        # function stub, i.e. "function foo end"
        # this should be on one line
        add_node!(t, Whitespace, join_lines=true)
        add_node!(t, pretty(x.args[3], s), join_lines=true)
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Macro,CSTParser.Struct}
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    s.indents += 1
    add_node!(t, pretty(x.args[3], s, ignore_single_line=true))
    s.indents -= 1
    add_node!(t, pretty(x.args[4], s))
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Mutable}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, pretty(x.args[3], s), join_lines=true)
    s.indents += 1
    add_node!(t, pretty(x.args[4], s, ignore_single_line=true))
    s.indents -= 1
    add_node!(t, pretty(x.args[5], s))
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.For,CSTParser.While}
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    s.indents += 1
    add_node!(t, pretty(x.args[3], s, ignore_single_line=true))
    s.indents -= 1
    add_node!(t, pretty(x.args[4], s))
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Do}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, pretty(x.args[3], s), join_lines=true)
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        s.indents += 1
        add_node!(t, pretty(x.args[4], s, ignore_single_line=true))
        s.indents -= 1
    end
    add_node!(t, pretty(x.args[end], s))
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Try}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    s.indents += 1
    add_node!(t, pretty(x.args[2], s, ignore_single_line=true))
    s.indents -= 1
    add_node!(t, pretty(x.args[3], s))

    if x.args[4].fullspan != 0
        add_node!(t, Whitespace, join_lines=true)
        add_node!(t, pretty(x.args[4], s), join_lines=true)
    end

    s.indents += 1
    add_node!(t, pretty(x.args[5], s, ignore_single_line=true))
    s.indents -= 1
    add_node!(t, pretty(x.args[6], s))

    if length(x.args) > 6
        s.indents += 1
        add_node!(t, pretty(x.args[7], s, ignore_single_line=true))
        s.indents -= 1
        add_node!(t, pretty(x.args[8], s))
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.ModuleH}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    if x.args[3].fullspan != 0
        add_node!(t, pretty(x.args[3], s))
    end
    add_node!(t, pretty(x.args[4], s))
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Return}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if x.args[2].fullspan != 0
        for a in x.args[2:end]
            add_node!(t, Whitespace, join_lines=true)
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Begin}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    s.indents += 1
    add_node!(t, pretty(x.args[2], s, ignore_single_line=true))
    s.indents -= 1
    add_node!(t, pretty(x.args[3], s))
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Quote}, s::State)
    t = PTree(x, nspaces(s))
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        add_node!(t, pretty(x.args[1], s))
        s.indents += 1
        add_node!(t, pretty(x.args[2], s, ignore_single_line=true))
        s.indents -= 1
        add_node!(t, pretty(x.args[3], s))
        return t
    end
    add_node!(t, pretty(x.args, s))
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Let}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if length(x.args) > 3
        add_node!(t, Whitespace, join_lines=true)
        add_node!(t, pretty(x.args[2], s), join_lines=true)
        s.indents += 1
        add_node!(t, pretty(x.args[3], s, ignore_single_line=true))
        s.indents -= 1
    else
        s.indents += 1
        add_node!(t, pretty(x.args[2], s, ignore_single_line=true))
        s.indents -= 1
    end
    add_node!(t, pretty(x.args[end], s))
    t
end

# TODO: See if we can change .If in CSTParser to either
# 1) 4 element struct i.e. "if cond block end"
# 2) 6 element struct i.e. "if cond block1 else block2 end"
# 3) n element struct, same as 1) or 2) but with elseif
function pretty(x::CSTParser.EXPR{CSTParser.If}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.IF
        add_node!(t, pretty(x.args[2], s), join_lines=true)
        s.indents += 1
        add_node!(t, pretty(x.args[3], s, ignore_single_line=true))
        s.indents -= 1
        add_node!(t, pretty(x.args[4], s))
        if length(x.args) > 4
            s.indents += 1
            if x.args[4].kind == Tokens.ELSEIF
                add_node!(t, Whitespace, join_lines=true)
                add_node!(t, pretty(x.args[5], s), join_lines=true)
            else
                add_node!(t, pretty(x.args[5], s, ignore_single_line=true))
            end
            s.indents -= 1
            # END KEYWORD
            add_node!(t, pretty(x.args[6], s))
        end
    else
        add_node!(t, pretty(x.args[2], s, ignore_single_line=true))
        if length(x.args) > 2
            s.indents -= 1
            add_node!(t, pretty(x.args[3], s))
            s.indents += 1

            # this either else or elseif
            if x.args[3].kind == Tokens.ELSEIF
                add_node!(t, Whitespace, join_lines=true)
                add_node!(t, pretty(x.args[4], s), join_lines=true)
            else
                add_node!(t, pretty(x.args[4], s, ignore_single_line=true))
            end
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Comparison,CSTParser.ChainOpCall,CSTParser.Kw}
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if a isa CSTParser.OPERATOR
            add_node!(t, Whitespace, join_lines=true)
            add_node!(t, n, join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        elseif i == length(x) - 1 && a isa CSTParser.PUNCTUATION && x.args[i+1] isa CSTParser.PUNCTUATION
            add_node!(t, n, join_lines=true)
        elseif a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA && i != length(x)
            add_node!(t, n, join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, n, join_lines=true)
        end
    end
    t
end

nestable(_) = false
function nestable(x::T) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    x.op.kind == Tokens.ISSUBTYPE && (return false)
    true
end

function pretty(x::T, s::State) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.arg1, s))
    if CSTParser.precedence(x.op) in (8, 13, 14, 16) && x.op.kind != Tokens.ANON_FUNC
        add_node!(t, pretty(x.op, s), join_lines=true)
    elseif x.op.kind == Tokens.EX_OR
        add_node!(t, Whitespace, join_lines=true)
        add_node!(t, pretty(x.op, s), join_lines=true)
    elseif nestable(x)
        add_node!(t, Whitespace, join_lines=true)
        add_node!(t, pretty(x.op, s), join_lines=true)
        add_node!(t, PlaceholderWS, join_lines=true)
    else
        add_node!(t, Whitespace, join_lines=true)
        add_node!(t, pretty(x.op, s), join_lines=true)
        add_node!(t, Whitespace, join_lines=true)
    end
    add_node!(t, pretty(x.arg2, s), join_lines=true)
    t
end

# A where B
# should format B prior to A
#
# line_offset_A = ...
# line_offset_B = ...
# indent_A = ...
# indent_B = ...
#
# line_offset
function pretty(x::CSTParser.WhereOpCall, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.arg1, s))

    add_node!(t, Whitespace, join_lines=true)
    add_node!(t, pretty(x.op, s), join_lines=true)
    add_node!(t, Whitespace, join_lines=true)

    # Used to mark where `B` starts.
    add_node!(t, Placeholder, join_lines=true)

    for a in x.args
        add_node!(t, pretty(a, s), join_lines=true)
        if CSTParser.is_comma(a)
            add_node!(t, Placeholder, join_lines=true)
        end
    end
    t
end

# C ? E1 : E2
#
# if the above is > s.max_width
# format to
#
# C ? E1 :
# E2
#
# still doesn't fit?
#
# C ?
# E1 :
# E2
#
# C1 ? E1 : C2 ? E2 : C3 ? E3 : C4 ? E4 : E5
#
# [C1 ?, E1 :, C2 ? E2 : C3 ? E3 : C4 ? E4 : E5]
# [C2 ?, E2 :, C3 ? E3 : C4 ? E4 : E5]
# [C3 ?, E3 :, C4 ? E4 : E5]
# [C4 ?, E4 :, E5]
function pretty(x::CSTParser.ConditionalOpCall, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.cond, s))
    add_node!(t, Whitespace, join_lines=true)
    add_node!(t, pretty(x.op1, s), join_lines=true)
    add_node!(t, PlaceholderWS, join_lines=true)

    add_node!(t, pretty(x.arg1, s), join_lines=true)
    add_node!(t, Whitespace, join_lines=true)
    add_node!(t, pretty(x.op2, s), join_lines=true)
    add_node!(t, PlaceholderWS, join_lines=true)

    add_node!(t, pretty(x.arg2, s), join_lines=true)
    t
end

function pretty(x::CSTParser.UnarySyntaxOpCall, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.arg1, s))
    add_node!(t, pretty(x.arg2, s), join_lines=true)
    t
end

function pretty(x::CSTParser.UnaryOpCall, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.op, s))
    add_node!(t, pretty(x.arg, s), join_lines=true)
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Curly,CSTParser.Call}
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)

    for (i, a) in enumerate(x.args[3:end])
        if CSTParser.is_comma(a) && i < length(x) - 3 && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, pretty(a, s), join_lines=true)
            if x isa CSTParser.EXPR{CSTParser.Call} 
                add_node!(t, PlaceholderWS, join_lines=true)
            else
                add_node!(t, Placeholder, join_lines=true)
            end
            #= add_node!(t, Placeholder, join_lines=true) =#
        elseif a isa CSTParser.EXPR{CSTParser.Parameters}
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Semicolon, join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
            #= add_node!(t, Placeholder, join_lines=true) =#
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    #= add_node!(t, Placeholder, join_lines=true) =#
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Parameters}, s::State)
    t = PTree(x, nspaces(s))
    for a in (x)
        if CSTParser.is_comma(a)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.InvisBrackets,CSTParser.Braces}
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, pretty(a, s), join_lines=true)
            if !(x isa CSTParser.EXPR{CSTParser.Braces})
                add_node!(t, PlaceholderWS, join_lines=true)
            else
                add_node!(t, Placeholder, join_lines=true)
            end
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Export,CSTParser.Import,CSTParser.Using}
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    sidx = 2
    for (i, a) in enumerate(x.args[2:end])
        if CSTParser.is_comma(a) || CSTParser.is_colon(a)
            CSTParser.is_colon(a) && (sidx = 3)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Vcat}, s::State)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if i > 1 && i < length(x) - 1
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Semicolon, join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.TypedVcat}, s::State)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if i > 2 && i < length(x) - 1
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Semicolon, join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Hcat}, s::State)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if i > 1 && i < length(x) - 1
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.TypedHcat}, s::State)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if i > 2 && i < length(x) - 1
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Row}, s::State)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if i < length(x)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace, join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Expr KEYWORD Expr
function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Generator,CSTParser.Filter}
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if a isa CSTParser.KEYWORD
            add_node!(t, Whitespace, join_lines=true)
            add_node!(t, pretty(a, s), join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end
