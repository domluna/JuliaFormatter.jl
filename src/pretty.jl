# Creates a _prettified_ version of a CST.

abstract type AbstractPLeaf end

struct Newline <: AbstractPLeaf
end
Base.length(::Newline) = 1

struct Semicolon <: AbstractPLeaf
end
Base.length(::Semicolon) = 1

struct Whitespace <: AbstractPLeaf
end
Base.length(::Whitespace) = 1

struct Placeholder <: AbstractPLeaf
end
Base.length(::Placeholder) = 0

struct PlaceholderWS <: AbstractPLeaf
end
Base.length(::PlaceholderWS) = 1

struct NotCode <: AbstractPLeaf
    startline::Int
    endline::Int
    indent::Int
end
Base.length(::NotCode) = 0

const newline = Newline()
const semicolon = Semicolon()
const whitespace = Whitespace()
const placeholder = Placeholder()
const placeholderWS = PlaceholderWS()

struct PLeaf{T} <: AbstractPLeaf
    startline::Int
    endline::Int
    text::String
end
PLeaf(::T, startline::Int, endline::Int, text::String) where T = PLeaf{T}(startline, endline, text)
Base.length(x::PLeaf) = length(x.text)

const empty_start = PLeaf{CSTParser.LITERAL}(1, 1, "")

is_lbrace(_) = false
is_lbrace(x::PLeaf{CSTParser.PUNCTUATION}) = x.text == "{"
is_placeholder(x) = x === placeholder || x === placeholderWS
is_empty_lit(_) = false
is_empty_lit(x::PLeaf{CSTParser.LITERAL}) = x.text == ""

mutable struct PTree{T}
    startline::Int
    endline::Int
    indent::Int
    len::Int
    nodes::Vector{Union{PTree,AbstractPLeaf}}
end
PTree(::T, indent::Int) where T = PTree{T}(-1, -1, indent, 0, Union{PTree,PLeaf}[])
PTree{T}(indent::Int) where T = PTree{T}(-1, -1, indent, 0, Union{PTree,PLeaf}[])
Base.length(x::PTree) = x.len

function add_node!(t::PTree, node::AbstractPLeaf)
    t.len += length(node)
    push!(t.nodes, node)
end

function add_node!(t::PTree, node::Union{PTree,PLeaf}; join_lines=false)
    if length(t.nodes) == 0
        t.startline = node.startline
        t.endline = node.endline
        t.len += length(node)
        push!(t.nodes, node)
        return
    end

    if t.nodes[end] !== newline && !join_lines
        notcode_startline = t.nodes[end].endline+1 
        notcode_endline = node.startline-1
        if notcode_startline <= notcode_endline && node isa PTree
            add_node!(t, newline)
            push!(t.nodes, NotCode(notcode_startline, notcode_endline, node.indent))
        else
            add_node!(t, newline)
        end
    end

    if node.startline < t.startline || t.startline == -1 
        t.startline = node.startline
    end
    if node.endline > t.endline || t.endline == -1 
        t.endline = node.endline
    end
    t.len += length(node)
    push!(t.nodes, node)
    nothing
end

function pretty(x::T, s::State) where T <: Union{AbstractVector,CSTParser.AbstractEXPR}
    t = PTree(x, nspaces(s))
    for a in x
        n = pretty(a, s)
        n === empty_start && (continue)
        add_node!(t, n, join_lines=true)
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.FileH}, s::State)
    t = PTree(x, nspaces(s))
    for a in x
        n = pretty(a, s)
        n === empty_start && (continue)
        add_node!(t, n)
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

function pretty(x::CSTParser.LITERAL, s::State; surround_with_quotes=true, is_doc=false)
    loc0 = cursor_loc(s)
    s.offset += x.fullspan
    if !CSTParser.is_lit_string(x)
        return PLeaf(x, loc0[1], loc0[1], x.val)
    end

    loc1 = cursor_loc(s, s.offset-1)
    # At the moment CSTParser does not return Tokens.TRIPLE_STRING
    # tokens :(
    # https://github.com/ZacLN/CSTParser.jl/issues/88
    #
    # So we'll just look at the source directly!
    ts = s.doc.text[loc0[2]:loc0[2]+2] == "\"\"\"" ? true : false

    @info "" is_doc surround_with_quotes ts loc0 loc1 x.val
    #= @info "" s.doc.text =#

    str = s.doc.text[s.doc.ranges[loc0[1]]]
    if is_doc && loc0[1] == loc1[1] 
        t = PTree{CSTParser.StringH}(nspaces(s))
        add_node!(t, PLeaf{CSTParser.LITERAL}(loc0[1], loc0[1], "\"\"\""))
        i1, i2 = ts ? (3, 3) : (1, 1)

        # strip newlines
        str[loc1[2]] == '\n' && (i2 += 1)
        val = str[loc0[2]+i1:loc1[2]-i2]
        @info "" val str[loc0[2]:loc1[2]]
        add_node!(t, PLeaf{CSTParser.LITERAL}(loc0[1]+1, loc0[1]+1, val))
        add_node!(t, PLeaf{CSTParser.LITERAL}(loc0[1]+2, loc0[1]+2, "\"\"\""))
        return t
    elseif loc0[1] == loc1[1] 
        i1, i2 = surround_with_quotes ? (0, 0) : ts ? (3, 3) : (1, 1)
        str[loc1[2]] == '\n' && (i2 += 1)
        val = str[loc0[2]+i1:loc1[2]-i2]
        @info "" val str[loc0[2]:loc1[2]]
        return PLeaf(x, loc0[1], loc1[1], val)
    end

    # Multiline string
    t = PTree{CSTParser.StringH}(nspaces(s))
    for l in loc0[1]:loc1[1]
        str = s.doc.text[s.doc.ranges[l]]
        @info "multiline string" str

        # only first and last lines will have surrounding quotes
        if l == loc0[1]
            if is_doc
                i1 = ts ? 3 : 1
                i2 = str[end] == '\n' ? 1 : 0
                add_node!(t, PLeaf{CSTParser.LITERAL}(l, l, "\"\"\""))
                val = str[loc0[2]+i1:end-i2]
                if val != ""
                    add_node!(t, PLeaf{CSTParser.LITERAL}(l, l, val))
                end
            else
                i1 = surround_with_quotes ? 0 : ts ? 3 : 1
                i2 = str[end] == '\n' ? 1 : 0
                val = str[loc0[2]+i1:end-i2]
                add_node!(t, PLeaf{CSTParser.LITERAL}(l, l, val))
            end
        elseif l == loc1[1]
            if is_doc
                i2 = ts ? 3 : 1
                str[loc1[2]] == '\n' && (i2 += 1)
                val = str[1:loc1[2]-i2]
                @info "" l val
                if val != ""
                    add_node!(t, PLeaf{CSTParser.LITERAL}(l, l, val))
                end
                add_node!(t, PLeaf{CSTParser.LITERAL}(l, l, "\"\"\""))
            else
                i2 = surround_with_quotes ? 0 : ts ? 3 : 1
                str[end] == '\n' && (i2 += 1)
                val = str[1:loc1[2]-i2]
                add_node!(t, PLeaf{CSTParser.LITERAL}(l, l, val))
            end
        else
            i2 = str[end] == '\n' ? 1 : 0
            val = str[1:end-i2]
            add_node!(t, PLeaf{CSTParser.LITERAL}(l, l, val))
        end
    end
    return t
end

function pretty(x::CSTParser.EXPR{CSTParser.StringH}, s::State; is_doc=false)
    t = PTree(x, nspaces(s))
    loc = cursor_loc(s)
    q = is_doc ? "\"\"\"" : "\""
    add_node!(t, PLeaf{CSTParser.LITERAL}(loc[1], loc[1], q))

    for (i, a) in enumerate(x)
        if a isa CSTParser.LITERAL
            n = pretty(a, s, surround_with_quotes=false, is_doc=is_doc)
            #= @info "" n =#
            if i == 1 && is_doc && length(n) != 0
                add_node!(t, n)
            else
                add_node!(t, n, join_lines=true)
            end
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end

    loc = cursor_loc(s)
    n = PLeaf{CSTParser.LITERAL}(loc[1], loc[1], q)
    if is_empty_lit(t.nodes[end].nodes[end]) || !is_doc
        add_node!(t, n, join_lines=true)
    else
        add_node!(t, n)
    end

    t
end

function pretty(x::CSTParser.EXPR{CSTParser.MacroCall}, s::State)
    t = PTree(x, nspaces(s))
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        loc = cursor_loc(s)
        n = pretty(x.args[1], s)
        n.startline = loc[1]
        n.endline = loc[1]
        add_node!(t, n)

        if x.args[2] isa CSTParser.LITERAL
            add_node!(t, pretty(x.args[2], s, is_doc=true), join_lines=true)
        else
            add_node!(t, pretty(x.args[2], s, is_doc=true), join_lines=true)
        end

        add_node!(t, pretty(x.args[3], s))
        return t
    end

    # same as CSTParser.EXPR{CSTParser.CALL} but whitespace sensitive
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if a isa CSTParser.EXPR{CSTParser.MacroName}
            if a.fullspan - a.span > 0
                add_node!(t, n, join_lines=true)
                add_node!(t, whitespace)
            else
                # assumes the next argument is a brace of some sort
                add_node!(t, n, join_lines=true)
            end
        elseif CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, n, join_lines=true)
            add_node!(t, placeholderWS)
        elseif a.fullspan - a.span > 0 && i < length(x)
            add_node!(t, n, join_lines=true)
            add_node!(t, whitespace)
        else
            add_node!(t, n, join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Block}, s::State; ignore_single_line=false)
    single_line = ignore_single_line ? false : cursor_loc(s)[1] == cursor_loc(s, s.offset+x.span-1)[1] 
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if i < length(x) && CSTParser.is_comma(a) && x.args[i+1] isa CSTParser.PUNCTUATION
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a) && i != length(x)
            add_node!(t, n, join_lines=true)
            add_node!(t, whitespace)
        elseif single_line
            if i == 1 || CSTParser.is_comma(x.args[i-1])
                add_node!(t, n, join_lines=true)
            else
                add_node!(t, semicolon)
                add_node!(t, whitespace)
                add_node!(t, n, join_lines=true)
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
    add_node!(t, whitespace)
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
        add_node!(t, whitespace)
        add_node!(t, pretty(x.args[3], s), join_lines=true)
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Macro,CSTParser.Struct}
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    if x.args[3].fullspan == 0
        add_node!(t, whitespace)
        add_node!(t, pretty(x.args[4], s), join_lines=true)
    else
        s.indents += 1
        add_node!(t, pretty(x.args[3], s, ignore_single_line=true))
        s.indents -= 1
        add_node!(t, pretty(x.args[4], s))
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Mutable}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, pretty(x.args[3], s), join_lines=true)
    if x.args[4].fullspan == 0
        add_node!(t, whitespace)
        add_node!(t, pretty(x.args[5], s), join_lines=true)
    else
        s.indents += 1
        add_node!(t, pretty(x.args[4], s, ignore_single_line=true))
        s.indents -= 1
        add_node!(t, pretty(x.args[5], s))
    end
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
        add_node!(t, whitespace)
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
    if x.args[3].fullspan == 0
        add_node!(t, whitespace)
        add_node!(t, pretty(x.args[4], s), join_lines=true)
    else
        add_node!(t, pretty(x.args[3], s))
        add_node!(t, pretty(x.args[4], s))
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Return}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if x.args[2].fullspan != 0
        for a in x.args[2:end]
            add_node!(t, whitespace)
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Begin}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if x.args[2].fullspan == 0
        add_node!(t, whitespace)
        add_node!(t, pretty(x.args[3], s), join_lines=true)
    else
        s.indents += 1
        add_node!(t, pretty(x.args[2], s, ignore_single_line=true))
        s.indents -= 1
        add_node!(t, pretty(x.args[3], s))
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Quote}, s::State)
    t = PTree(x, nspaces(s))
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        add_node!(t, pretty(x.args[1], s))
        if x.args[2].fullspan == 0
            add_node!(t, whitespace)
            add_node!(t, pretty(x.args[3], s), join_lines=true)
        else
            s.indents += 1
            add_node!(t, pretty(x.args[2], s, ignore_single_line=true))
            s.indents -= 1
            add_node!(t, pretty(x.args[3], s))
        end
        return t
    end
    add_node!(t, pretty(x.args, s))
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Let}, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if length(x.args) > 3
        add_node!(t, whitespace)
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
                add_node!(t, whitespace)
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
                add_node!(t, whitespace)
                add_node!(t, pretty(x.args[4], s), join_lines=true)
            else
                add_node!(t, pretty(x.args[4], s, ignore_single_line=true))
            end
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Comparison,CSTParser.ChainOpCall}
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if a isa CSTParser.OPERATOR
            add_node!(t, whitespace)
            add_node!(t, n, join_lines=true)
            add_node!(t, whitespace)
        elseif i == length(x) - 1 && a isa CSTParser.PUNCTUATION && x.args[i+1] isa CSTParser.PUNCTUATION
            add_node!(t, n, join_lines=true)
        elseif a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA && i != length(x)
            add_node!(t, n, join_lines=true)
            add_node!(t, whitespace)
        else
            add_node!(t, n, join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Kw}, s::State)
    t = PTree(x, nspaces(s))
    for a in x
        add_node!(t, pretty(a, s), join_lines=true)
    end
    t
end

nestable(_) = false
function nestable(x::T) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    x.op.kind == Tokens.ANON_FUNC && (return false)
    x.op.kind == Tokens.PAIR_ARROW && (return false)
    CSTParser.precedence(x.op) in (1, 6) && (return false)
    true
end

function pretty(x::T, s::State; nospaces=false, nonest=false) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    t = PTree(x, nspaces(s))

    x.op.kind == Tokens.COLON && (nospaces = true)
    arg1 = x.arg1 isa T ? pretty(x.arg1, s, nospaces=nospaces, nonest=nonest) : pretty(x.arg1, s)
    add_node!(t, arg1)

    if (CSTParser.precedence(x.op) in (8, 13, 14, 16) && x.op.kind != Tokens.ANON_FUNC) || nospaces
        add_node!(t, pretty(x.op, s), join_lines=true)
    elseif x.op.kind == Tokens.EX_OR
        add_node!(t, whitespace)
        add_node!(t, pretty(x.op, s), join_lines=true)
    elseif nestable(x) && !nonest
        add_node!(t, whitespace)
        add_node!(t, pretty(x.op, s), join_lines=true)
        add_node!(t, placeholderWS)
    else
        add_node!(t, whitespace)
        add_node!(t, pretty(x.op, s), join_lines=true)
        add_node!(t, whitespace)
    end

    arg2 = x.arg2 isa T ? pretty(x.arg2, s, nospaces=nospaces, nonest=nonest) : pretty(x.arg2, s)
    add_node!(t, arg2, join_lines=true)

    t
end

function pretty(x::CSTParser.WhereOpCall, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.arg1, s))

    add_node!(t, whitespace)
    add_node!(t, pretty(x.op, s), join_lines=true)
    add_node!(t, whitespace)

    # Used to mark where `B` starts.
    add_node!(t, placeholder)

    for a in x.args
        add_node!(t, pretty(a, s), join_lines=true)
        if CSTParser.is_comma(a)
            add_node!(t, placeholder)
        end
    end
    t
end

function pretty(x::CSTParser.ConditionalOpCall, s::State)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.cond, s))
    add_node!(t, whitespace)
    add_node!(t, pretty(x.op1, s), join_lines=true)
    add_node!(t, placeholderWS)

    add_node!(t, pretty(x.arg1, s), join_lines=true)
    add_node!(t, whitespace)
    add_node!(t, pretty(x.op2, s), join_lines=true)
    add_node!(t, placeholderWS)

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
                add_node!(t, placeholderWS)
            else
                add_node!(t, placeholder)
            end
        elseif a isa CSTParser.EXPR{CSTParser.Parameters}
            add_node!(t, semicolon)
            add_node!(t, placeholderWS)
            add_node!(t, pretty(a, s), join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Parameters}, s::State)
    t = PTree(x, nspaces(s))
    for a in x.args
        if CSTParser.is_comma(a)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, placeholderWS)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.InvisBrackets}
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, placeholderWS)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Braces}, s::State)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, placeholder)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Vect}, s::State)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, placeholderWS)
        elseif a isa Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
            add_node!(t, pretty(a, s, nospaces=true), join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Export,CSTParser.Import,CSTParser.Using}
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    for (i, a) in enumerate(x.args[2:end])
        if CSTParser.is_comma(a)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, placeholderWS)
        elseif CSTParser.is_colon(a)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, whitespace)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.Ref}, s::State)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if a isa Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
            add_node!(t, pretty(a, s, nospaces=true), join_lines=true)
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
            add_node!(t, semicolon)
            add_node!(t, whitespace)
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
            add_node!(t, semicolon)
            add_node!(t, whitespace)
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
            add_node!(t, whitespace)
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
            add_node!(t, whitespace)
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
            add_node!(t, whitespace)
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
            add_node!(t, whitespace)
            add_node!(t, pretty(a, s), join_lines=true)
        elseif a isa Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
            add_node!(t, pretty(a, s, nonest=true), join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end
