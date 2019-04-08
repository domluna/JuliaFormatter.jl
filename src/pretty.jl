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

is_placeholder(x) = x === placeholder || x === placeholderWS
is_empty_lit(_) = false
is_empty_lit(x::PLeaf{CSTParser.LITERAL}) = x.text == ""
is_closer(_) = false
is_closer(x::PLeaf{CSTParser.PUNCTUATION}) = x.text == "}" || x.text == ")" || x.text == "]"
is_opener(_) = false
is_opener(x::PLeaf{CSTParser.PUNCTUATION}) = x.text == "{" || x.text == "(" || x.text == "["

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

# which_quote is used for multiline strings
# to determines whether
# 1. quotes on start and last line are included (default 0)
# 2. quotes on start line are included (1)
# 3. quotes on end line are included (2)
function pretty(x::CSTParser.LITERAL, s::State; include_quotes=true)
    loc0 = cursor_loc(s)
    if !CSTParser.is_lit_string(x)
        s.offset += x.fullspan
        return PLeaf(x, loc0[1], loc0[1], x.val)
    end
    
    # At the moment CSTParser does not return Tokens.TRIPLE_STRING
    # tokens :(
    # https://github.com/ZacLN/CSTParser.jl/issues/88
    #
    # Also strings are unescaped to by CSTParser
    # to mimic Meta.parse which makes finding newlines
    # for indentation problematic.
    #
    # So we'll just look at the source directly!

    startline, endline, str = s.doc.lit_strings[s.offset-1]
    s.offset += x.fullspan
    #= @info "" startline endline include_quotes str =#

    if !include_quotes
        idx = startswith(str, "\"\"\"") ? 4 : 2
        str = str[idx:end-idx+1]
        #= @info "after removing quotes" str =#
        str = strip(str, ' ')
        #= @info "after stripping surrounding whitespace" str =#
        str[1] == '\n' && (str = str[2:end])
        str[end] == '\n' && (str = str[1:end-1])
        #= @info "after removing extra newlines" str =#
    end

    lines = split(str, "\n")
    ns = !include_quotes ? max(0, nspaces(s)-s.indent_size) : nspaces(s)
    t = PTree{CSTParser.EXPR{CSTParser.StringH}}(ns)
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        add_node!(t, PLeaf{CSTParser.LITERAL}(ln, ln, l))
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.StringH}, s::State; include_quotes=true)
    startline, endline, str = s.doc.lit_strings[s.offset-1]
    s.offset += x.fullspan
    #= @info "" startline endline include_quotes str =#

    if !include_quotes
        idx = startswith(str, "\"\"\"") ? 4 : 2
        str = str[idx:end-idx+1]
        #= @info "after removing quotes" str =#
        str = strip(str, ' ')
        #= @info "after stripping surrounding whitespace" str =#
        str[1] == '\n' && (str = str[2:end])
        str[end] == '\n' && (str = str[1:end-1])
        #= @info "after removing extra newlines" str =#
    end

    lines = split(str, "\n")
    ns = !include_quotes ? max(0, nspaces(s)-s.indent_size) : nspaces(s)
    t = PTree(x, ns)
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        add_node!(t, PLeaf{CSTParser.LITERAL}(ln, ln, l))
    end
    t
end

function pretty(x::CSTParser.EXPR{CSTParser.MacroCall}, s::State)
    t = PTree(x, nspaces(s))
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        loc = cursor_loc(s)
        # x.args[1] is empty and fullspan is 0 so we can skip it
        add_node!(t, PLeaf{CSTParser.LITERAL}(loc[1], loc[1], "\"\"\""))
        add_node!(t, pretty(x.args[2], s, include_quotes=false))
        loc = cursor_loc(s)
        add_node!(t, PLeaf{CSTParser.LITERAL}(loc[1], loc[1], "\"\"\""))
        add_node!(t, pretty(x.args[3], s))
        return t
    end

    # same as CSTParser.Call but whitespace sensitive
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
        elseif is_opener(n)
            add_node!(t, n, join_lines=true)
            add_node!(t, placeholder)
        elseif is_closer(n)
            add_node!(t, placeholder)
            add_node!(t, n, join_lines=true)
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
    if x.op.kind == Tokens.EQ && CSTParser.defines_function(x)
        return true
    end
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
        n = pretty(a, s)
        if is_opener(n)
            add_node!(t, n, join_lines=true)
            add_node!(t, placeholder)
        elseif is_closer(n)
            add_node!(t, placeholder)
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a)
            add_node!(t, n, join_lines=true)
            add_node!(t, placeholder)
        else
            add_node!(t, n, join_lines=true)
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

    curly = x isa CSTParser.EXPR{CSTParser.Curly}
    no_args = length(x.args) < 4

    !no_args && (add_node!(t, placeholder))
    for (i, a) in enumerate(x.args[3:end])
        if i + 2 == length(x) && !no_args
            add_node!(t, placeholder)
            add_node!(t, pretty(a, s), join_lines=true)
        elseif CSTParser.is_comma(a) && i < length(x) - 3 && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, pretty(a, s), join_lines=true)
            if curly
                add_node!(t, placeholder)
            else
                add_node!(t, placeholderWS)
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

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.Braces,CSTParser.Vect}
    t = PTree(x, nspaces(s))
    braces = CSTParser.is_lbrace(x.args[1])
    no_args = length(x.args) < 3

    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if is_opener(n) && !no_args
            add_node!(t, n, join_lines=true)
            add_node!(t, placeholder)
        elseif is_closer(n) && !no_args
            add_node!(t, placeholder)
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, n, join_lines=true)
            if braces
                add_node!(t, placeholder)
            else
                add_node!(t, placeholderWS)
            end
        else
            add_node!(t, n, join_lines=true)
        end
    end
    t
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Parameters}
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            add_node!(t, n, join_lines=true)
            add_node!(t, placeholderWS)
        else
            add_node!(t, n, join_lines=true)
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
