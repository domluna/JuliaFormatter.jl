# Creates a _prettified_ version of a CST.
#
abstract type AbstractFormatNode end
abstract type AbstractFormatLeaf <: AbstractFormatNode end
abstract type AbstractCSTLeaf <: AbstractFormatNode end

struct Identifier <: AbstractCSTLeaf end
struct Operator <: AbstractCSTLeaf end
struct Punctuation <: AbstractCSTLeaf end
struct Keyword <: AbstractCSTLeaf end
struct Literal <: AbstractCSTLeaf end
struct StringFN <: AbstractFormatNode end
struct Block <: AbstractFormatNode end
struct Module <: AbstractFormatNode end
struct Baremodule <: AbstractFormatNode end
struct Function <: AbstractFormatNode end
struct Macro <: AbstractFormatNode end
struct Struct <: AbstractFormatNode end
struct Mutable <: AbstractFormatNode end
struct Abstract <: AbstractFormatNode end
struct Primitive <: AbstractFormatNode end
struct For <: AbstractFormatNode end
struct While <: AbstractFormatNode end
struct Do <: AbstractFormatNode end
struct If <: AbstractFormatNode end
struct Try <: AbstractFormatNode end
struct Toplevel <: AbstractFormatNode end
struct Begin <: AbstractFormatNode end
struct Quote <: AbstractFormatNode end
struct Let <: AbstractFormatNode end
struct Vect <: AbstractFormatNode end
struct Comprehension <: AbstractFormatNode end
struct Braces <: AbstractFormatNode end
struct TupleFN <: AbstractFormatNode end
struct Invisbrackets <: AbstractFormatNode end
struct Curly <: AbstractFormatNode end
struct Call <: AbstractFormatNode end
struct Macrocall <: AbstractFormatNode end
struct Whereopcall <: AbstractFormatNode end
struct Conditionalopcall <: AbstractFormatNode end
struct Binaryopcall <: AbstractFormatNode end
struct Unaryopcall <: AbstractFormatNode end
struct Chainopcall <: AbstractFormatNode end
struct Colonopcall <: AbstractFormatNode end
struct Comparison <: AbstractFormatNode end
struct Kw <: AbstractFormatNode end
struct Parameters <: AbstractFormatNode end
struct Local <: AbstractFormatNode end
struct Global <: AbstractFormatNode end
struct Const <: AbstractFormatNode end
struct Return <: AbstractFormatNode end
struct Import <: AbstractFormatNode end
struct Export <: AbstractFormatNode end
struct Using <: AbstractFormatNode end
struct Row <: AbstractFormatNode end
struct Vcat <: AbstractFormatNode end
struct Typedvcat <: AbstractFormatNode end
struct Hcat <: AbstractFormatNode end
struct Typedhcat <: AbstractFormatNode end
struct RefFN <: AbstractFormatNode end
struct Generator <: AbstractFormatNode end
struct Filter <: AbstractFormatNode end
struct Flatten <: AbstractFormatNode end
struct StrFN <: AbstractFormatNode end
struct CmdFN <: AbstractFormatNode end
struct FileFN <: AbstractFormatNode end

# These types are not explicitly related to
# CSTParser.
struct Doc <: AbstractFormatNode end

struct NEWLINE <: AbstractFormatLeaf end
struct SEMICOLON <: AbstractFormatLeaf end
struct WHITESPACE <: AbstractFormatLeaf end
struct PLACEHOLDER <: AbstractFormatLeaf end
struct NOTCODE <: AbstractFormatLeaf end
struct INLINECOMMENT <: AbstractFormatLeaf end
struct TRAILINGCOMMA <: AbstractFormatLeaf end
struct TRAILINGSEMICOLON <: AbstractFormatLeaf end

# NOTE: maybe AbstractFormatNode() would suffice?
struct Other <: AbstractFormatNode end

@inline function nodetype(typ::CSTParser.Head)
    typ === CSTParser.IDENTIFIER && return Identifier()
    typ === CSTParser.OPERATOR && return Operator()
    typ === CSTParser.PUNCTUATION && return Punctuation()
    typ === CSTParser.KEYWORD && return Keyword()
    typ === CSTParser.LITERAL && return Literal()
    typ === CSTParser.StringH && return StringFN()
    typ === CSTParser.Block && return Block()
    typ === CSTParser.ModuleH && return Module()
    typ === CSTParser.BareModule && return Baremodule()
    typ === CSTParser.FunctionDef && return Function()
    typ === CSTParser.Macro && return Macro()
    typ === CSTParser.Struct && return Struct()
    typ === CSTParser.Mutable && return Mutable()
    typ === CSTParser.Abstract && return Abstract()
    typ === CSTParser.Primitive && return Primitive()
    typ === CSTParser.For && return For()
    typ === CSTParser.While && return While()
    typ === CSTParser.Do && return Do()
    typ === CSTParser.If && return If()
    typ === CSTParser.Try && return Try()
    typ === CSTParser.TopLevel && return Toplevel()
    typ === CSTParser.Begin && return Begin()
    typ === CSTParser.Quote && return Quote()
    typ === CSTParser.Let && return Let()
    typ === CSTParser.Vect && return Vect()
    typ === CSTParser.Comprehension && return Comprehension()
    typ === CSTParser.Braces && return Braces()
    typ === CSTParser.TupleH && return TupleFN()
    typ === CSTParser.InvisBrackets && return Invisbrackets()
    typ === CSTParser.Curly && return Curly()
    typ === CSTParser.Call && return Call()
    typ === CSTParser.MacroCall && return Macrocall()
    typ === CSTParser.WhereOpCall && return Whereopcall()
    typ === CSTParser.ConditionalOpCall && return Conditionalopcall()
    typ === CSTParser.BinaryOpCall && return Binaryopcall()
    typ === CSTParser.UnaryOpCall && return Unaryopcall()
    typ === CSTParser.ChainOpCall && return Chainopcall()
    typ === CSTParser.ColonOpCall && return Colonopcall()
    typ === CSTParser.Comparison && return Comparison()
    typ === CSTParser.Kw && return Kw()
    typ === CSTParser.Parameters && return Parameters()
    typ === CSTParser.Local && return Local()
    typ === CSTParser.Global && return Global()
    typ === CSTParser.Const && return Const()
    typ === CSTParser.Return && return Return()
    typ === CSTParser.Import && return Import()
    typ === CSTParser.Export && return Export()
    typ === CSTParser.Using && return Using()
    typ === CSTParser.Row && return Row()
    typ === CSTParser.Vcat && return Vcat()
    typ === CSTParser.TypedVcat && return Typedvcat()
    typ === CSTParser.Hcat && return Hcat()
    typ === CSTParser.TypedHcat && return Typedhcat()
    typ === CSTParser.Ref && return RefFN()
    typ === CSTParser.Generator && return Generator()
    typ === CSTParser.Filter && return Filter()
    typ === CSTParser.Flatten && return Flatten()
    typ === CSTParser.x_Str && return StrFN()
    typ === CSTParser.x_Cmd && return CmdFN()
    typ === CSTParser.FileH && return FileFN()
    return Other()
end
@inline nodetype(cst::CSTParser.EXPR) = nodetype(CSTParser.typof(cst))

# Formatted Syntax Tree
mutable struct FST{T<:AbstractFormatNode}
    startline::Int
    endline::Int
    indent::Int
    len::Int
    val::Union{Nothing,AbstractString}
    nodes::Union{Nothing,Vector{FST}}
    ref::Union{Nothing,Ref{CSTParser.EXPR}}
    force_nest::Bool
    extra_margin::Int
end

FST{T}(cst::CSTParser.EXPR, indent::Integer) where {T<:AbstractFormatNode} =
    FST{T}(-1, -1, indent, 0, nothing, FST[], Ref(cst), false, 0)

function FST{T}(
    cst::CSTParser.EXPR,
    startline::Integer,
    endline::Integer,
    val::AbstractString,
) where {T<:AbstractFormatNode}
    FST{T}(startline, endline, 0, length(val), val, nothing, Ref(cst), false, 0)
end

function FST{T}(
    startline::Integer,
    endline::Integer,
    val::AbstractString,
) where {T<:Union{AbstractFormatNode,AbstractCSTLeaf}}
    FST{T}(startline, endline, 0, length(val), val, nothing, nothing, false, 0)
end

Base.setindex!(fst::FST, node::FST, ind::Int) = fst.nodes[ind] = node
Base.getindex(fst::FST, inds...) = fst.nodes[inds...]
Base.lastindex(fst::FST) = length(fst.nodes)
Base.eltype(::FST{T}) where {T<:AbstractFormatNode} = T

Newline(; length = 0, force_nest = false) =
    FST{NEWLINE}(-1, -1, 0, length, "\n", nothing, nothing, force_nest, 0)
Semicolon() = FST{SEMICOLON}(-1, -1, 0, 1, ";", nothing, nothing, false, 0)
TrailingComma() = FST{TRAILINGCOMMA}(-1, -1, 0, 0, "", nothing, nothing, false, 0)
TrailingSemicolon() = FST{TRAILINGSEMICOLON}(-1, -1, 0, 1, ";", nothing, nothing, false, 0)
Whitespace(n) = FST{WHITESPACE}(-1, -1, 0, n, " "^n, nothing, nothing, false, 0)
Placeholder(n) = FST{PLACEHOLDER}(-1, -1, 0, n, " "^n, nothing, nothing, false, 0)
Notcode(startline, endline) =
    FST{NOTCODE}(startline, endline, 0, 0, "", nothing, nothing, false, 0)
InlineComment(line) = FST{INLINECOMMENT}(line, line, 0, 0, "", nothing, nothing, false, 0)

Base.length(fst::FST) = fst.len

@inline is_placeholder(::FST) = false
@inline is_placeholder(::FST{PLACEHOLDER}) = true

@inline is_leaf(cst::CSTParser.EXPR) = cst.args === nothing
@inline is_leaf(fst::FST) = fst.nodes === nothing
@inline empty_start(fst::FST) = fst.startline == 1 && fst.endline == 1 && fst.val == ""

is_punc(cst::CSTParser.EXPR) = CSTParser.ispunctuation(cst)

@inline is_end(::FST) = false
@inline is_end(cst::CSTParser.EXPR) = cst.typ === CSTParser.KEYWORD && cst.val == "end"
@inline is_end(fst::FST{Keyword}) = fst.val == "end"

@inline is_colon(::FST) = false
@inline is_colon(cst::CSTParser.EXPR) = cst.typ === CSTParser.OPERATOR && cst.val == ":"
@inline is_colon(fst::FST{Operator}) = fst.val == ":"

@inline is_comma(::FST) = false
@inline is_comma(fst::FST{Punctuation}) = fst.val == ","
@inline is_comma(::FST{TRAILINGCOMMA}) = true

@inline is_comment(::FST) = false
@inline is_comment(::FST{INLINECOMMENT}) = true
@inline is_comment(::FST{NOTCODE}) = true

@inline is_colon_op(cst::CSTParser.EXPR) =
    (cst.typ === CSTParser.BinaryOpCall && cst[2].kind === Tokens.COLON) ||
    cst.typ === CSTParser.ColonOpCall

@inline is_lazy_op(cst::CSTParser.EXPR) =
    cst.typ === CSTParser.BinaryOpCall &&
    (cst[2].kind === Tokens.LAZY_OR || cst[2].kind === Tokens.LAZY_AND)

@inline is_multiline(::FST) = false
@inline is_multiline(::FST{StringFN}) = true
@inline is_multiline(fst::FST{StrFN}) = fst[2] isa FST{StringFN}
@inline is_multiline(fst::FST{CmdFN}) = fst[2] isa FST{StringFN}
@inline is_multiline(fst::FST{Vcat}) = fst.endline > fst.startline
@inline is_multiline(fst::FST{Typedvcat}) = fst.endline > fst.startline

@inline is_closer(::FST) = false
@inline is_closer(fst::FST{Punctuation}) =
    fst.val == "}" || fst.val == ")" || fst.val == "]"
@inline is_closer(cst::CSTParser.EXPR) =
    cst.kind === Tokens.RBRACE || cst.kind === Tokens.RPAREN || cst.kind === Tokens.RSQUARE

@inline is_opener(::FST) = false
@inline is_opener(fst::FST{Punctuation}) =
    fst.val == "{" || fst.val == "(" || fst.val == "["
@inline is_opener(cst::CSTParser.EXPR) =
    cst.kind === Tokens.LBRACE || cst.kind === Tokens.LPAREN || cst.kind === Tokens.LSQUARE

# f a function which returns a bool
function parent_is(cst::CSTParser.EXPR, f; ignore_typs = [])
    p = cst.parent
    p === nothing && return false
    while p !== nothing && p.typ in ignore_typs
        p = p.parent
    end
    f(p)
end

function contains_comment(fst::FST)
    is_leaf(fst) && return false
    findfirst(is_comment, fst.nodes) !== nothing
end

# TODO: Remove once this is fixed in CSTParser.
# https://github.com/julia-vscode/CSTParser.jl/issues/108
function get_args(cst::CSTParser.EXPR)
    if cst.typ === CSTParser.MacroCall || cst.typ === CSTParser.TypedVcat ||
       cst.typ === CSTParser.Ref || cst.typ === CSTParser.Curly
        return get_args(cst.args[2:end])
    elseif cst.typ === CSTParser.Parameters || cst.typ === CSTParser.Braces ||
           cst.typ === CSTParser.Vcat || cst.typ === CSTParser.TupleH ||
           cst.typ === CSTParser.Vect || cst.typ === CSTParser.InvisBrackets
        return get_args(cst.args)
    end
    CSTParser.get_args(cst)
end

function get_args(args::Vector{CSTParser.EXPR})
    args0 = CSTParser.EXPR[]
    for arg in args
        CSTParser.ispunctuation(arg) && continue
        if CSTParser.typof(arg) === CSTParser.Parameters
            for j = 1:length(arg.args)
                parg = arg[j]
                CSTParser.ispunctuation(parg) && continue
                push!(args0, parg)
            end
        else
            push!(args0, arg)
        end
    end
    args0
end

n_args(x) = length(get_args(x))

# function add_node!(t::FST, n::FST{T}, ::State) where T <: AbstractFormatLeaf
#         t.len += length(n)
#         n.startline = t.startline
#         n.endline = t.endline
#         push!(t.nodes, n)
# end
function add_node!(t::FST, n::FST{NOTCODE}, s::State)
    n.indent = s.indent
    push!(t.nodes, n)
end
add_node!(t::FST, n::FST{INLINECOMMENT}, ::State) = push!(t.nodes, n)

function add_node!(t::FST, n::FST{TRAILINGCOMMA}, s::State)
    en = t.nodes[end]
    if en isa FST{Generator} || en isa FST{Filter} || en isa FST{Flatten} ||
       en isa FST{Macrocall} || (is_comma(en) && t isa FST{TupleFN} && n_args(t.ref[]) == 1)
        # don't insert trailing comma in these cases
    elseif is_comma(en)
        t.nodes[end] = n
        t.len -= 1
    else
        t.len += length(n)
        n.startline = t.startline
        n.endline = t.endline
        push!(t.nodes, n)
    end
end

function add_node!(t::FST, n::FST, s::State; join_lines = false, max_padding = -1)
    if n isa FST{SEMICOLON}
        join_lines = true
        loc = s.offset > length(s.doc.text) && t isa FST{Toplevel} ?
            cursor_loc(s, s.offset - 1) : cursor_loc(s)
        for l = t.endline:loc[1]
            if has_semicolon(s.doc, l)
                n.startline = l
                n.endline = l
                break
            end
        end
        # @info "" t.endline n.endline loc[1]

        # If there's no semicolon, treat it
        # as a leaf
        if n.startline == -1
            t.len += length(n)
            n.startline = t.startline
            n.endline = t.endline
            push!(t.nodes, n)
            return
        end
    elseif n isa FST{Block} && length(n) == 0
        push!(t.nodes, n)
        return
    elseif n isa FST{Parameters}
        if n_args(t.ref[]) == n_args(n.ref[])
            # There are no arguments prior to params
            # so we can remove the initial placeholder.
            idx = findfirst(n -> n isa FST{PLACEHOLDER}, t.nodes)
            idx !== nothing && deleteat!(t.nodes, idx)
        end
        add_node!(t, Semicolon(), s)
        if length(n.nodes) > 0
            multi_arg = n_args(t.ref[]) > 0
            multi_arg ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
        end
    elseif eltype(n) <: AbstractFormatLeaf
        t.len += length(n)
        n.startline = t.startline
        n.endline = t.endline
        push!(t.nodes, n)
        return
    end

    if length(t.nodes) == 0
        t.startline = n.startline
        t.endline = n.endline
        t.len += length(n)
        push!(t.nodes, n)
        return
    end

    if !is_prev_newline(t.nodes[end])
        current_line = t.endline
        notcode_startline = current_line + 1
        notcode_endline = n.startline - 1
        lastnode = t.nodes[end]

        if notcode_startline <= notcode_endline
            # If there are comments in between node elements
            # nesting is forced in an effort to preserve them.
            t.force_nest = true

            # If the previous node type is WHITESPACE - reset it.
            # This fixes cases similar to the one shown in issue #51.
            lastnode isa FST{WHITESPACE} && (t.nodes[end] = Whitespace(0))

            hs = hascomment(s.doc, current_line)
            hs && add_node!(t, InlineComment(current_line), s)
            if !(lastnode isa FST{PLACEHOLDER})
                add_node!(t, Newline(force_nest = true), s)
            elseif hs && lastnode isa FST{PLACEHOLDER}
                # swap PLACEHOLDER (will be NEWLINE) with INLINECOMMENT node
                idx = length(t.nodes)
                t.nodes[idx-1], t.nodes[idx] = t.nodes[idx], t.nodes[idx-1]
            end
            add_node!(t, Notcode(notcode_startline, notcode_endline), s)
            add_node!(t, Newline(force_nest = true), s)
        elseif !join_lines
            if hascomment(s.doc, current_line) && current_line != n.startline
                add_node!(t, InlineComment(current_line), s)
            end
            add_node!(t, Newline(force_nest = true), s)
        elseif lastnode isa FST{PLACEHOLDER} &&
               current_line != n.startline && hascomment(s.doc, current_line)
            t.force_nest = true
            add_node!(t, InlineComment(current_line), s)
            # swap PLACEHOLDER (will be NEWLINE) with INLINECOMMENT node
            idx = length(t.nodes)
            t.nodes[idx-1], t.nodes[idx] = t.nodes[idx], t.nodes[idx-1]
        end

        if n isa FST{Parameters} && n.force_nest
            t.force_nest = true
        end
    end

    if n.startline < t.startline || t.startline == -1
        t.startline = n.startline
    end
    if n.endline > t.endline || t.endline == -1
        t.endline = n.endline
    end

    if !join_lines && is_end(n)
        # end keyword isn't useful w.r.t margin lengths
    elseif t isa FST{StringFN}
        # @info "insert literal into stringh" length(n) n n.indent + length(n) - t.indent t.indent n.indent

        # The length of this node is the length of
        # the longest string. The length of the string is
        # only considered "in the positive" when it's past
        # the hits the initial """ offset, i.e. `t.indent`.
        t.len = max(t.len, n.indent + length(n) - t.indent)
    elseif is_multiline(n)
        is_iterable(t) && n_args(t.ref[]) > 1 && (t.force_nest = true)
        t.len += length(n)
    elseif max_padding >= 0
        t.len = max(t.len, length(n) + max_padding)
    else
        t.len += length(n)
    end
    push!(t.nodes, n)
    nothing
end

is_prev_newline(::FST{NEWLINE}) = true
function is_prev_newline(fst::FST)
    if is_leaf(fst) || length(fst.nodes) == 0
        return false
    end
    is_prev_newline(fst[end])
end

"""
    `length_to(x::FST, ntyps; start::Int = 1)`

Returns the length to any node type in `ntyps` based off the `start` index.
"""
function length_to(fst::FST, ntyps::Vector; start::Int = 1)
    eltype(fst) in ntyps && return 0, true
    is_leaf(fst) && return length(fst), false
    len = 0
    for i = start:length(fst.nodes)
        l, found = length_to(fst.nodes[i], ntyps)
        len += l
        found && return len, found
    end
    return len, false
end


pretty(style::DefaultStyle, cst::CSTParser.EXPR, s::State; kwargs...) =
    pretty(style, nodetype(cst), cst, s; kwargs...)
function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:AbstractFormatNode}
    t = FST{T}(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, join_lines = true, max_padding = -1)
    end
    t
end

function pretty(style::DefaultStyle, ::FileFN, cst::CSTParser.EXPR, s::State)
    t = FST{FileFN}(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, join_lines = false, max_padding = 0)
    end
    t
end


function pretty(style::DefaultStyle, ::Identifier, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    loc = cursor_loc(s)
    s.offset += cst.fullspan
    FST{Identifier}(cst, loc[1], loc[1], cst.val)
end

function pretty(style::DefaultStyle, ::Operator, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    loc = cursor_loc(s)
    val = string(CSTParser.Expr(cst))
    s.offset += cst.fullspan
    FST{Operator}(cst, loc[1], loc[1], val)
end

function pretty(style::DefaultStyle, ::Keyword, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    loc = cursor_loc(s)
    val = cst.kind === Tokens.ABSTRACT ? "abstract" :
        cst.kind === Tokens.BAREMODULE ? "baremodule" :
        cst.kind === Tokens.BEGIN ? "begin" :
        cst.kind === Tokens.BREAK ? "break" :
        cst.kind === Tokens.CATCH ? "catch" :
        cst.kind === Tokens.CONST ? "const" :
        cst.kind === Tokens.CONTINUE ? "continue" :
        cst.kind === Tokens.NEW ? "new" :
        cst.kind === Tokens.DO ? "do" :
        cst.kind === Tokens.IF ? "if" :
        cst.kind === Tokens.ELSEIF ? "elseif" :
        cst.kind === Tokens.ELSE ? "else" :
        cst.kind === Tokens.END ? "end" :
        cst.kind === Tokens.EXPORT ? "export" :
        cst.kind === Tokens.FINALLY ? "finally" :
        cst.kind === Tokens.FOR ? "for" :
        cst.kind === Tokens.FUNCTION ? "function" :
        cst.kind === Tokens.GLOBAL ? "global" :
        cst.kind === Tokens.IMPORT ? "import" :
        cst.kind === Tokens.LET ? "let" :
        cst.kind === Tokens.LOCAL ? "local" :
        cst.kind === Tokens.MACRO ? "macro" :
        cst.kind === Tokens.MODULE ? "module" :
        cst.kind === Tokens.MUTABLE ? "mutable" :
        cst.kind === Tokens.OUTER ? "outer " :
        cst.kind === Tokens.PRIMITIVE ? "primitive" :
        cst.kind === Tokens.QUOTE ? "quote" :
        cst.kind === Tokens.RETURN ? "return" :
        cst.kind === Tokens.STRUCT ? "struct" :
        cst.kind === Tokens.TYPE ? "type" :
        cst.kind === Tokens.TRY ? "try" :
        cst.kind === Tokens.USING ? "using" : cst.kind === Tokens.WHILE ? "while" : ""
    s.offset += cst.fullspan
    FST{Keyword}(cst, loc[1], loc[1], val)
end

function pretty(style::DefaultStyle, ::Punctuation, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    loc = cursor_loc(s)
    val = cst.kind === Tokens.LPAREN ? "(" :
        cst.kind === Tokens.LBRACE ? "{" :
        cst.kind === Tokens.LSQUARE ? "[" :
        cst.kind === Tokens.RPAREN ? ")" :
        cst.kind === Tokens.RBRACE ? "}" :
        cst.kind === Tokens.RSQUARE ? "]" :
        cst.kind === Tokens.COMMA ? "," :
        cst.kind === Tokens.SEMICOLON ? ";" :
        cst.kind === Tokens.AT_SIGN ? "@" : cst.kind === Tokens.DOT ? "." : ""
    s.offset += cst.fullspan
    FST{Punctuation}(cst, loc[1], loc[1], val)
end

function pretty(style::DefaultStyle, ::Literal, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    loc = cursor_loc(s)
    if !is_str_or_cmd(cst.kind)
        val = cst.val
        if cst.kind === Tokens.FLOAT && cst.val[end] == '.'
            # If a floating point ends in `.`, add trailing zero.
            val *= '0'
        elseif cst.kind === Tokens.FLOAT && cst.val[1] == '.'
            val = '0' * val
        end
        s.offset += cst.fullspan
        return FST{Literal}(cst, loc[1], loc[1], val)
    end

    # Strings are unescaped by CSTParser
    # to mimic Meta.parse which makes reproducing
    # the correct output from the LITERAL value problematic.
    # So we'll just look at the source directly!
    str_info = get(s.doc.lit_strings, s.offset - 1, nothing)

    # Tokenize treats the `ix` part of r"^(=?[^=]+)=(.*)$"ix as an
    # IDENTIFIER where as CSTParser parses it as a LITERAL.
    # An IDENTIFIER won't show up in the string literal lookup table.
    if str_info === nothing &&
       (cst.parent.typ === CSTParser.x_Str || cst.parent.typ === CSTParser.x_Cmd)
        s.offset += cst.fullspan
        return FST{Literal}(cst, loc[1], loc[1], cst.val)
    end

    startline, endline, str = str_info
    # @debug "" loc startline endline str

    s.offset += cst.fullspan

    lines = split(str, "\n")
    length(lines) == 1 && return FST{Literal}(
        loc[1],
        loc[1],
        0,
        length(lines[1]),
        lines[1],
        nothing,
        Ref(cst),
        false,
        0,
    )

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    # @debug "" lines cst.val loc loc[2] sidx

    t = FST{StringFN}(-1, -1, loc[2] - 1, 0, nothing, FST[], Ref(cst), false, 0)
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        tt = FST{Literal}(ln, ln, sidx - 1, length(l), l, nothing, nothing, false, 0)
        add_node!(t, tt, s)
    end
    t
end

function pretty(style::DefaultStyle, ::StringFN, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    loc = cursor_loc(s)
    startline, endline, str = s.doc.lit_strings[s.offset-1]

    s.offset += cst.fullspan

    lines = split(str, "\n")

    length(lines) == 1 && return FST{Literal}(
        startline,
        endline,
        0,
        length(lines[1]),
        lines[1],
        nothing,
        Ref(cst),
        false,
        0,
    )

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    # @debug "" lines cst.val loc loc[2] sidx

    t = FST{StringFN}(cst, loc[2] - 1)
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        tt = FST{Literal}(ln, ln, sidx - 1, length(l), l, nothing, nothing, false, 0)
        add_node!(t, tt, s)
    end
    t
end

function pretty(style::DefaultStyle, ::Doc, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Doc}(cst, nspaces(s))
    if cst[1].typ === CSTParser.GlobalRefDoc
        # cst[1] is empty and fullspan is 0 so we can skip it
        if cst[2].typ === CSTParser.LITERAL
            add_node!(t, pretty(style, cst[2], s), s, max_padding = 0)
        elseif cst[2].typ == CSTParser.StringH
            add_node!(t, pretty(style, cst[2], s), s)
        end
        add_node!(t, pretty(style, cst[3], s), s, max_padding = 0)
    elseif length(cst) == 3 &&
           cst[1].typ === CSTParser.MacroName && cst[1][2].val == "doc" && is_str(cst[2])
        add_node!(t, pretty(style, cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
        n = pretty(style, cst[3], s)
        join_lines = t.endline == n.startline
        join_lines && add_node!(t, Whitespace(1), s)
        add_node!(t, n, s, join_lines = join_lines, max_padding = 0)
    end
    return t
end

function pretty(style::DefaultStyle, ::Macrocall, cst::CSTParser.EXPR, s::State)
    t = FST{Macrocall}(cst, nspaces(s))
    doc = pretty(style, Doc(), cst, s)
    if length(doc) > 0
        add_node!(t, doc, s)
        return t
    end

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))
    has_closer = is_closer(cst.args[end])

    # @info "" has_closer
    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if a.typ === CSTParser.MacroName
            if a.fullspan - a.span > 0 && length(cst) > 1
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            else
                # assumes the next argument is a brace of some sort
                add_node!(t, n, s, join_lines = true)
            end
        elseif is_opener(n) && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(n) && nest
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif a.fullspan - a.span > 0
            if has_closer
                add_node!(t, n, s, join_lines = true)
                if cst[i+1].typ !== CSTParser.Parameters && i < length(cst) - 1
                    add_node!(t, Whitespace(1), s)
                end
            else
                add_node!(t, n, s, join_lines = true, max_padding = 0)
                i < length(cst) && add_node!(t, Whitespace(1), s)
            end
        else
            if has_closer
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, n, s, join_lines = true, max_padding = 0)
            end
        end
    end
    t
end

# Block
# length of Block is the length of the longest expr
function pretty(
    style::DefaultStyle,
    ::Block,
    cst::CSTParser.EXPR,
    s::State;
    ignore_single_line = false,
    from_quote = false,
    join_body = false,
)
    style = getstyle(style)
    t = FST{Block}(cst, nspaces(s))
    single_line = ignore_single_line ? false :
        cursor_loc(s)[1] == cursor_loc(s, s.offset + cst.span - 1)[1]

    # @info "" from_quote single_line ignore_single_line join_body
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if from_quote && !single_line
            if i == 1 || CSTParser.is_comma(a)
                add_node!(t, n, s, join_lines = true)
            elseif CSTParser.is_comma(cst[i-1])
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, Semicolon(), s)
                add_node!(t, n, s, max_padding = 0)
            end
        elseif single_line
            if i == 1 || CSTParser.is_comma(a)
                add_node!(t, n, s, join_lines = true)
            elseif CSTParser.is_comma(cst[i-1])
                add_node!(t, Placeholder(1), s)
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, Semicolon(), s)
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            end
        else
            if i < length(cst) && CSTParser.is_comma(a) && is_punc(cst[i+1])
                add_node!(t, n, s, join_lines = true)
            elseif CSTParser.is_comma(a) && i != length(cst)
                add_node!(t, n, s, join_lines = true)
                join_body && add_node!(t, Placeholder(1), s)
            elseif join_body
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, n, s, max_padding = 0)
            end
        end
    end
    t
end

function pretty(style::DefaultStyle, ::Abstract, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Abstract}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    t
end

function pretty(style::DefaultStyle, ::Primitive, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Primitive}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[5], s), s, join_lines = true)
    t
end

# FunctionDef/Macro
function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Function,Macro}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if length(cst) > 3
        if cst[3].fullspan == 0
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
        else
            s.indent += s.indent_size
            add_node!(
                t,
                pretty(style, cst[3], s, ignore_single_line = true),
                s,
                max_padding = s.indent_size,
            )
            s.indent -= s.indent_size
            add_node!(t, pretty(style, cst[4], s), s)
        end
    else
        # function stub, i.e. "function foo end"
        # this should be on one line
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    end
    t
end

function pretty(style::DefaultStyle, ::Struct, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Struct}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if cst[3].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(style, cst[4], s), s)
    end
    t
end

function pretty(style::DefaultStyle, ::Mutable, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Mutable}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    if cst[4].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[5], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[4], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(style, cst[5], s), s)
    end
    t
end

function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Module,Baremodule}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if cst[3].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    else
        add_node!(t, pretty(style, cst[3], s), s, max_padding = 0)
        add_node!(t, pretty(style, cst[4], s), s)
    end
    t
end

# Const/Local/Global/Return
function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Const,Local,Global,Return}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if cst[2].fullspan != 0
        for a in cst.args[2:end]
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

function pretty(style::DefaultStyle, ::Toplevel, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Toplevel}(cst, nspaces(s))
    for a in cst.args
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, max_padding = s.indent_size)
        add_node!(t, Semicolon(), s)
    end
    t
end

function pretty(style::DefaultStyle, ::Begin, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Begin}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if cst[2].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[2], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(style, cst[3], s), s)
    end
    t
end

# Quote
function pretty(style::DefaultStyle, ::Quote, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Quote}(cst, nspaces(s))
    if cst[1].typ === CSTParser.KEYWORD && cst[1].kind === Tokens.QUOTE
        add_node!(t, pretty(style, cst[1], s), s)
        if cst[2].fullspan == 0
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
        else
            s.indent += s.indent_size
            add_node!(
                t,
                pretty(style, cst[2], s, ignore_single_line = true),
                s,
                max_padding = s.indent_size,
            )
            s.indent -= s.indent_size
            add_node!(t, pretty(style, cst[3], s), s)
        end
    else
        for a in cst.args
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

# Let
#
# two forms:
#
# let var1 = value1, var2
#     body
# end
#
# y, back = let
#     body
# end
function pretty(style::DefaultStyle, ::Let, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Let}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if length(cst.args) > 3
        add_node!(t, Whitespace(1), s)
        s.indent += s.indent_size
        if cst[2].typ === CSTParser.Block
            add_node!(t, pretty(style, cst[2], s, join_body = true), s, join_lines = true)
        else
            add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
        end
        s.indent -= s.indent_size

        idx = length(t.nodes)
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        # Possible newline after args if nested to act as a separator
        # to the block body.
        if cst[2].typ === CSTParser.Block && !(t.nodes[end-2] isa FST{NOTCODE})
            add_node!(t.nodes[idx], Placeholder(0), s)
        end
        add_node!(t, pretty(style, cst.args[end], s), s)
    else
        s.indent += s.indent_size
        add_node!(t, pretty(style, cst[2], s, ignore_single_line = true), s)
        s.indent -= s.indent_size
        add_node!(t, pretty(style, cst.args[end], s), s)
    end
    t
end



# Transforms
#
# for i = iter body end
#
# =>
#
# for i in iter body end
#
# AND
#
# for i in 1:10 body end
#
# =>
#
# for i = 1:10 body end
#
# https://github.com/domluna/JuliaFormatter.jl/issues/34
function eq_to_in_normalization!(cst::CSTParser.EXPR, always_for_in::Bool)
    if cst.typ === CSTParser.BinaryOpCall
        op = cst[2]
        rhs = cst[3]

        if always_for_in
            cst[2].kind = Tokens.IN
            return
        end

        if op.kind === Tokens.EQ && !is_colon_op(rhs)
            cst[2].kind = Tokens.IN
        elseif op.kind === Tokens.IN && is_colon_op(rhs)
            cst[2].kind = Tokens.EQ
        end
    elseif cst.typ === CSTParser.Block || cst.typ === CSTParser.InvisBrackets
        for a in cst
            eq_to_in_normalization!(a, always_for_in)
        end
    end
end

# For/While
function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{For,While}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    if cst[1].kind === Tokens.FOR
        eq_to_in_normalization!(cst[2], s.opts.always_for_in)
    end
    if cst[2].typ === CSTParser.Block
        s.indent += s.indent_size
        add_node!(t, pretty(style, cst[2], s, join_body = true), s, join_lines = true)
        s.indent -= s.indent_size
    else
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    end
    idx = length(t.nodes)
    s.indent += s.indent_size
    add_node!(
        t,
        pretty(style, cst[3], s, ignore_single_line = true),
        s,
        max_padding = s.indent_size,
    )
    s.indent -= s.indent_size

    # Possible newline after args if nested to act as a separator
    # to the block body.
    if cst[2].typ === CSTParser.Block && !(t.nodes[end-2] isa FST{NOTCODE})
        add_node!(t.nodes[idx], Placeholder(0), s)
    end
    add_node!(t, pretty(style, cst[4], s), s)
    t
end

# Do
function pretty(style::DefaultStyle, ::Do, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Do}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if cst[3].fullspan != 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    end
    if cst[4].typ === CSTParser.Block
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[4], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
    end
    add_node!(t, pretty(style, cst.args[end], s), s)
    t
end

# Try
function pretty(style::DefaultStyle, ::Try, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Try}(cst, nspaces(s))
    for a in cst.args
        if a.fullspan == 0 && a.typ !== CSTParser.Block
        elseif a.typ === CSTParser.KEYWORD
            add_node!(t, pretty(style, a, s), s, max_padding = 0)
        elseif a.typ === CSTParser.Block
            s.indent += s.indent_size
            add_node!(
                t,
                pretty(style, a, s, ignore_single_line = true),
                s,
                max_padding = s.indent_size,
            )
            s.indent -= s.indent_size
        else
            len = length(t)
            add_node!(t, Whitespace(1), s)
            n = pretty(style, a, s)
            # "catch n"
            t.len = max(len, 5 + 1 + length(n))
            add_node!(t, n, s, join_lines = true, max_padding = 0)
        end
    end
    t
end

function pretty(style::DefaultStyle, ::If, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{If}(cst, nspaces(s))
    if cst[1].typ === CSTParser.KEYWORD && cst[1].kind === Tokens.IF
        add_node!(t, pretty(style, cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size

        len = length(t)
        if length(cst.args) > 4
            add_node!(t, pretty(style, cst[4], s), s, max_padding = 0)
            if cst[4].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1), s)
                n = pretty(style, cst[5], s)
                add_node!(t, n, s, join_lines = true)
                # "elseif n"
                t.len = max(len, length(n))
            else
                # ELSE KEYWORD
                s.indent += s.indent_size
                add_node!(
                    t,
                    pretty(style, cst[5], s, ignore_single_line = true),
                    s,
                    max_padding = s.indent_size,
                )
                s.indent -= s.indent_size
            end
        end
        # END KEYWORD
        add_node!(t, pretty(style, cst.args[end], s), s)
    else
        # "cond" part of "elseif cond"
        t.len += 7
        add_node!(t, pretty(style, cst[1], s), s)

        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[2], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size

        len = length(t)
        if length(cst.args) > 2
            # this either else or elseif keyword
            add_node!(t, pretty(style, cst[3], s), s, max_padding = 0)

            if cst[3].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1), s)
                n = pretty(style, cst[4], s)
                add_node!(t, n, s, join_lines = true)
                # "elseif n"
                t.len = max(len, length(n))
            else
                s.indent += s.indent_size
                add_node!(
                    t,
                    pretty(style, cst[4], s, ignore_single_line = true),
                    s,
                    max_padding = s.indent_size,
                )
                s.indent -= s.indent_size
            end
        end
    end
    t
end

# ChainOpCall/Comparison
function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
) where {T<:Union{Comparison,Chainopcall}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    nws = nospace ? 0 : 1
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if a.typ === CSTParser.OPERATOR
            !nospace && add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
            if nonest
                add_node!(t, Whitespace(nws), s)
            else
                add_node!(t, Placeholder(nws), s)
            end
        elseif i == length(cst) - 1 && is_punc(a) && is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

# ColonOpCall
function pretty(style::DefaultStyle, ::Colonopcall, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Colonopcall}(cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices
    for a in cst
        if a.typ === CSTParser.BinaryOpCall
            n = pretty(style, a, s, nonest = true, nospace = nospace)
        elseif a.typ === CSTParser.InvisBrackets
            n = pretty(style, a, s, nonest = true, nospace = nospace)
        elseif a.typ === CSTParser.ChainOpCall || a.typ === CSTParser.Comparison
            n = pretty(style, a, s, nonest = true, nospace = nospace)
        else
            n = pretty(style, a, s)
        end

        if s.opts.whitespace_ops_in_indices && !is_leaf(n) && !is_iterable(n)
            paren = FST{Punctuation}(n.startline, n.startline, "(")
            add_node!(t, paren, s, join_lines = true)
            add_node!(t, n, s, join_lines = true)
            paren = FST{Punctuation}(n.startline, n.startline, ")")
            add_node!(t, paren, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

# Kw
function pretty(style::DefaultStyle, ::Kw, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Kw}(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.EQ
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

is_str(cst::CSTParser.EXPR) = is_str_or_cmd(cst.kind) || is_str_or_cmd(cst.typ)

function is_iterable(cst::CSTParser.EXPR)
    cst.typ === CSTParser.TupleH && return true
    cst.typ === CSTParser.Vect && return true
    cst.typ === CSTParser.Vcat && return true
    cst.typ === CSTParser.Braces && return true
    cst.typ === CSTParser.Call && return true
    cst.typ === CSTParser.Curly && return true
    cst.typ === CSTParser.Comprehension && return true
    cst.typ === CSTParser.MacroCall && return true
    cst.typ === CSTParser.InvisBrackets && return true
    cst.typ === CSTParser.Ref && return true
    cst.typ === CSTParser.TypedVcat && return true
    return false
end
is_iterable(::FST) = false
is_iterable(::FST{TupleFN}) = true
is_iterable(::FST{Vect}) = true
is_iterable(::FST{Vcat}) = true
is_iterable(::FST{Braces}) = true
is_iterable(::FST{Call}) = true
is_iterable(::FST{Curly}) = true
is_iterable(::FST{Comprehension}) = true
is_iterable(::FST{Macrocall}) = true
is_iterable(::FST{Invisbrackets}) = true
is_iterable(::FST{RefFN}) = true
is_iterable(::FST{Typedvcat}) = true

is_block(cst::CSTParser.EXPR) =
    cst.typ === CSTParser.If || cst.typ === CSTParser.Do || cst.typ === CSTParser.Try ||
    cst.typ === CSTParser.For || cst.typ === CSTParser.While || cst.typ === CSTParser.Let

nest_assignment(cst::CSTParser.EXPR) = CSTParser.precedence(cst[2].kind) == 1

unnestable_arg(cst::CSTParser.EXPR) =
    is_iterable(cst) || is_str(cst) || cst.typ === CSTParser.LITERAL ||
    (cst.typ === CSTParser.BinaryOpCall && cst[2].kind === Tokens.DOT)

function nest_rhs(cst::CSTParser.EXPR)::Bool
    if CSTParser.defines_function(cst)
        rhs = cst[3]
        rhs.typ === CSTParser.Block && (rhs = rhs[1])
        return is_block(rhs)
    end
    false
end

function nestable(::DefaultStyle, cst::CSTParser.EXPR)
    CSTParser.defines_function(cst) && cst[1].typ !== CSTParser.UnaryOpCall && return true
    nest_assignment(cst) && return !is_str(cst[3])
    true
end

# BinaryOpCall
function pretty(
    style::DefaultStyle,
    ::Binaryopcall,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(style)
    t = FST{Binaryopcall}(cst, nspaces(s))
    op = cst[2]
    nonest = nonest || op.kind === Tokens.COLON
    if cst.parent.typ === CSTParser.Curly &&
       op.kind in (Tokens.ISSUBTYPE, Tokens.ISSUPERTYPE) && !s.opts.whitespace_typedefs
        nospace = true
    elseif op.kind === Tokens.COLON
        nospace = true
    end
    nospace_args = s.opts.whitespace_ops_in_indices ? false : nospace

    if cst[1].typ === CSTParser.BinaryOpCall
        n = pretty(
            style,
            Binaryopcall(),
            cst[1],
            s,
            nonest = nonest,
            nospace = nospace_args,
        )
    elseif cst[1].typ === CSTParser.InvisBrackets
        n = pretty(
            style,
            Invisbrackets(),
            cst[1],
            s,
            nonest = nonest,
            nospace = nospace_args,
        )
    elseif cst[1].typ === CSTParser.ChainOpCall || cst[1].typ === CSTParser.Comparison
        n = pretty(style, Chainopcall(), cst[1], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[1], s)
    end
    # n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices && !is_leaf(cst[1]) && !is_iterable(cst[1])
        paren = FST{Punctuation}(n.startline, n.startline, "(")
        add_node!(t, paren, s)
        add_node!(t, n, s, join_lines = true)
        paren = FST{Punctuation}(n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s)
    end

    nrhs = nest_rhs(cst)
    nrhs && (t.force_nest = true)
    nest = (nestable(style, cst) && !nonest) || nrhs
    # @info "" nestable(style, cst) !nonest nrhs nest cst[2]

    if op.fullspan == 0 && cst[3].typ === CSTParser.IDENTIFIER
        # do nothing
    elseif op.kind === Tokens.EX_OR
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif op.kind === Tokens.CIRCUMFLEX_ACCENT && op.dot
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif (
        nospace ||
        (CSTParser.precedence(op) in (8, 13, 14, 16) && op.kind !== Tokens.ANON_FUNC)
    ) && op.kind !== Tokens.IN
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    else
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    end

    if cst[3].typ === CSTParser.BinaryOpCall
        n = pretty(
            style,
            Binaryopcall(),
            cst[3],
            s,
            nonest = nonest,
            nospace = nospace_args,
        )
    elseif cst[3].typ === CSTParser.InvisBrackets
        n = pretty(
            style,
            Invisbrackets(),
            cst[3],
            s,
            nonest = nonest,
            nospace = nospace_args,
        )
    elseif cst[3].typ === CSTParser.ChainOpCall || cst[3].typ === CSTParser.Comparison
        n = pretty(style, Chainopcall(), cst[3], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[3], s)
    end
    # n = pretty(style, cst[3], s, nonest = nonest, nospace = nospace_args)

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices && !is_leaf(cst[3]) && !is_iterable(cst[3])
        paren = FST{Punctuation}(n.startline, n.startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(t, n, s, join_lines = true)
        paren = FST{Punctuation}(n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s, join_lines = true)
    end

    if nest
        # for indent, will be converted to `indent_size` if needed
        insert!(t.nodes, length(t.nodes), Placeholder(0))
    end

    t
end

function pretty(style::DefaultStyle, ::Whereopcall, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Whereopcall}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)

    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    # Used to mark where `B` starts.
    add_node!(t, Placeholder(0), s)

    nest = length(CSTParser.get_where_params(cst)) > 0
    args = get_args(cst.args[3:end])
    # nest = length(args) > 0 && !(length(CSTParser.get_where_params(cst)) == 1 && unnestable_arg(cst[1]))
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))
    add_braces =
        !CSTParser.is_lbrace(cst[3]) && cst.parent.typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.Curly && cst[3].typ !== CSTParser.BracesCat

    add_braces &&
    add_node!(t, FST{Punctuation}(t.endline, t.endline, "{"), s, join_lines = true)

    nws = s.opts.whitespace_typedefs ? 1 : 0
    for (i, a) in enumerate(cst.args[3:end])
        if is_opener(a) && nest
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
            s.indent += s.indent_size
        elseif is_closer(a) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            s.indent -= s.indent_size
        elseif CSTParser.is_comma(a) && !is_punc(cst[i+3])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(
                t,
                pretty(style, a, s, nospace = !s.opts.whitespace_typedefs),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    add_braces &&
    add_node!(t, FST{Punctuation}(t.endline, t.endline, "}"), s, join_lines = true)
    return t
end

# Conditional
function pretty(style::DefaultStyle, ::Conditionalopcall, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Conditionalopcall}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Placeholder(1), s)

    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    add_node!(t, Placeholder(1), s)

    add_node!(t, pretty(style, cst[5], s), s, join_lines = true)
    t
end

function pretty(
    style::DefaultStyle,
    ::Unaryopcall,
    cst::CSTParser.EXPR,
    s::State;
    nospace = true,
)
    style = getstyle(style)
    t = FST{Unaryopcall}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    !nospace && add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    t
end

function pretty(style::DefaultStyle, ::Curly, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Curly}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(cst[1]))

    nws = s.opts.whitespace_typedefs ? 1 : 0
    if nest
        add_node!(t, Placeholder(0), s)
    end

    for (i, a) in enumerate(cst.args[3:end])
        if i + 2 == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

function pretty(style::DefaultStyle, ::Call, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Call}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))

    if nest
        add_node!(t, Placeholder(0), s)
    end

    for (i, a) in enumerate(cst.args[3:end])
        if i + 2 == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

# InvisBrackets
function pretty(
    style::DefaultStyle,
    ::Invisbrackets,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(style)
    t = FST{Invisbrackets}(cst, nspaces(s))
    nest = !is_iterable(cst[2]) && !nonest
    # @info "nest invis" nonest

    for (i, a) in enumerate(cst)
        if a.typ === CSTParser.Block
            add_node!(t, pretty(style, a, s, from_quote = true), s, join_lines = true)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(
                t,
                pretty(style, a, s, nonest = nonest, nospace = nospace),
                s,
                join_lines = true,
            )
        elseif a.typ === CSTParser.InvisBrackets
            add_node!(
                t,
                pretty(style, a, s, nonest = nonest, nospace = nospace),
                s,
                join_lines = true,
            )
        elseif is_opener(a) && nest
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(a) && nest
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

function pretty(style::DefaultStyle, ::TupleFN, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{TupleFN}(cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if is_opener(n) && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(n) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

function pretty(style::DefaultStyle, ::Braces, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Braces}(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_arg(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if i == 1 && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif i == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Vect,Comprehension}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_arg(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if i == 1 && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif i == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end


# Parameters
function pretty(style::DefaultStyle, ::Parameters, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Parameters}(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if i == length(cst) && CSTParser.is_comma(a)
            # do nothing
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Import,Export,Using}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for (i, a) in enumerate(cst.args[2:end])
        if CSTParser.is_comma(a) || CSTParser.is_colon(a)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

function pretty(style::DefaultStyle, ::RefFN, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{RefFN}(cst, nspaces(s))
    nest = length(cst) > 5 && !(length(cst) == 5 && unnestable_arg(cst[3]))
    nospace = !s.opts.whitespace_ops_in_indices
    for (i, a) in enumerate(cst)
        if is_closer(a) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif is_opener(a) && nest
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(
                t,
                pretty(style, a, s, nonest = true, nospace = nospace),
                s,
                join_lines = true,
            )
        elseif a.typ === CSTParser.InvisBrackets
            add_node!(
                t,
                pretty(style, a, s, nonest = true, nospace = nospace),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Vcat,Typedvcat}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    st = cst.typ === CSTParser.Vcat ? 1 : 2
    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))
    # @info "" nest length(cst) st

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        diff_line = t.endline != t.startline
        if is_opener(a) && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif !is_closer(a) && i > st
            add_node!(t, n, s, join_lines = true)
            if i != length(cst) - 1
                has_semicolon(s.doc, n.startline) && add_node!(t, TrailingSemicolon(), s)
                add_node!(t, Placeholder(1), s)
                # Keep trailing semicolon if there's only one arg
            elseif n_args(cst) == 1
                add_node!(t, Semicolon(), s)
                add_node!(t, Placeholder(0), s)
            else
                add_node!(t, Placeholder(0), s)
            end
        else
            # If arguments are on different force nest
            diff_line && (t.force_nest = true)
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Hcat,Typedhcat}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    st = cst.typ === CSTParser.Hcat ? 1 : 2
    for (i, a) in enumerate(cst)
        if i > st && i < length(cst) - 1
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

function pretty(style::DefaultStyle, ::Row, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Row}(cst, nspaces(s))

    # Currently {A <:B} is parsed as a Row type with elements A and <:B
    # instead of a BinaryOpCall A <: B, which is inconsistent with Meta.parse.
    #
    # This is used to overcome that current limitation.
    in_braces = cst.parent === nothing ? false : cst.parent.typ === CSTParser.BracesCat
    nospace = !s.opts.whitespace_typedefs

    for (i, a) in enumerate(cst)
        if in_braces && i < length(cst) && cst[i+1].typ === CSTParser.UnaryOpCall
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Whitespace(nospace ? 0 : 1), s)
        elseif in_braces && a.typ === CSTParser.UnaryOpCall
            add_node!(t, pretty(style, a, s, nospace = nospace), s, join_lines = true)
            i < length(cst) && add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            i < length(cst) && add_node!(t, Whitespace(1), s)
        end
    end
    t
end

function pretty(
    style::DefaultStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Generator,Filter}}
    style = getstyle(style)
    t = FST{T}(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        if a.typ === CSTParser.KEYWORD
            if a.kind === Tokens.FOR && parent_is(
                a,
                is_iterable,
                ignore_typs = (
                    CSTParser.InvisBrackets,
                    CSTParser.Generator,
                    CSTParser.Flatten,
                    CSTParser.Filter,
                ),
            )
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end

            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
            if a.kind === Tokens.FOR
                for j = i+1:length(cst)
                    eq_to_in_normalization!(cst[j], s.opts.always_for_in)
                end
            end
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(t, pretty(style, a, s, nonest = true), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
