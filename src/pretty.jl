# Creates a _prettified_ version of a CST.

@enum(
    PLeaf,
    NEWLINE,
    SEMICOLON,
    WHITESPACE,
    PLACEHOLDER,
    NOTCODE,
    INLINECOMMENT,
    TRAILINGCOMMA,
    TRAILINGSEMICOLON,
)

# Formatted Syntax Tree
mutable struct FST
    typ::Union{CSTParser.Head,PLeaf}
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

FST(cst::CSTParser.EXPR, indent::Integer) =
    FST(cst.typ, -1, -1, indent, 0, nothing, FST[], Ref(cst), false, 0)

function FST(cst::CSTParser.EXPR, startline::Integer, endline::Integer, val::AbstractString)
    FST(cst.typ, startline, endline, 0, length(val), val, nothing, Ref(cst), false, 0)
end

function FST(cst::CSTParser.Head, startline::Integer, endline::Integer, val::AbstractString)
    FST(cst, startline, endline, 0, length(val), val, nothing, nothing, false, 0)
end

@inline Base.setindex!(fst::FST, node::FST, ind::Int) = fst.nodes[ind] = node
@inline Base.getindex(fst::FST, inds...) = fst.nodes[inds...]
@inline Base.lastindex(fst::FST) = length(fst.nodes)


@inline Newline(; length = 0, force_nest = false) =
    FST(NEWLINE, -1, -1, 0, length, "\n", nothing, nothing, force_nest, 0)
@inline Semicolon() = FST(SEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, false, 0)
@inline TrailingComma() = FST(TRAILINGCOMMA, -1, -1, 0, 0, "", nothing, nothing, false, 0)
@inline TrailingSemicolon() = FST(TRAILINGSEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, false, 0)
@inline Whitespace(n) = FST(WHITESPACE, -1, -1, 0, n, " "^n, nothing, nothing, false, 0)
@inline Placeholder(n) = FST(PLACEHOLDER, -1, -1, 0, n, " "^n, nothing, nothing, false, 0)
@inline Notcode(startline, endline) =
    FST(NOTCODE, startline, endline, 0, 0, "", nothing, nothing, false, 0)
@inline InlineComment(line) = FST(INLINECOMMENT, line, line, 0, 0, "", nothing, nothing, false, 0)

@inline Base.length(fst::FST) = fst.len

@inline is_leaf(cst::CSTParser.EXPR) = cst.args === nothing
@inline is_leaf(fst::FST) = fst.nodes === nothing

@inline is_punc(cst::CSTParser.EXPR) = CSTParser.ispunctuation(cst)
@inline is_end(x) = x.typ === CSTParser.KEYWORD && x.val == "end"
@inline is_colon(x) = x.typ === CSTParser.OPERATOR && x.val == ":"
@inline is_comma(fst::FST) =
    (fst.typ === CSTParser.PUNCTUATION && fst.val == ",") || fst.typ === TRAILINGCOMMA
@inline is_comment(fst::FST) = fst.typ === INLINECOMMENT || fst.typ === NOTCODE

@inline is_colon_op(cst::CSTParser.EXPR) =
    (cst.typ === CSTParser.BinaryOpCall && cst[2].kind === Tokens.COLON) ||
    cst.typ === CSTParser.ColonOpCall

@inline is_lazy_op(cst::CSTParser.EXPR) =
    cst.typ === CSTParser.BinaryOpCall &&
    (cst[2].kind === Tokens.LAZY_OR || cst[2].kind === Tokens.LAZY_AND)

function is_multiline(fst::FST)
    fst.typ === CSTParser.StringH && return true
    if fst.typ === CSTParser.x_Str && fst[2].typ === CSTParser.StringH
        return true
    elseif fst.typ === CSTParser.x_Cmd && fst[2].typ === CSTParser.StringH
        return true
    elseif fst.typ === CSTParser.Vcat && fst.endline > fst.startline
        return true
    elseif fst.typ === CSTParser.TypedVcat && fst.endline > fst.startline
        return true
    end
    false
end

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

function add_node!(t::FST, n::FST, s::State; join_lines = false, max_padding = -1)
    if n.typ === SEMICOLON
        join_lines = true
        loc = s.offset > length(s.doc.text) && t.typ === CSTParser.TopLevel ?
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
        # as a PLeaf
        if n.startline == -1
            t.len += length(n)
            n.startline = t.startline
            n.endline = t.endline
            push!(t.nodes, n)
            return
        end
    elseif n.typ === TRAILINGCOMMA
        en = t.nodes[end]
        if en.typ === CSTParser.Generator || en.typ === CSTParser.Filter ||
           en.typ === CSTParser.Flatten || en.typ === CSTParser.MacroCall ||
           (is_comma(en) && t.typ === CSTParser.TupleH && n_args(t.ref[]) == 1)
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
        return
    elseif n.typ === NOTCODE
        n.indent = s.indent
        push!(t.nodes, n)
        return
    elseif n.typ === INLINECOMMENT
        push!(t.nodes, n)
        return
    elseif n.typ isa PLeaf
        t.len += length(n)
        n.startline = t.startline
        n.endline = t.endline
        push!(t.nodes, n)
        return
    end

    if n.typ === CSTParser.Block && length(n) == 0
        push!(t.nodes, n)
        return
    elseif n.typ === CSTParser.Parameters
        if n_args(t.ref[]) == n_args(n.ref[])
            # There are no arguments prior to params
            # so we can remove the initial placeholder.
            idx = findfirst(n -> n.typ === PLACEHOLDER, t.nodes)
            idx !== nothing && deleteat!(t.nodes, idx)
        end
        add_node!(t, Semicolon(), s)
        if length(n.nodes) > 0
            multi_arg = n_args(t.ref[]) > 0
            multi_arg ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
        end
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
        nt = t.nodes[end].typ

        if notcode_startline <= notcode_endline
            # If there are comments in between node elements
            # nesting is forced in an effort to preserve them.
            t.force_nest = true

            # If the previous node type is WHITESPACE - reset it.
            # This fixes cases similar to the one shown in issue #51.
            nt === WHITESPACE && (t.nodes[end] = Whitespace(0))

            hs = hascomment(s.doc, current_line)
            hs && add_node!(t, InlineComment(current_line), s)
            if nt !== PLACEHOLDER
                add_node!(t, Newline(force_nest = true), s)
            elseif hs && nt === PLACEHOLDER
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
        elseif nt === PLACEHOLDER &&
               current_line != n.startline && hascomment(s.doc, current_line)
            t.force_nest = true
            add_node!(t, InlineComment(current_line), s)
            # swap PLACEHOLDER (will be NEWLINE) with INLINECOMMENT node
            idx = length(t.nodes)
            t.nodes[idx-1], t.nodes[idx] = t.nodes[idx], t.nodes[idx-1]
        end

        if n.typ === CSTParser.Parameters && n.force_nest
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
    elseif t.typ === CSTParser.StringH
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

@inline function is_prev_newline(fst::FST)
    if fst.typ === NEWLINE
        return true
    elseif is_leaf(fst) || length(fst.nodes) == 0
        return false
    end
    is_prev_newline(fst[end])
end

"""
    `length_to(x::FST, ntyps; start::Int = 1)`

Returns the length to any node type in `ntyps` based off the `start` index.
"""
@inline function length_to(fst::FST, ntyps::Vector; start::Int = 1)
    fst.typ in ntyps && return 0, true
    is_leaf(fst) && return length(fst), false
    len = 0
    for i = start:length(fst.nodes)
        l, found = length_to(fst.nodes[i], ntyps)
        len += l
        found && return len, found
    end
    return len, false
end

@inline is_closer(fst::FST) =
    fst.typ === CSTParser.PUNCTUATION &&
    (fst.val == "}" || fst.val == ")" || fst.val == "]")
@inline is_closer(cst::CSTParser.EXPR) =
    cst.kind === Tokens.RBRACE || cst.kind === Tokens.RPAREN || cst.kind === Tokens.RSQUARE

@inline is_opener(fst::FST) =
    fst.typ === CSTParser.PUNCTUATION &&
    (fst.val == "{" || fst.val == "(" || fst.val == "[")
@inline is_opener(cst::CSTParser.EXPR) =
    cst.kind === Tokens.LBRACE || cst.kind === Tokens.LPAREN || cst.kind === Tokens.LSQUARE

function pretty(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; kwargs...)
    style = getstyle(ds)
    if cst.typ === CSTParser.IDENTIFIER
        return p_identifier(style, cst, s)
    elseif cst.typ === CSTParser.OPERATOR
        return p_operator(style, cst, s)
    elseif cst.typ === CSTParser.PUNCTUATION
        return p_punctuation(style, cst, s)
    elseif cst.typ === CSTParser.KEYWORD
        return p_keyword(style, cst, s)
    elseif cst.typ === CSTParser.LITERAL
        return p_literal(style, cst, s)
    elseif cst.typ === CSTParser.StringH
        return p_stringh(style, cst, s)
    elseif cst.typ === CSTParser.Block
        return p_block(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.ModuleH
        return p_module(style, cst, s)
    elseif cst.typ === CSTParser.BareModule
        return p_baremodule(style, cst, s)
    elseif cst.typ === CSTParser.FunctionDef
        return p_functiondef(style, cst, s)
    elseif cst.typ === CSTParser.Macro
        return p_macro(style, cst, s)
    elseif cst.typ === CSTParser.Primitive
        return p_primitive(style, cst, s)
    elseif cst.typ === CSTParser.Struct
        return p_struct(style, cst, s)
    elseif cst.typ === CSTParser.Mutable
        return p_mutable(style, cst, s)
    elseif cst.typ === CSTParser.Abstract
        return p_abstract(style, cst, s)
    elseif cst.typ === CSTParser.Primitive
        return p_primitive(style, cst, s)
    elseif cst.typ === CSTParser.For
        return p_for(style, cst, s)
    elseif cst.typ === CSTParser.While
        return p_while(style, cst, s)
    elseif cst.typ === CSTParser.Do
        return p_do(style, cst, s)
    elseif cst.typ === CSTParser.If
        return p_if(style, cst, s)
    elseif cst.typ === CSTParser.Try
        return p_try(style, cst, s)
    elseif cst.typ === CSTParser.TopLevel
        return p_toplevel(style, cst, s)
    elseif cst.typ === CSTParser.Begin
        return p_begin(style, cst, s)
    elseif cst.typ === CSTParser.Quote
        return p_quote(style, cst, s)
    elseif cst.typ === CSTParser.Let
        return p_let(style, cst, s)
    elseif cst.typ === CSTParser.Vect
        return p_vect(style, cst, s)
    elseif cst.typ === CSTParser.Comprehension
        return p_comprehension(style, cst, s)
    elseif cst.typ === CSTParser.Braces
        return p_braces(style, cst, s)
    elseif cst.typ === CSTParser.TupleH
        return p_tupleh(style, cst, s)
    elseif cst.typ === CSTParser.InvisBrackets
        return p_invisbrackets(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.Curly
        return p_curly(style, cst, s)
    elseif cst.typ === CSTParser.Call
        return p_call(style, cst, s)
    elseif cst.typ === CSTParser.MacroCall
        return p_macrocall(style, cst, s)
    elseif cst.typ === CSTParser.WhereOpCall
        return p_whereopcall(style, cst, s)
    elseif cst.typ === CSTParser.ConditionalOpCall
        return p_conditionalopcall(style, cst, s)
    elseif cst.typ === CSTParser.BinaryOpCall
        return p_binaryopcall(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.UnaryOpCall
        return p_unaryopcall(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.ChainOpCall
        return p_chainopcall(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.Comparison
        return p_comparison(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.ColonOpCall
        return p_colonopcall(style, cst, s)
    elseif cst.typ === CSTParser.Kw
        return p_kw(style, cst, s)
    elseif cst.typ === CSTParser.Parameters
        return p_parameters(style, cst, s)
    elseif cst.typ === CSTParser.Local
        return p_local(style, cst, s)
    elseif cst.typ === CSTParser.Global
        return p_global(style, cst, s)
    elseif cst.typ === CSTParser.Const
        return p_const(style, cst, s)
    elseif cst.typ === CSTParser.Return
        return p_return(style, cst, s)
    elseif cst.typ === CSTParser.Import
        return p_import(style, cst, s)
    elseif cst.typ === CSTParser.Export
        return p_export(style, cst, s)
    elseif cst.typ === CSTParser.Using
        return p_using(style, cst, s)
    elseif cst.typ === CSTParser.Row
        return p_row(style, cst, s)
    elseif cst.typ === CSTParser.Vcat
        return p_vcat(style, cst, s)
    elseif cst.typ === CSTParser.TypedVcat
        return p_typedvcat(style, cst, s)
    elseif cst.typ === CSTParser.Hcat
        return p_hcat(style, cst, s)
    elseif cst.typ === CSTParser.TypedHcat
        return p_typedhcat(style, cst, s)
    elseif cst.typ === CSTParser.Ref
        return p_ref(style, cst, s)
    elseif cst.typ === CSTParser.Generator
        return p_generator(style, cst, s)
    elseif cst.typ === CSTParser.Filter
        return p_filter(style, cst, s)
    elseif cst.typ === CSTParser.FileH
        return p_fileh(style, cst, s)
    end

    t = FST(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(
            t,
            pretty(style,a, s),
            s,
            join_lines = true,
        )
    end
    t
end
pretty(style::S, cst::CSTParser.EXPR, s::State; kwargs...) where S <: AbstractStyle = pretty(DefaultStyle(style), cst, s; kwargs...)

function p_fileh(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(
            t,
            pretty(ds ,a, s),
            s,
            join_lines = false,
            max_padding = 0,
        )
    end
    t
end
p_fileh(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_fileh(DefaultStyle(style), cst, s)

@inline function p_identifier(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    loc = cursor_loc(s)
    s.offset += cst.fullspan
    FST(cst, loc[1], loc[1], cst.val)
end
p_identifier(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_identifier(DefaultStyle(style), cst, s)

@inline function p_operator(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    loc = cursor_loc(s)
    val = string(CSTParser.Expr(cst))
    s.offset += cst.fullspan
    FST(cst, loc[1], loc[1], val)
end
p_operator(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_operator(DefaultStyle(style), cst, s)

@inline function p_keyword(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
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
    FST(cst, loc[1], loc[1], val)
end
p_keyword(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_keyword(DefaultStyle(style), cst, s)

@inline function p_punctuation(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
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
    FST(cst, loc[1], loc[1], val)
end
p_punctuation(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_punctuation(DefaultStyle(style), cst, s)

@inline function p_literal(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
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
        return FST(cst, loc[1], loc[1], val)
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
        return FST(cst, loc[1], loc[1], cst.val)
    end

    startline, endline, str = str_info
    # @debug "" loc startline endline str

    s.offset += cst.fullspan

    lines = split(str, "\n")

    if length(lines) == 1
        return FST(cst, loc[1], loc[1], lines[1])
    end

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    # @debug "" lines cst.val loc loc[2] sidx

    t = FST(CSTParser.StringH, -1, -1, loc[2] - 1, 0, nothing, FST[], Ref(cst), false, 0)
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        tt = FST(
            CSTParser.LITERAL,
            ln,
            ln,
            sidx - 1,
            length(l),
            l,
            nothing,
            nothing,
            false,
            0,
        )
        add_node!(t, tt, s)
    end
    t
end
p_literal(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_literal(DefaultStyle(style), cst, s)

# StringH
function p_stringh(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    loc = cursor_loc(s)
    startline, endline, str = s.doc.lit_strings[s.offset-1]

    s.offset += cst.fullspan

    lines = split(str, "\n")

    if length(lines) == 1
        t = FST(cst, startline, startline, lines[1])
        t.typ = CSTParser.LITERAL
        return t
    end

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    # @debug "" lines cst.val loc loc[2] sidx

    t = FST(cst, loc[2] - 1)
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        tt = FST(
            CSTParser.LITERAL,
            ln,
            ln,
            sidx - 1,
            length(l),
            l,
            nothing,
            nothing,
            false,
            0,
        )
        add_node!(t, tt, s)
    end
    t
end
p_stringh(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_stringh(DefaultStyle(style), cst, s)

# MacroCall
function p_macrocall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    if cst[1].typ === CSTParser.GlobalRefDoc
        # cst[1] is empty and fullspan is 0 so we can skip it
        if cst[2].typ === CSTParser.LITERAL
            add_node!(t, p_literal(style, cst[2], s), s, max_padding = 0)
        elseif cst[2].typ == CSTParser.StringH
            add_node!(t, p_stringh(style, cst[2], s), s)
        end
        add_node!(t, pretty(style,cst[3], s), s, max_padding = 0)
        return t
    elseif length(cst) == 3 &&
           cst[1].typ === CSTParser.MacroName && cst[1][2].val == "doc" && is_str(cst[2])
        add_node!(t, pretty(style,cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
        n = pretty(style,cst[3], s)
        join_lines = t.endline == n.startline
        join_lines && add_node!(t, Whitespace(1), s)
        add_node!(t, n, s, join_lines = join_lines, max_padding = 0)
        return t
    end

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))
    has_closer = is_closer(cst.args[end])

    # @info "" has_closer

    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(cst)
        n = pretty(style,a, s)
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
p_macrocall(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_macrocall(DefaultStyle(style), cst, s)

# Block
# length Block is the length of the longest expr
function p_block(
                 ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    ignore_single_line = false,
    from_quote = false,
    join_body = false,
)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    single_line = ignore_single_line ? false :
        cursor_loc(s)[1] == cursor_loc(s, s.offset + cst.span - 1)[1]

    # @info "" from_quote single_line ignore_single_line join_body
    for (i, a) in enumerate(cst)
        n = pretty(style,a, s)
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
p_block(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_block(DefaultStyle(style), cst, s)

# Abstract
function p_abstract(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[4], s), s, join_lines = true)
    t
end
p_abstract(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_abstract(DefaultStyle(style), cst, s)

# Primitive
function p_primitive(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[4], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[5], s), s, join_lines = true)
    t
end
p_primitive(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_primitive(DefaultStyle(style), cst, s)

# FunctionDef/Macro
function p_functiondef(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    if length(cst) > 3
        if cst[3].fullspan == 0
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style,cst[4], s), s, join_lines = true)
        else
            s.indent += s.indent_size
            add_node!(
                t,
                pretty(style, cst[3], s, ignore_single_line = true),
                s,
                max_padding = s.indent_size,
            )
            s.indent -= s.indent_size
            add_node!(t, pretty(style,cst[4], s), s)
        end
    else
        # function stub, i.e. "function foo end"
        # this should be on one line
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,cst[3], s), s, join_lines = true)
    end
    t
end
p_functiondef(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_functiondef(DefaultStyle(style), cst, s)

@inline p_macro(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_functiondef(ds, cst, s)
p_macro(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_macro(DefaultStyle(style), cst, s)

# Struct
function p_struct(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    if cst[3].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,cst[4], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(style,cst[4], s), s)
    end
    t
end
p_struct(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_struct(DefaultStyle(style), cst, s)

# Mutable
function p_mutable(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[3], s), s, join_lines = true)
    if cst[4].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,cst[5], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[4], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(style,cst[5], s), s)
    end
    t
end
p_mutable(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_mutable(DefaultStyle(style), cst, s)

# ModuleH/BareModule
function p_module(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    if cst[3].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,cst[4], s), s, join_lines = true)
    else
        add_node!(t, pretty(style,cst[3], s), s, max_padding = 0)
        add_node!(t, pretty(style,cst[4], s), s)
    end
    t
end
p_module(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_module(DefaultStyle(style), cst, s)

@inline p_baremodule(style::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_module(style, cst, s)
p_baremodule(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_baremodule(DefaultStyle(style), cst, s)

# Const/Local/Global/Return
function p_const(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    if cst[2].fullspan != 0
        for a in cst.args[2:end]
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_const(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_const(DefaultStyle(style), cst, s)

@inline p_local(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_const(ds, cst, s)
p_local(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_local(DefaultStyle(style), cst, s)

@inline p_global(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_const(ds, cst, s)
p_global(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_global(DefaultStyle(style), cst, s)

@inline p_return(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_const(ds, cst, s)
p_return(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_return(DefaultStyle(style), cst, s)

# TopLevel
function p_toplevel(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    for a in cst.args
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style,a, s), s, max_padding = s.indent_size)
        add_node!(t, Semicolon(), s)
    end
    t
end
p_toplevel(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_toplevel(DefaultStyle(style), cst, s)

# Begin
function p_begin(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    if cst[2].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,cst[3], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            pretty(style, cst[2], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(style,cst[3], s), s)
    end
    t
end
p_begin(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_begin(DefaultStyle(style), cst, s)

# Quote
function p_quote(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    if cst[1].typ === CSTParser.KEYWORD && cst[1].kind === Tokens.QUOTE
        add_node!(t, pretty(style,cst[1], s), s)
        if cst[2].fullspan == 0
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style,cst[3], s), s, join_lines = true)
        else
            s.indent += s.indent_size
            add_node!(
                t,
                pretty(style, cst[2], s, ignore_single_line = true),
                s,
                max_padding = s.indent_size,
            )
            s.indent -= s.indent_size
            add_node!(t, pretty(style,cst[3], s), s)
        end
    else
        for a in cst.args
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_quote(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_quote(DefaultStyle(style), cst, s)

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
function p_let(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    if length(cst.args) > 3
        add_node!(t, Whitespace(1), s)
        s.indent += s.indent_size
        if cst[2].typ === CSTParser.Block
            add_node!(t, pretty(style, cst[2], s, join_body = true), s, join_lines = true)
        else
            add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
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
        if cst[2].typ === CSTParser.Block && t.nodes[end-2].typ !== NOTCODE
            add_node!(t.nodes[idx], Placeholder(0), s)
        end
        add_node!(t, pretty(style,cst.args[end], s), s)
    else
        s.indent += s.indent_size
        add_node!(t, pretty(style, cst[2], s, ignore_single_line = true), s)
        s.indent -= s.indent_size
        add_node!(t, pretty(style,cst.args[end], s), s)
    end
    t
end
p_let(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_let(DefaultStyle(style), cst, s)


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
function p_for(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    if cst[1].kind === Tokens.FOR
        eq_to_in_normalization!(cst[2], s.opts.always_for_in)
    end
    if cst[2].typ === CSTParser.Block
        s.indent += s.indent_size
        add_node!(t, pretty(style, cst[2], s, join_body = true), s, join_lines = true)
        s.indent -= s.indent_size
    else
        add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
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
    if cst[2].typ === CSTParser.Block && t.nodes[end-2].typ !== NOTCODE
        add_node!(t.nodes[idx], Placeholder(0), s)
    end
    add_node!(t, pretty(style,cst[4], s), s)
    t
end
p_for(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_for(DefaultStyle(style), cst, s)

@inline p_while(style::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_for(style, cst, s)
p_while(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_while(DefaultStyle(style), cst, s)

# Do
function p_do(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    if cst[3].fullspan != 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,cst[3], s), s, join_lines = true)
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
    add_node!(t, pretty(style,cst.args[end], s), s)
    t
end
p_do(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_do(DefaultStyle(style), cst, s)

# Try
function p_try(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    for a in cst.args
        if a.fullspan == 0 && a.typ !== CSTParser.Block
        elseif a.typ === CSTParser.KEYWORD
            add_node!(t, pretty(style,a, s), s, max_padding = 0)
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
            n = pretty(style,a, s)
            # "catch n"
            t.len = max(len, 5 + 1 + length(n))
            add_node!(t, n, s, join_lines = true, max_padding = 0)
        end
    end
    t
end
p_try(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_try(DefaultStyle(style), cst, s)

# If
function p_if(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    if cst[1].typ === CSTParser.KEYWORD && cst[1].kind === Tokens.IF
        add_node!(t, pretty(style,cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
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
            add_node!(t, pretty(style,cst[4], s), s, max_padding = 0)
            if cst[4].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1), s)
                n = pretty(style,cst[5], s)
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
        add_node!(t, pretty(style,cst.args[end], s), s)
    else
        # "cond" part of "elseif cond"
        t.len += 7
        add_node!(t, pretty(style,cst[1], s), s)

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
            add_node!(t, pretty(style,cst[3], s), s, max_padding = 0)

            if cst[3].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1), s)
                n = pretty(style,cst[4], s)
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
p_if(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_if(DefaultStyle(style), cst, s)

# ChainOpCall/Comparison
function p_chainopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; nonest = false, nospace = false)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nws = nospace ? 0 : 1
    for (i, a) in enumerate(cst)
        n = pretty(style,a, s)
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
p_chainopcall(style::S, cst::CSTParser.EXPR, s::State; nonest = false, nospace = false) where S <: AbstractStyle = p_chainopcall(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

@inline p_comparison(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; nonest = false, nospace = false) =
    p_chainopcall(ds, cst, s, nonest = nonest, nospace = nospace)
p_comparison(style::S, cst::CSTParser.EXPR, s::State; nonest = false, nospace = false) where S <: AbstractStyle= 
p_comparison(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

# ColonOpCall
function p_colonopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices
    for a in cst
        if a.typ === CSTParser.BinaryOpCall
            n = pretty(style, a, s, nonest = true, nospace = nospace)
        elseif a.typ === CSTParser.InvisBrackets
            n = pretty(style, a, s, nonest = true, nospace = nospace)
        elseif a.typ === CSTParser.ChainOpCall || a.typ === CSTParser.Comparison
            n = pretty(style, a, s, nonest = true, nospace = nospace)
        else
            n = pretty(style,a, s)
        end

        if s.opts.whitespace_ops_in_indices && !is_leaf(n) && !is_iterable(n)
            paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, "(")
            add_node!(t, paren, s, join_lines = true)
            add_node!(t, n, s, join_lines = true)
            paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, ")")
            add_node!(t, paren, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_colonopcall(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_colonopcall(DefaultStyle(style), cst, s)

# Kw
function p_kw(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.EQ
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_kw(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_kw(DefaultStyle(style), cst, s)

@inline is_str(cst::CSTParser.EXPR) = is_str_or_cmd(cst.kind) || is_str_or_cmd(cst.typ)

is_iterable(x::Union{CSTParser.EXPR,FST}) =
    x.typ === CSTParser.TupleH || x.typ === CSTParser.Vect ||
    x.typ === CSTParser.Vcat || x.typ === CSTParser.Braces || x.typ === CSTParser.Call ||
    x.typ === CSTParser.Curly || x.typ === CSTParser.Comprehension ||
    x.typ === CSTParser.MacroCall || x.typ === CSTParser.InvisBrackets ||
    x.typ === CSTParser.Ref || x.typ === CSTParser.TypedVcat

is_block(cst::CSTParser.EXPR) =
    cst.typ === CSTParser.If || cst.typ === CSTParser.Do || cst.typ === CSTParser.Try ||
    cst.typ === CSTParser.For || cst.typ === CSTParser.While || cst.typ === CSTParser.Let

nest_assignment(cst::CSTParser.EXPR) = CSTParser.precedence(cst[2].kind) == 1

unnestable_arg(cst::CSTParser.EXPR) =
    is_iterable(cst) || is_str(cst) || cst.typ === CSTParser.LITERAL ||
    (cst.typ === CSTParser.BinaryOpCall && cst[2].kind === Tokens.DOT)

function nestable(::S, cst::CSTParser.EXPR) where S <: AbstractStyle
    CSTParser.defines_function(cst) && cst[1].typ !== CSTParser.UnaryOpCall && return true
    nest_assignment(cst) && return !is_str(cst[3])
    true
end

function nest_rhs(cst::CSTParser.EXPR)::Bool
    if CSTParser.defines_function(cst)
        rhs = cst[3]
        rhs.typ === CSTParser.Block && (rhs = rhs[1])
        return is_block(rhs)
    end
    false
end

# BinaryOpCall
function p_binaryopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; nonest = false, nospace = false)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
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
        n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)
    elseif cst[1].typ === CSTParser.InvisBrackets
        n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)
    elseif cst[1].typ === CSTParser.ChainOpCall || cst[1].typ === CSTParser.Comparison
        n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style,cst[1], s)
    end

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices && !is_leaf(cst[1]) && !is_iterable(cst[1])
        paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, "(")
        add_node!(t, paren, s)
        add_node!(t, n, s, join_lines = true)
        paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s)
    end

    nrhs = nest_rhs(cst)
    nrhs && (t.force_nest = true)
    nest = (nestable(style, cst) && !nonest) || nrhs
    # @info "" nestable(cst) !nonest nrhs nest cst[2]

    if op.fullspan == 0 && cst[3].typ === CSTParser.IDENTIFIER
        # do nothing
    elseif op.kind === Tokens.EX_OR
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,op, s), s, join_lines = true)
    elseif op.kind === Tokens.CIRCUMFLEX_ACCENT && op.dot
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif (
        nospace ||
        (CSTParser.precedence(op) in (8, 13, 14, 16) && op.kind !== Tokens.ANON_FUNC)
    ) && op.kind !== Tokens.IN
        add_node!(t, pretty(style,op, s), s, join_lines = true)
    else
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style,op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    end

    if cst[3].typ === CSTParser.BinaryOpCall
        n = pretty(style, cst[3], s, nonest = nonest, nospace = nospace_args)
    elseif cst[3].typ === CSTParser.InvisBrackets
        n = pretty(style, cst[3], s, nonest = nonest, nospace = nospace_args)
    elseif cst[3].typ === CSTParser.ChainOpCall || cst[3].typ === CSTParser.Comparison
        n = pretty(style, cst[3], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style,cst[3], s)
    end

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices && !is_leaf(cst[3]) && !is_iterable(cst[3])
        paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(t, n, s, join_lines = true)
        paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, ")")
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
p_binaryopcall(style::S, cst::CSTParser.EXPR, s::State; nonest = false, nospace = false) where S <: AbstractStyle = p_binaryopcall(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

# WhereOpCall
# A where B
function p_whereopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)

    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
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

    add_braces && add_node!(
        t,
        FST(CSTParser.PUNCTUATION, t.endline, t.endline, "{"),
        s,
        join_lines = true,
    )

    nws = s.opts.whitespace_typedefs ? 1 : 0
    # @debug "" nest in_braces cst[3].val == "{" cst.args[end].val
    for (i, a) in enumerate(cst.args[3:end])
        if is_opener(a) && nest
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
            s.indent += s.indent_size
        elseif is_closer(a) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            s.indent -= s.indent_size
        elseif CSTParser.is_comma(a) && !is_punc(cst[i+3])
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(
                t,
                pretty(style, a, s, nospace = !s.opts.whitespace_typedefs),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    add_braces && add_node!(
        t,
        FST(CSTParser.PUNCTUATION, t.endline, t.endline, "}"),
        s,
        join_lines = true,
    )
    t
end
p_whereopcall(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_whereopcall(DefaultStyle(style), cst, s)

# Conditional
function p_conditionalopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    add_node!(t, Placeholder(1), s)

    add_node!(t, pretty(style,cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[4], s), s, join_lines = true)
    add_node!(t, Placeholder(1), s)

    add_node!(t, pretty(style,cst[5], s), s, join_lines = true)
    t
end
p_conditionalopcall(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_conditionalopcall(DefaultStyle(style), cst, s)

# UnaryOpCall
function p_unaryopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; nospace = true)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    !nospace && add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)
    t
end
p_unaryopcall(style::S, cst::CSTParser.EXPR, s::State; nospace = true) where S <: AbstractStyle = p_unaryopcall(DefaultStyle(style), cst, s, nospace = nospace)

function p_curly(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)

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
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_curly(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_curly(DefaultStyle(style), cst, s)

function p_call(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, pretty(style,cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))

    if nest
        add_node!(t, Placeholder(0), s)
    end

    for (i, a) in enumerate(cst.args[3:end])
        if i + 2 == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_call(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_call(DefaultStyle(style), cst, s)

# InvisBrackets
function p_invisbrackets(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; nonest = false, nospace = false)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
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
            # @info "opening"
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(a) && nest
            # @info "closing"
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_invisbrackets(style::S, cst::CSTParser.EXPR, s::State; nonest = false, nospace = false) where S <: AbstractStyle = p_invisbrackets(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

# TupleH
function p_tupleh(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))

    for (i, a) in enumerate(cst)
        n = pretty(style,a, s)
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
p_tupleh(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_tupleh(DefaultStyle(style), cst, s)

# Braces
function p_braces(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_arg(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(style,a, s)
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
p_braces(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_braces(DefaultStyle(style), cst, s)

# Vect
function p_vect(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_arg(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(style,a, s)
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
p_vect(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_vect(DefaultStyle(style), cst, s)

@inline p_comprehension(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_vect(ds, cst, s)
p_comprehension(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_comprehension(DefaultStyle(style), cst, s)

# Parameters
function p_parameters(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(style,a, s)
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
p_parameters(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_parameters(DefaultStyle(style), cst, s)

# Import, Export, Using
function p_import(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style,cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for (i, a) in enumerate(cst.args[2:end])
        if CSTParser.is_comma(a) || CSTParser.is_colon(a)
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_import(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_import(DefaultStyle(style), cst, s)

@inline p_export(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_import(ds, cst, s)
p_export(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_export(DefaultStyle(style), cst, s)

@inline p_using(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_import(ds, cst, s)
p_using(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_using(DefaultStyle(style), cst, s)

# Ref
function p_ref(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 5 && !(length(cst) == 5 && unnestable_arg(cst[3]))
    nospace = !s.opts.whitespace_ops_in_indices
    for (i, a) in enumerate(cst)
        if is_closer(a) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        elseif is_opener(a) && nest
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style,a, s), s, join_lines = true)
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
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_ref(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_ref(DefaultStyle(style), cst, s)

# Vcat/TypedVcat
function p_vcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    st = cst.typ === CSTParser.Vcat ? 1 : 2
    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))
    # @info "" nest length(cst) st

    for (i, a) in enumerate(cst)
        n = pretty(style,a, s)
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
p_vcat(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_vcat(DefaultStyle(style), cst, s)

@inline p_typedvcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_vcat(ds, cst, s)
p_typedvcat(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_typedvcat(DefaultStyle(style), cst, s)

# Hcat/TypedHcat
function p_hcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    st = cst.typ === CSTParser.Hcat ? 1 : 2
    for (i, a) in enumerate(cst)
        if i > st && i < length(cst) - 1
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_hcat(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_hcat(DefaultStyle(style), cst, s)

@inline p_typedhcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_hcat(ds, cst, s)
p_typedhcat(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_typedhcat(DefaultStyle(style), cst, s)

# Row
function p_row(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    # Currently {A <:B} is parsed as a Row type with elements A and <:B
    # instead of a BinaryOpCall A <: B, which is inconsistent with Meta.parse.
    #
    # This is used to overcome that current limitation.
    in_braces = cst.parent === nothing ? false : cst.parent.typ === CSTParser.BracesCat
    nospace = !s.opts.whitespace_typedefs

    for (i, a) in enumerate(cst)
        if in_braces && i < length(cst) && cst[i+1].typ === CSTParser.UnaryOpCall
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Whitespace(nospace ? 0 : 1), s)
        elseif in_braces && a.typ === CSTParser.UnaryOpCall
            add_node!(t, pretty(style, a, s, nospace = nospace), s, join_lines = true)
            i < length(cst) && add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            i < length(cst) && add_node!(t, Whitespace(1), s)
        end
    end
    t
end
p_row(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_row(DefaultStyle(style), cst, s)

# Generator/Filter
function p_generator(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
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

            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
            if a.kind === Tokens.FOR
                for j = i+1:length(cst)
                    eq_to_in_normalization!(cst[j], s.opts.always_for_in)
                end
            end
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(t, pretty(style, a, s, nonest = true), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style,a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style,a, s), s, join_lines = true)
        end
    end
    t
end
p_generator(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_generator(DefaultStyle(style), cst, s)

@inline p_filter(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_generator(ds, cst, s)
p_filter(style::S, cst::CSTParser.EXPR, s::State) where S <: AbstractStyle = p_filter(DefaultStyle(style), cst, s)
