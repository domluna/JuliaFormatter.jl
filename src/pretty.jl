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

Base.setindex!(fst::FST, node::FST, ind::Int) = fst.nodes[ind] = node
Base.getindex(fst::FST, inds...) = fst.nodes[inds...]
Base.lastindex(fst::FST) = length(fst.nodes)


Newline(; length = 0, force_nest = false) =
    FST(NEWLINE, -1, -1, 0, length, "\n", nothing, nothing, force_nest, 0)
Semicolon() = FST(SEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, false, 0)
TrailingComma() = FST(TRAILINGCOMMA, -1, -1, 0, 0, "", nothing, nothing, false, 0)
TrailingSemicolon() = FST(TRAILINGSEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, false, 0)
Whitespace(n) = FST(WHITESPACE, -1, -1, 0, n, " "^n, nothing, nothing, false, 0)
Placeholder(n) = FST(PLACEHOLDER, -1, -1, 0, n, " "^n, nothing, nothing, false, 0)
Notcode(startline, endline) =
    FST(NOTCODE, startline, endline, 0, 0, "", nothing, nothing, false, 0)
InlineComment(line) = FST(INLINECOMMENT, line, line, 0, 0, "", nothing, nothing, false, 0)

Base.length(fst::FST) = fst.len

is_leaf(cst::CSTParser.EXPR) = cst.args === nothing
is_leaf(fst::FST) = fst.nodes === nothing
empty_start(fst::FST) = fst.startline == 1 && fst.endline == 1 && fst.val == ""

is_punc(x) = CSTParser.ispunctuation(x)
is_end(x) = x.typ === CSTParser.KEYWORD && x.val == "end"
is_colon(x) = x.typ === CSTParser.OPERATOR && x.val == ":"
is_comma(fst::FST) =
    (fst.typ === CSTParser.PUNCTUATION && fst.val == ",") || fst.typ === TRAILINGCOMMA
is_comment(fst::FST) = fst.typ === INLINECOMMENT || fst.typ === NOTCODE

is_colon_op(x) =
    (x.typ === CSTParser.BinaryOpCall && x[2].kind === Tokens.COLON) ||
    x.typ === CSTParser.ColonOpCall

is_lazy_op(x) =
    x.typ === CSTParser.BinaryOpCall &&
    (x[2].kind === Tokens.LAZY_OR || x[2].kind === Tokens.LAZY_AND)

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
                # @info "found semicolon" l
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
    elseif n.typ === NOTCODE || n.typ === INLINECOMMENT
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
            hascomment(s.doc, current_line) && add_node!(t, InlineComment(current_line), s)
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

function is_prev_newline(fst::FST)
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
function length_to(fst::FST, ntyps::Vector; start::Int = 1)
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

is_closer(fst::FST) =
    fst.typ === CSTParser.PUNCTUATION &&
    (fst.val == "}" || fst.val == ")" || fst.val == "]")
is_closer(cst::CSTParser.EXPR) =
    cst.kind === Tokens.RBRACE || cst.kind === Tokens.RPAREN || cst.kind === Tokens.RSQUARE

is_opener(fst::FST) =
    fst.typ === CSTParser.PUNCTUATION &&
    (fst.val == "{" || fst.val == "(" || fst.val == "[")
is_opener(cst::CSTParser.EXPR) =
    cst.kind === Tokens.LBRACE || cst.kind === Tokens.LPAREN || cst.kind === Tokens.LSQUARE

function pretty(cst::CSTParser.EXPR, s::State)
    if cst.typ === CSTParser.IDENTIFIER
        return p_identifier(cst, s)
    elseif cst.typ === CSTParser.OPERATOR
        return p_operator(cst, s)
    elseif cst.typ === CSTParser.PUNCTUATION
        return p_punctuation(cst, s)
    elseif cst.typ === CSTParser.KEYWORD
        return p_keyword(cst, s)
    elseif cst.typ === CSTParser.LITERAL
        return p_literal(cst, s)
    elseif cst.typ === CSTParser.StringH
        return p_stringh(cst, s)
    elseif cst.typ === CSTParser.Block
        return p_block(cst, s)
    elseif cst.typ === CSTParser.ModuleH
        return p_module(cst, s)
    elseif cst.typ === CSTParser.BareModule
        return p_baremodule(cst, s)
    elseif cst.typ === CSTParser.FunctionDef
        return p_functiondef(cst, s)
    elseif cst.typ === CSTParser.Macro
        return p_macro(cst, s)
    elseif cst.typ === CSTParser.Primitive
        return p_primitive(cst, s)
    elseif cst.typ === CSTParser.Struct
        return p_struct(cst, s)
    elseif cst.typ === CSTParser.Mutable
        return p_mutable(cst, s)
    elseif cst.typ === CSTParser.Abstract
        return p_abstract(cst, s)
    elseif cst.typ === CSTParser.Primitive
        return p_primitive(cst, s)
    elseif cst.typ === CSTParser.For
        return p_for(cst, s)
    elseif cst.typ === CSTParser.While
        return p_while(cst, s)
    elseif cst.typ === CSTParser.Do
        return p_do(cst, s)
    elseif cst.typ === CSTParser.If
        return p_if(cst, s)
    elseif cst.typ === CSTParser.Try
        return p_try(cst, s)
    elseif cst.typ === CSTParser.TopLevel
        return p_toplevel(cst, s)
    elseif cst.typ === CSTParser.Begin
        return p_begin(cst, s)
    elseif cst.typ === CSTParser.Quote
        return p_quote(cst, s)
    elseif cst.typ === CSTParser.Let
        return p_let(cst, s)
    elseif cst.typ === CSTParser.Vect
        return p_vect(cst, s)
    elseif cst.typ === CSTParser.Comprehension
        return p_comprehension(cst, s)
    elseif cst.typ === CSTParser.Braces
        return p_braces(cst, s)
    elseif cst.typ === CSTParser.TupleH
        return p_tupleh(cst, s)
    elseif cst.typ === CSTParser.InvisBrackets
        return p_invisbrackets(cst, s)
    elseif cst.typ === CSTParser.Curly
        return p_curly(cst, s)
    elseif cst.typ === CSTParser.Call
        return p_call(cst, s)
    elseif cst.typ === CSTParser.MacroCall
        return p_macrocall(cst, s)
    elseif cst.typ === CSTParser.WhereOpCall
        return p_whereopcall(cst, s)
    elseif cst.typ === CSTParser.ConditionalOpCall
        return p_conditionalopcall(cst, s)
    elseif cst.typ === CSTParser.BinaryOpCall
        return p_binaryopcall(cst, s)
    elseif cst.typ === CSTParser.UnaryOpCall
        return p_unaryopcall(cst, s)
    elseif cst.typ === CSTParser.ChainOpCall
        return p_chainopcall(cst, s)
    elseif cst.typ === CSTParser.ColonOpCall
        return p_colonopcall(cst, s)
    elseif cst.typ === CSTParser.Comparison
        return p_comparison(cst, s)
    elseif cst.typ === CSTParser.Kw
        return p_kw(cst, s)
    elseif cst.typ === CSTParser.Parameters
        return p_parameters(cst, s)
    elseif cst.typ === CSTParser.Local
        return p_local(cst, s)
    elseif cst.typ === CSTParser.Global
        return p_global(cst, s)
    elseif cst.typ === CSTParser.Const
        return p_const(cst, s)
    elseif cst.typ === CSTParser.Return
        return p_return(cst, s)
    elseif cst.typ === CSTParser.Import
        return p_import(cst, s)
    elseif cst.typ === CSTParser.Export
        return p_export(cst, s)
    elseif cst.typ === CSTParser.Using
        return p_using(cst, s)
    elseif cst.typ === CSTParser.Row
        return p_row(cst, s)
    elseif cst.typ === CSTParser.Vcat
        return p_vcat(cst, s)
    elseif cst.typ === CSTParser.TypedVcat
        return p_typedvcat(cst, s)
    elseif cst.typ === CSTParser.Hcat
        return p_hcat(cst, s)
    elseif cst.typ === CSTParser.TypedHcat
        return p_typedhcat(cst, s)
    elseif cst.typ === CSTParser.Ref
        return p_ref(cst, s)
    elseif cst.typ === CSTParser.Generator
        return p_generator(cst, s)
    elseif cst.typ === CSTParser.Filter
        return p_filter(cst, s)
    end

    t = FST(cst, nspaces(s))
    is_fileh = cst.typ === CSTParser.FileH
    for a in cst
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        # @debug "" a a.typ
        add_node!(
            t,
            pretty(a, s),
            s,
            join_lines = !is_fileh,
            max_padding = is_fileh ? 0 : -1,
        )
    end
    t
end

function p_identifier(cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    s.offset += cst.fullspan
    FST(cst, loc[1], loc[1], cst.val)
end

function p_operator(cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    val = string(CSTParser.Expr(cst))
    s.offset += cst.fullspan
    FST(cst, loc[1], loc[1], val)
end

function p_keyword(cst::CSTParser.EXPR, s::State)
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

function p_punctuation(cst::CSTParser.EXPR, s::State)
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

function p_literal(cst::CSTParser.EXPR, s::State)
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

# StringH
function p_stringh(cst::CSTParser.EXPR, s::State)
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


# MacroCall
function p_macrocall(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    if cst[1].typ === CSTParser.GlobalRefDoc
        # cst[1] is empty and fullspan is 0 so we can skip it
        if cst[2].typ === CSTParser.LITERAL
            add_node!(t, p_literal(cst[2], s), s, max_padding = 0)
        elseif cst[2].typ == CSTParser.StringH
            add_node!(t, p_stringh(cst[2], s), s)
        end
        add_node!(t, pretty(cst[3], s), s, max_padding = 0)
        return t
    elseif cst[1].typ === CSTParser.MacroName  && cst[1][2].val == "doc" && is_str(cst[2])
        add_node!(t, pretty(cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(cst[2], s), s, join_lines=true)
        n = pretty(cst[3], s)
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
        n = pretty(a, s)
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
            if has_closer && i < length(cst) - 1
                add_node!(t, n, s, join_lines = true)
                if cst[i+1].typ !== CSTParser.Parameters
                    add_node!(t, Whitespace(1), s)
                end
            elseif !has_closer && i < length(cst)
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            else
                add_node!(t, n, s, join_lines = true)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

# Block
# length Block is the length of the longest expr
function p_block(
    cst::CSTParser.EXPR,
    s::State;
    ignore_single_line = false,
    from_quote = false,
    join_body = false,
)
    t = FST(cst, nspaces(s))
    single_line = ignore_single_line ? false :
        cursor_loc(s)[1] == cursor_loc(s, s.offset + cst.span - 1)[1]

    # @info "" from_quote single_line ignore_single_line join_body
    for (i, a) in enumerate(cst)
        n = pretty(a, s)
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

# Abstract
function p_abstract(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[4], s), s, join_lines = true)
    t
end

# Primitive
function p_primitive(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[4], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[5], s), s, join_lines = true)
    t
end

# FunctionDef/Macro
function p_functiondef(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    if length(cst) > 3
        if cst[3].fullspan == 0
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(cst[4], s), s, join_lines = true)
        else
            s.indent += s.indent_size
            add_node!(
                t,
                p_block(cst[3], s, ignore_single_line = true),
                s,
                max_padding = s.indent_size,
            )
            s.indent -= s.indent_size
            add_node!(t, pretty(cst[4], s), s)
        end
    else
        # function stub, i.e. "function foo end"
        # this should be on one line
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(cst[3], s), s, join_lines = true)
    end
    t
end
p_macro(cst::CSTParser.EXPR, s::State) = p_functiondef(cst, s)

# Struct
function p_struct(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    if cst[3].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(cst[4], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            p_block(cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(cst[4], s), s)
    end
    t
end

# Mutable
function p_mutable(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[3], s), s, join_lines = true)
    if cst[4].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(cst[5], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            p_block(cst[4], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(cst[5], s), s)
    end
    t
end

# ModuleH/BareModule
function p_module(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    if cst[3].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(cst[4], s), s, join_lines = true)
    else
        add_node!(t, pretty(cst[3], s), s, max_padding = 0)
        add_node!(t, pretty(cst[4], s), s)
    end
    t
end
p_baremodule(cst::CSTParser.EXPR, s::State) = p_module(cst, s)

# Const/Local/Global/Return
function p_const(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    if cst[2].fullspan != 0
        for a in cst.args[2:end]
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end
p_local(cst::CSTParser.EXPR, s::State) = p_const(cst, s)
p_global(cst::CSTParser.EXPR, s::State) = p_const(cst, s)
p_return(cst::CSTParser.EXPR, s::State) = p_const(cst, s)

# TopLevel
function p_toplevel(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for a in cst.args
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(a, s), s, max_padding = s.indent_size)
        add_node!(t, Semicolon(), s)
    end
    t
end

# Begin
function p_begin(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    if cst[2].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(cst[3], s), s, join_lines = true)
    else
        s.indent += s.indent_size
        add_node!(
            t,
            p_block(cst[2], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        add_node!(t, pretty(cst[3], s), s)
    end
    t
end

# Quote
function p_quote(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    if cst[1].typ === CSTParser.KEYWORD && cst[1].kind === Tokens.QUOTE
        add_node!(t, pretty(cst[1], s), s)
        if cst[2].fullspan == 0
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(cst[3], s), s, join_lines = true)
        else
            s.indent += s.indent_size
            add_node!(
                t,
                p_block(cst[2], s, ignore_single_line = true),
                s,
                max_padding = s.indent_size,
            )
            s.indent -= s.indent_size
            add_node!(t, pretty(cst[3], s), s)
        end
    else
        for a in cst.args
            add_node!(t, pretty(a, s), s, join_lines = true)
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
function p_let(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    if length(cst.args) > 3
        add_node!(t, Whitespace(1), s)
        s.indent += s.indent_size
        if cst[2].typ === CSTParser.Block
            add_node!(t, p_block(cst[2], s, join_body = true), s, join_lines = true)
        else
            add_node!(t, pretty(cst[2], s), s, join_lines = true)
        end
        s.indent -= s.indent_size

        idx = length(t.nodes)
        s.indent += s.indent_size
        add_node!(
            t,
            p_block(cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
        # Possible newline after args if nested to act as a separator
        # to the block body.
        if cst[2].typ === CSTParser.Block && t.nodes[end-2].typ !== NOTCODE
            add_node!(t.nodes[idx], Placeholder(0), s)
        end
        add_node!(t, pretty(cst.args[end], s), s)
    else
        s.indent += s.indent_size
        add_node!(t, p_block(cst[2], s, ignore_single_line = true), s)
        s.indent -= s.indent_size
        add_node!(t, pretty(cst.args[end], s), s)
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
function p_for(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    if cst[1].kind === Tokens.FOR
        eq_to_in_normalization!(cst[2], s.opts.always_for_in)
    end
    if cst[2].typ === CSTParser.Block
        s.indent += s.indent_size
        add_node!(t, p_block(cst[2], s, join_body = true), s, join_lines = true)
        s.indent -= s.indent_size
    else
        add_node!(t, pretty(cst[2], s), s, join_lines = true)
    end
    idx = length(t.nodes)
    s.indent += s.indent_size
    add_node!(
        t,
        p_block(cst[3], s, ignore_single_line = true),
        s,
        max_padding = s.indent_size,
    )
    s.indent -= s.indent_size

    # Possible newline after args if nested to act as a separator
    # to the block body.
    if cst[2].typ === CSTParser.Block && t.nodes[end-2].typ !== NOTCODE
        add_node!(t.nodes[idx], Placeholder(0), s)
    end
    add_node!(t, pretty(cst[4], s), s)
    t
end
p_while(cst::CSTParser.EXPR, s::State) = p_for(cst, s)

# Do
function p_do(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    if cst[3].fullspan != 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(cst[3], s), s, join_lines = true)
    end
    if cst[4].typ === CSTParser.Block
        s.indent += s.indent_size
        add_node!(
            t,
            p_block(cst[4], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size
    end
    add_node!(t, pretty(cst.args[end], s), s)
    t
end

# Try
function p_try(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for a in cst.args
        if a.fullspan == 0
        elseif a.typ === CSTParser.KEYWORD
            add_node!(t, pretty(a, s), s, max_padding = 0)
        elseif a.typ === CSTParser.Block
            s.indent += s.indent_size
            add_node!(
                t,
                p_block(a, s, ignore_single_line = true),
                s,
                max_padding = s.indent_size,
            )
            s.indent -= s.indent_size
        else
            len = length(t)
            add_node!(t, Whitespace(1), s)
            n = pretty(a, s)
            # "catch n"
            t.len = max(len, 5 + 1 + length(n))
            add_node!(t, n, s, join_lines = true, max_padding = 0)
        end
    end
    t
end

# If
function p_if(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    if cst[1].typ === CSTParser.KEYWORD && cst[1].kind === Tokens.IF
        add_node!(t, pretty(cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(cst[2], s), s, join_lines = true)
        s.indent += s.indent_size
        add_node!(
            t,
            p_block(cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size

        len = length(t)
        if length(cst.args) > 4
            add_node!(t, pretty(cst[4], s), s, max_padding = 0)
            if cst[4].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1), s)
                n = pretty(cst[5], s)
                add_node!(t, n, s, join_lines = true)
                # "elseif n"
                t.len = max(len, length(n))
            else
                # ELSE KEYWORD
                s.indent += s.indent_size
                add_node!(
                    t,
                    p_block(cst[5], s, ignore_single_line = true),
                    s,
                    max_padding = s.indent_size,
                )
                s.indent -= s.indent_size
            end
        end
        # END KEYWORD
        add_node!(t, pretty(cst.args[end], s), s)
    else
        # "cond" part of "elseif cond"
        t.len += 7
        add_node!(t, pretty(cst[1], s), s)

        s.indent += s.indent_size
        add_node!(
            t,
            p_block(cst[2], s, ignore_single_line = true),
            s,
            max_padding = s.indent_size,
        )
        s.indent -= s.indent_size

        len = length(t)
        if length(cst.args) > 2
            # this either else or elseif keyword
            add_node!(t, pretty(cst[3], s), s, max_padding = 0)

            if cst[3].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1), s)
                n = pretty(cst[4], s)
                add_node!(t, n, s, join_lines = true)
                # "elseif n"
                t.len = max(len, length(n))
            else
                s.indent += s.indent_size
                add_node!(
                    t,
                    p_block(cst[4], s, ignore_single_line = true),
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
function p_chainopcall(cst::CSTParser.EXPR, s::State; nonest = false, nospace = false)
    t = FST(cst, nspaces(s))
    nws = nospace ? 0 : 1
    for (i, a) in enumerate(cst)
        n = pretty(a, s)
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
p_comparison(cst::CSTParser.EXPR, s::State; nonest = false, nospace = false) =
    p_chainopcall(cst, s, nonest = nonest, nospace = nospace)

# ColonOpCall
function p_colonopcall(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices
    for a in cst
        if a.typ === CSTParser.BinaryOpCall
            n = p_binaryopcall(a, s, nonest = true, nospace = nospace)
        elseif a.typ === CSTParser.InvisBrackets
            n = p_invisbrackets(a, s, nonest = true, nospace = nospace)
        elseif a.typ === CSTParser.ChainOpCall || a.typ === CSTParser.Comparison
            n = p_chainopcall(a, s, nonest = true, nospace = nospace)
        else
            n = pretty(a, s)
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

# Kw
function p_kw(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.EQ
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end

is_str(cst::CSTParser.EXPR) = is_str_or_cmd(cst.kind) || is_str_or_cmd(cst.typ)

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

function nestable(cst::CSTParser.EXPR)
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
function p_binaryopcall(cst::CSTParser.EXPR, s::State; nonest = false, nospace = false)
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
        n = p_binaryopcall(cst[1], s, nonest = nonest, nospace = nospace_args)
    elseif cst[1].typ === CSTParser.InvisBrackets
        n = p_invisbrackets(cst[1], s, nonest = nonest, nospace = nospace_args)
    elseif cst[1].typ === CSTParser.ChainOpCall || cst[1].typ === CSTParser.Comparison
        n = p_chainopcall(cst[1], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(cst[1], s)
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
    nest = (nestable(cst) && !nonest) || nrhs
    # @info "" nestable(cst) !nonest nrhs nest cst[2]

    if op.fullspan == 0 && cst[3].typ === CSTParser.IDENTIFIER
        # do nothing
    elseif op.kind === Tokens.EX_OR
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(op, s), s, join_lines = true)
    elseif op.kind === Tokens.CIRCUMFLEX_ACCENT && op.dot
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif (
        nospace ||
        (CSTParser.precedence(op) in (8, 13, 14, 16) && op.kind !== Tokens.ANON_FUNC)
    ) && op.kind !== Tokens.IN
        add_node!(t, pretty(op, s), s, join_lines = true)
    else
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    end

    if cst[3].typ === CSTParser.BinaryOpCall
        n = p_binaryopcall(cst[3], s, nonest = nonest, nospace = nospace_args)
    elseif cst[3].typ === CSTParser.InvisBrackets
        n = p_invisbrackets(cst[3], s, nonest = nonest, nospace = nospace_args)
    elseif cst[3].typ === CSTParser.ChainOpCall || cst[3].typ === CSTParser.Comparison
        n = p_chainopcall(cst[3], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(cst[3], s)
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

# WhereOpCall
# A where B
function p_whereopcall(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)

    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
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
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
            s.indent += s.indent_size
        elseif is_closer(a) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(a, s), s, join_lines = true)
            s.indent -= s.indent_size
        elseif CSTParser.is_comma(a) && !is_punc(cst[i+3])
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(
                t,
                p_binaryopcall(a, s, nospace = !s.opts.whitespace_typedefs),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
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

# Conditional
function p_conditionalopcall(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    add_node!(t, Placeholder(1), s)

    add_node!(t, pretty(cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[4], s), s, join_lines = true)
    add_node!(t, Placeholder(1), s)

    add_node!(t, pretty(cst[5], s), s, join_lines = true)
    t
end

# UnaryOpCall
function p_unaryopcall(cst::CSTParser.EXPR, s::State; nospace = true)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    !nospace && add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)
    t
end

function p_curly(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)

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
            add_node!(t, pretty(a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end

function p_call(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, pretty(cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))

    if nest
        add_node!(t, Placeholder(0), s)
    end

    for (i, a) in enumerate(cst.args[3:end])
        if i + 2 == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end

# InvisBrackets
function p_invisbrackets(cst::CSTParser.EXPR, s::State; nonest = false, nospace = false)
    t = FST(cst, nspaces(s))
    nest = !is_iterable(cst[2]) && !nonest
    # @info "nest invis" nonest

    for (i, a) in enumerate(cst)
        if a.typ === CSTParser.Block
            add_node!(t, p_block(a, s, from_quote = true), s, join_lines = true)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(
                t,
                p_binaryopcall(a, s, nonest = nonest, nospace = nospace),
                s,
                join_lines = true,
            )
        elseif a.typ === CSTParser.InvisBrackets
            add_node!(
                t,
                p_invisbrackets(a, s, nonest = nonest, nospace = nospace),
                s,
                join_lines = true,
            )
        elseif is_opener(a) && nest
            # @info "opening"
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(a) && nest
            # @info "closing"
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(a, s), s, join_lines = true)
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end

# TupleH
function p_tupleh(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))

    for (i, a) in enumerate(cst)
        n = pretty(a, s)
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

# Braces
function p_braces(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_arg(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(a, s)
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

# Vect
function p_vect(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_arg(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(a, s)
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
p_comprehension(cst::CSTParser.EXPR, s::State) = p_vect(cst, s)


# Parameters
function p_parameters(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(a, s)
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

# Import, Export, Using, ImportAll
function p_import(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for (i, a) in enumerate(cst.args[2:end])
        if CSTParser.is_comma(a) || CSTParser.is_colon(a)
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end
p_export(cst::CSTParser.EXPR, s::State) = p_import(cst, s)
p_using(cst::CSTParser.EXPR, s::State) = p_import(cst, s)
p_importall(cst::CSTParser.EXPR, s::State) = p_import(cst, s)

# Ref
function p_ref(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 5 && !(length(cst) == 5 && unnestable_arg(cst[3]))
    nospace = !s.opts.whitespace_ops_in_indices
    for (i, a) in enumerate(cst)
        if is_closer(a) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(a, s), s, join_lines = true)
        elseif is_opener(a) && nest
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(
                t,
                p_binaryopcall(a, s, nonest = true, nospace = nospace),
                s,
                join_lines = true,
            )
        elseif a.typ === CSTParser.InvisBrackets
            add_node!(
                t,
                p_invisbrackets(a, s, nonest = true, nospace = nospace),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end

# Vcat/TypedVcat
function p_vcat(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    st = cst.typ === CSTParser.Vcat ? 1 : 2
    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))
    # @info "" nest length(cst) st

    for (i, a) in enumerate(cst)
        n = pretty(a, s)
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
p_typedvcat(cst::CSTParser.EXPR, s::State) = p_vcat(cst, s)

# Hcat/TypedHcat
function p_hcat(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    st = cst.typ === CSTParser.Hcat ? 1 : 2
    for (i, a) in enumerate(cst)
        if i > st && i < length(cst) - 1
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end
p_typedhcat(cst::CSTParser.EXPR, s::State) = p_hcat(cst, s)

# Row
function p_row(cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    # Currently {A <:B} is parsed as a Row type with elements A and <:B
    # instead of a BinaryOpCall A <: B, which is inconsistent with Meta.parse.
    #
    # This is used to overcome that current limitation.
    in_braces = cst.parent === nothing ? false : cst.parent.typ === CSTParser.BracesCat
    nospace = !s.opts.whitespace_typedefs

    for (i, a) in enumerate(cst)
        if in_braces && i < length(cst) && cst[i+1].typ === CSTParser.UnaryOpCall
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Whitespace(nospace ? 0 : 1), s)
        elseif in_braces && a.typ === CSTParser.UnaryOpCall
            add_node!(t, p_unaryopcall(a, s, nospace = nospace), s, join_lines = true)
            i < length(cst) && add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
            i < length(cst) && add_node!(t, Whitespace(1), s)
        end
    end
    t
end

# Generator/Filter
function p_generator(cst::CSTParser.EXPR, s::State)
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

            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
            if a.kind === Tokens.FOR
                for j = i+1:length(cst)
                    eq_to_in_normalization!(cst[j], s.opts.always_for_in)
                end
            end
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(t, p_binaryopcall(a, s, nonest = true), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(a, s), s, join_lines = true)
        end
    end
    t
end
p_filter(cst::CSTParser.EXPR, s::State) = p_generator(cst, s)
