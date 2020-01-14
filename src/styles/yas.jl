# YAS style !!!

struct YASStyle <: AbstractStyle end
@inline getstyle(s::YASStyle) = s

yasformat(s::AbstractString; kwargs...) = format_text(
    s;
    kwargs...,
    always_for_in = true,
    whitespace_ops_in_indices = true,
    whitespace_typedefs = false,
    style = YASStyle(),
)

nestable(::YASStyle, cst::CSTParser.EXPR) = false

function p_kw(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for a in cst
        add_node!(t, pretty(ys, a, s), s, join_lines = true)
    end
    t
end

function p_curly(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(ys, a, s)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i == length(cst) - 1
            # remove trailing comma
        elseif is_closer(n) || is_opener(n)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = t.endline == n.startline)
        end
    end
    t
end
@inline p_braces(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_curly(ys, cst, s)

function p_call(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    for (i, a) in enumerate(cst)
        n = pretty(ys, a, s)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i == length(cst) - 1
            # remove trailing comma
        elseif is_closer(n) || is_opener(n)
            add_node!(t, n, s, join_lines = true)
        elseif n.typ === CSTParser.Parameters
            join_lines = t.endline == n.startline
            add_node!(t, n, s, join_lines = join_lines)
        else
            join_lines = t.endline == n.startline
            if join_lines && !is_opener(t[end])
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s, join_lines = join_lines)
        end
    end
    t
end
@inline p_ref(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)
@inline p_vect(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)
@inline p_tupleh(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)
@inline p_parameters(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)
@inline p_comprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)

function p_whereopcall(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(ys, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(ys, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    add_braces =
        !CSTParser.is_lbrace(cst[3]) && cst.parent.typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.Curly && cst[3].typ !== CSTParser.BracesCat

    brace = FST(CSTParser.PUNCTUATION, t.endline, t.endline, "{")
    add_braces && add_node!(t, brace, s, join_lines = true)

    for (i, a) in enumerate(cst.args[3:end])
        n = a.typ === CSTParser.BinaryOpCall ? pretty(ys, a, s, nospace = true) :
            pretty(ys, a, s)
        if is_closer(n) || is_opener(n)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && !is_punc(cst[i+3])
            add_node!(t, n, s, join_lines = true)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(t, n, s, join_lines = t.endline == n.startline)
        else
            add_node!(t, n, s, join_lines = t.endline == n.startline)
        end
    end
    brace = FST(CSTParser.PUNCTUATION, t.endline, t.endline, "}")
    add_braces && add_node!(t, brace, s, join_lines = true)
    t
end

function p_generator(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = a.typ === CSTParser.BinaryOpCall ? pretty(ys, a, s, nonest = true) :
            pretty(ys, a, s)
        if a.typ === CSTParser.KEYWORD
            add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
            # add_node!(t, Whitespace(1), s)
            if a.kind === Tokens.FOR
                for j = i+1:length(cst)
                    eq_to_in_normalization!(cst[j], s.opts.always_for_in)
                end
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        else
            join_lines = t.endline == n.startline
            join_lines && add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = join_lines)
        end
    end
    t
end
@inline p_filter(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_generator(ys, cst, s)

#
# Nesting
#

function nest_if_over_margin!(style, fst::FST, s::State, i::Int)
    margin = s.line_offset
    idx = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
    if idx === nothing
        margin += sum(length.(fst[i+1:end])) + fst.extra_margin
    else
        margin += sum(length.(fst[i+1:idx]))
    end
    if margin > s.margin || is_comment(fst[i+1])
        fst[i] = Newline(length = fst[i].len)
        s.line_offset = fst.indent
    else
        nest!(style, fst[i], s)
    end
end

function n_call!(ys::YASStyle, fst::FST, s::State)
    line_offset = s.line_offset
    fst.indent = line_offset + sum(length.(fst[1:2]))

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === TRAILINGSEMICOLON
            n.val = ""
            n.len = 0
            nest!(ys, n, s)
        elseif n.typ === CSTParser.Parameters || n.typ === CSTParser.Generator
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(ys, n, s)
        else
            n.extra_margin = 1
            nest!(ys, n, s)
        end
    end
end
@inline n_curly!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)
@inline n_ref!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)

function n_tupleh!(ys::YASStyle, fst::FST, s::State)
    line_offset = s.line_offset
    fst.typ !== CSTParser.Parameters && (fst.indent = line_offset)
    length(fst.nodes) > 0 && is_opener(fst[1]) && (fst.indent += 1)

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === TRAILINGSEMICOLON
            n.val = ""
            n.len = 0
            nest!(ys, n, s)
        elseif n.typ === CSTParser.Generator
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(ys, n, s)
        else
            n.extra_margin = 1
            nest!(ys, n, s)
        end
    end
end
@inline n_braces!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
@inline n_vect!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
@inline n_parameters!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
@inline n_invisbrackets!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
@inline n_comprehension!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)

function n_import!(ys::YASStyle, fst::FST, s::State)
    fst.indent = s.line_offset + sum(length.(fst[1:2]))
    for (i, n) in enumerate(fst.nodes)
        if n.typ === PLACEHOLDER
            nest_if_over_margin!(ys, fst, s, i)
        elseif n.typ === NEWLINE
            s.line_offset = fst.indent
        else
            nest!(ys, n, s)
        end
    end
end
@inline n_export!(ys::YASStyle, fst::FST, s::State) = n_import!(ys, fst, s)
@inline n_using!(ys::YASStyle, fst::FST, s::State) = n_import!(ys, fst, s)

n_whereopcall!(ys::YASStyle, fst::FST, s::State) =
    nest!(ys, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
n_chainopcall!(ys::YASStyle, fst::FST, s::State) =
    n_block!(DefaultStyle(ys), fst, s, custom_indent = s.line_offset)
