# Experimental
#
# YAS style !!!

"""
    YASStyle

**This style is EXPERIMENTAL !!!**

Formatting style based on https://github.com/jrevels/YASGuide.

Recommended options are:

- `always_for_in` = true,
- `whitespace_ops_in_indices` = true,
- `whitespace_typedefs` = false,
- `remove_extra_newlines` = true,
"""
struct YASStyle <: AbstractStyle end
@inline getstyle(s::YASStyle) = s

yasformat(s::AbstractString; kwargs...) = format_text(
    s;
    kwargs...,
    always_for_in = true,
    whitespace_ops_in_indices = true,
    whitespace_typedefs = false,
    remove_extra_newlines = true,
    style = YASStyle(),
)

function nestable(::YASStyle, cst::CSTParser.EXPR)
    (CSTParser.defines_function(cst) || nest_assignment(cst)) && return false
    (cst[2].kind === Tokens.PAIR_ARROW || cst[2].kind === Tokens.ANON_FUNC) && return false
    return true
end

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
        if CSTParser.is_comma(a) && i == length(cst) - 1
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
@inline p_braces(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_curly(ys, cst, s)

function p_tupleh(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(ys, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            if n_args(cst) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(cst[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

function p_call(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(ys, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
@inline p_ref(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)
@inline p_vect(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)
@inline p_comprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)
@inline p_typedcomprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State) =
    p_call(ys, cst, s)

function p_macrocall(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    fst = p_macrocall(DefaultStyle(ys), cst, s)
    fst.typ == CSTParser.MacroCall || return fst
    is_closer(cst.args[end]) || return fst

    # remove initial and last placeholders
    # @call(PLACEHOLDER, args..., PLACEHOLDER) -> @call(args...)
    idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
    idx !== nothing && (fst[idx] = Whitespace(0))
    idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes)
    idx !== nothing && (fst[idx] = Whitespace(0))

    return fst
end


function p_parameters(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(ys, a, s)
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

function p_whereopcall(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(ys, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(ys, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    add_braces =
        !CSTParser.is_lbrace(cst[3]) &&
        cst.parent.typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.Curly && cst[3].typ !== CSTParser.BracesCat

    brace = FST(CSTParser.PUNCTUATION, t.endline, t.endline, "{")
    add_braces && add_node!(t, brace, s, join_lines = true)

    for (i, a) in enumerate(cst.args[3:end])
        n = a.typ === CSTParser.BinaryOpCall ? pretty(ys, a, s, nospace = true) :
            pretty(ys, a, s)

        if CSTParser.is_comma(a) && i + 2 == length(cst) - 1
            continue
        elseif CSTParser.is_comma(a) && i + 2 < length(cst) && !is_punc(cst[i+3])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        else
            add_node!(t, n, s, join_lines = true)
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
            add_node!(t, Placeholder(1), s)
            if a.kind === Tokens.FOR
                for j = i+1:length(cst)
                    eq_to_in_normalization!(cst[j], s.opts.always_for_in)
                end
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
@inline p_filter(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_generator(ys, cst, s)
@inline p_flatten(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_generator(ys, cst, s)

#
# Nesting
#

function n_call!(ys::YASStyle, fst::FST, s::State)
    fst.indent = s.line_offset + sum(length.(fst[1:2]))

    @info "" fst[1].val fst.extra_margin

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            si = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
            nest_if_over_margin!(ys, fst, s, i; stop_idx = si)
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
@inline n_curly!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)
@inline n_ref!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)
@inline n_macrocall!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)
@inline n_typedcomprehension!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)

function n_tupleh!(ys::YASStyle, fst::FST, s::State)
    fst.indent = s.line_offset
    length(fst.nodes) > 0 && is_opener(fst[1]) && (fst.indent += 1)

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            si = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
            nest_if_over_margin!(ys, fst, s, i; stop_idx = si)
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
# @inline n_generator!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
# @inline n_filter!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
# @inline n_flatten!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)

function n_whereopcall!(ys::YASStyle, fst::FST, s::State)
    fst.indent = s.line_offset

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            si = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
            nest_if_over_margin!(ys, fst, s, i; stop_idx = si)
        elseif is_opener(n) && n.val == "{"
            fst.indent = s.line_offset + 1
            nest!(ys, n, s)
        elseif i == 1
            nest!(ys, n, s)
        else
            n.extra_margin = 1
            nest!(ys, n, s)
        end
    end
end


function n_using!(ys::YASStyle, fst::FST, s::State)
    fst.indent = s.line_offset + sum(length.(fst[1:2]))
    for (i, n) in enumerate(fst.nodes)
        if n.typ === PLACEHOLDER
            si = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
            nest_if_over_margin!(ys, fst, s, i; stop_idx = si)
        elseif n.typ === NEWLINE
            s.line_offset = fst.indent
        else
            nest!(ys, n, s)
        end
    end
end
@inline n_export!(ys::YASStyle, fst::FST, s::State) = n_using!(ys, fst, s)
@inline n_import!(ys::YASStyle, fst::FST, s::State) = n_using!(ys, fst, s)

# n_chainopcall!(ys::YASStyle, fst::FST, s::State) =
#     n_block!(DefaultStyle(ys), fst, s, indent = s.line_offset)
# n_comparison!(ys::YASStyle, fst::FST, s::State) =
#     n_block!(DefaultStyle(ys), fst, s, indent = s.line_offset)
