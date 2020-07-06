"""
    YASStyle()

Formatting style based on https://github.com/jrevels/YASGuide
and https://github.com/domluna/JuliaFormatter.jl/issues/198.

Recommended options are:

- `always_for_in` = true
- `whitespace_ops_in_indices` = true
- `whitespace_typedefs` = false
- `remove_extra_newlines` = true
- `import_to_using` = true
- `pipe_to_function_call` = true
- `short_to_long_function_def` = true
- `always_use_return` = true
- `whitespace_in_kwargs` = false
"""
struct YASStyle <: AbstractStyle end
@inline getstyle(s::YASStyle) = s

function nestable(::YASStyle, cst::CSTParser.EXPR)
    (CSTParser.defines_function(cst) || nest_assignment(cst)) && return false
    (cst[2].kind === Tokens.PAIR_ARROW || cst[2].kind === Tokens.ANON_FUNC) && return false
    return true
end

function p_import(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(ys, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for (i, a) in enumerate(cst.args[2:end])
        if CSTParser.is_comma(a)
            add_node!(t, pretty(ys, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif CSTParser.is_colon(a)
            add_node!(t, pretty(ys, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(ys, a, s), s, join_lines = true)
        end
    end
    t
end
@inline p_using(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_import(ys, cst, s)
@inline p_export(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_import(ys, cst, s)

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
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            n = pretty(ys, a, s)
            if n_args(cst) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(cst[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(ys, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif a.typ === CSTParser.BinaryOpCall && a[2].kind === Tokens.EQ
            add_node!(t, p_kw(ys, a, s), s, join_lines = true)
        else
            add_node!(t, pretty(ys, a, s), s, join_lines = true)
        end
    end
    t
end

function p_tupleh(ys::YASStyle, nodes::Vector{CSTParser.EXPR}, s::State)
    t = FST(CSTParser.TupleH, nspaces(s))
    for (i, a) in enumerate(nodes)
        n = pretty(ys, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(nodes)
            if n_args(nodes) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(nodes[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(nodes) && !is_punc(nodes[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

# InvisBrackets
function p_invisbrackets(
    ys::YASStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    t = p_invisbrackets(DefaultStyle(ys), cst, s, nonest = nonest, nospace = nospace)
    for (i, n) in enumerate(t.nodes)
        if n.typ === PLACEHOLDER
            t[i] = Whitespace(length(n))
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
@inline p_vect(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)

function p_ref(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices

    for (i, a) in enumerate(cst)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(ys, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif a.typ === CSTParser.BinaryOpCall ||
               a.typ === CSTParser.InvisBrackets ||
               a.typ === CSTParser.ChainOpCall ||
               a.typ === CSTParser.Comparison

            n = pretty(ys, a, s, nonest = true, nospace = nospace)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, pretty(ys, a, s), s, join_lines = true)
        end
    end
    t
end

function p_comprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    if is_block(cst[2])
        t.force_nest = true
    elseif cst[2].typ === CSTParser.Generator && is_block(cst[2][1])
        t.force_nest = true
    end

    add_node!(t, pretty(ys, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(ys, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(ys, cst[3], s), s, join_lines = true)
    t
end

function p_typedcomprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    if is_block(cst[3])
        t.force_nest = true
    elseif cst[3].typ === CSTParser.Generator && is_block(cst[3][1])
        t.force_nest = true
    end

    add_node!(t, pretty(ys, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(ys, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(ys, cst[3], s), s, join_lines = true)
    add_node!(t, pretty(ys, cst[4], s), s, join_lines = true)
    t
end

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
        cst[3].typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.BracesCat

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
        # n = a.typ === CSTParser.BinaryOpCall ? pretty(ys, a, s, nonest = true) :
        #     pretty(ys, a, s)
        n = pretty(ys, a, s)
        if a.typ === CSTParser.KEYWORD
            incomp = parent_is(
                a,
                is_iterable,
                ignore = n -> is_gen(n) || n.typ === CSTParser.InvisBrackets,
            )

            if is_block(cst[i-1])
                add_node!(t, Newline(), s)
            elseif incomp
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)

            if a.kind === Tokens.FOR
                for j = i+1:length(cst)
                    eq_to_in_normalization!(cst[j], s.opts.always_for_in)
                end
            end

            if !is_gen(cst.args[i+1])
                tup = p_tupleh(ys, cst.args[i+1:length(cst)], s)
                add_node!(t, tup, s, join_lines = true)
                return t
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
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(ys, n, s)
        else
            diff = fst.indent - fst[i].indent
            add_indent!(n, s, diff)
            n.extra_margin = 1
            nest!(ys, n, s)
        end
    end

    if is_closer(fst[end])
        fst[end].indent = fst.indent -1 
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
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(ys, n, s)
        else
            diff = fst.indent - fst[i].indent
            add_indent!(n, s, diff)
            n.extra_margin = 1
            nest!(ys, n, s)
        end
    end

    if is_closer(fst[end])
        fst[end].indent = fst.indent -1 
    end
end
@inline n_braces!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
@inline n_vect!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
@inline n_parameters!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
@inline n_invisbrackets!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)
@inline n_comprehension!(ys::YASStyle, fst::FST, s::State) = n_tupleh!(ys, fst, s)

function n_generator!(ys::YASStyle, fst::FST, s::State)
    diff = s.line_offset - fst[1].indent

    # if the first argument is not a leaf
    # aligns it to be inside the generator
    # expression
    add_indent!(fst[1], s, diff)

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            si = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
            nest_if_over_margin!(ys, fst, s, i; stop_idx = si)
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(ys, n, s)
        else
            n.extra_margin = 1
            nest!(ys, n, s)
        end
    end
end
@inline n_filter!(ys::YASStyle, fst::FST, s::State) = n_generator!(ys, fst, s)
@inline n_flatten!(ys::YASStyle, fst::FST, s::State) = n_generator!(ys, fst, s)

function n_whereopcall!(ys::YASStyle, fst::FST, s::State)
    fst.indent = s.line_offset
    # after "A where "
    Blen = sum(length.(fst[2:end]))
    fst[1].extra_margin = Blen + fst.extra_margin

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
    idx = findfirst(n -> n.val == ":", fst.nodes)
    fst.indent = s.line_offset
    if idx === nothing
        fst.indent += sum(length.(fst[1:2]))
    else
        fst.indent += sum(length.(fst[1:idx+1]))
    end
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

n_chainopcall!(ys::YASStyle, fst::FST, s::State) =
    n_block!(DefaultStyle(ys), fst, s, indent = s.line_offset)
n_comparison!(ys::YASStyle, fst::FST, s::State) =
    n_block!(DefaultStyle(ys), fst, s, indent = s.line_offset)

function n_binaryopcall!(ys::YASStyle, fst::FST, s::State)
    idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)

    if idx !== nothing
        n_binaryopcall!(DefaultStyle(ys), fst, s)
        return
    end

    # line_offset = s.line_offset
    walk(reset_line_offset!, fst.nodes[1:end-1], s, fst.indent)
    nest!(ys, fst[end], s)
    # s.line_offset = line_offset
    # nest!(ys, fst[1], s)
    # s.line_offset = line_offset
    # walk(reset_line_offset!, fst, s)

end
