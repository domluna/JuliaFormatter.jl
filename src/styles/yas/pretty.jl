"""
    YASStyle()

Formatting style based on [YASGuide](https://github.com/jrevels/YASGuide)
and [JuliaFormatter#198](https://github.com/domluna/JuliaFormatter.jl/issues/198).

Configurable options with different defaults to [`DefaultStyle`](@ref) are:
- `always_for_in` = true
- `whitespace_ops_in_indices` = true
- `remove_extra_newlines` = true
- `import_to_using` = true
- `pipe_to_function_call` = true
- `short_to_long_function_def` = true
- `always_use_return` = true
- `whitespace_in_kwargs` = false
"""
struct YASStyle <: AbstractStyle
    innerstyle::Union{Nothing,AbstractStyle}
end
YASStyle() = YASStyle(nothing)
@inline getstyle(s::YASStyle) = s.innerstyle === nothing ? s : s.innerstyle

function options(style::YASStyle)
    return (;
        always_for_in = true,
        whitespace_ops_in_indices = true,
        remove_extra_newlines = true,
        import_to_using = true,
        pipe_to_function_call = true,
        short_to_long_function_def = true,
        always_use_return = true,
        whitespace_in_kwargs = false,
    )
end

function nestable(::YASStyle, cst::CSTParser.EXPR)
    (CSTParser.defines_function(cst) || nest_assignment(cst)) && return false
    (cst[2].kind === Tokens.PAIR_ARROW || cst[2].kind === Tokens.ANON_FUNC) && return false
    return true
end

function p_import(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for (i, a) in enumerate(cst.args[2:end])
        if CSTParser.is_comma(a)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif CSTParser.is_colon(a)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
@inline p_using(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_import(ys, cst, s)
@inline p_export(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_import(ys, cst, s)

function p_curly(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
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
    style = getstyle(ys)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            n = pretty(style, a, s)
            if n_args(cst) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(cst[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif a.typ === CSTParser.BinaryOpCall && a[2].kind === Tokens.EQ
            add_node!(t, p_kw(style, a, s), s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

function p_tupleh(ys::YASStyle, nodes::Vector{CSTParser.EXPR}, s::State)
    style = getstyle(ys)
    t = FST(CSTParser.TupleH, nspaces(s))
    for (i, a) in enumerate(nodes)
        n = pretty(style, a, s)
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
    style = getstyle(ys)
    t = p_invisbrackets(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)
    for (i, n) in enumerate(t.nodes)
        if n.typ === PLACEHOLDER
            t[i] = Whitespace(length(n))
        end
    end
    t
end

function p_call(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    if !parent_is(cst, is_function_or_macro_def)
        separate_kwargs_with_semicolon!(t)
    end
    t
end
@inline p_vect(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_call(ys, cst, s)

function p_vcat(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))
    st = cst.typ === CSTParser.Vcat ? 1 : 2

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if !is_closer(a) && i > st
            join_lines = i == st + 1 ? true : t.endline == n.startline
            if join_lines && i != st + 1
                add_node!(t, Placeholder(1), s)
            end

            add_node!(t, n, s; join_lines = join_lines)
            if i != length(cst) - 1
                has_semicolon(s.doc, n.startline) &&
                    add_node!(t, InverseTrailingSemicolon(), s)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
@inline p_typedvcat(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_vcat(ys, cst, s)

function p_ref(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices

    for (i, a) in enumerate(cst)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif a.typ === CSTParser.BinaryOpCall ||
               a.typ === CSTParser.InvisBrackets ||
               a.typ === CSTParser.ChainOpCall ||
               a.typ === CSTParser.Comparison
            n = pretty(style, a, s, nonest = true, nospace = nospace)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end

function p_comprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))

    if is_block(cst[2]) || (cst[2].typ === CSTParser.Generator && is_block(cst[2][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    t
end

function p_typedcomprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))

    if is_block(cst[3]) || (cst[3].typ === CSTParser.Generator && is_block(cst[3][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    t
end

function p_macrocall(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    fst = p_macrocall(DefaultStyle(style), cst, s)
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

function p_whereopcall(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))

    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    add_braces =
        !CSTParser.is_lbrace(cst[3]) &&
        cst.parent.typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.BracesCat

    if add_braces
        brace = FST(CSTParser.PUNCTUATION, -1, t.endline, t.endline, "{")
        add_node!(t, brace, s, join_lines = true)
    end

    for (i, a) in enumerate(cst.args[3:end])
        n =
            a.typ === CSTParser.BinaryOpCall ? pretty(style, a, s, nospace = true) :
            pretty(style, a, s)

        if CSTParser.is_comma(a) && i + 2 == length(cst) - 1
            continue
        elseif CSTParser.is_comma(a) && i + 2 < length(cst) && !is_punc(cst[i+3])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end

    if add_braces
        brace = FST(CSTParser.PUNCTUATION, -1, t.endline, t.endline, "}")
        add_node!(t, brace, s, join_lines = true)
    end
    t
end

function p_generator(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(cst, nspaces(s))
    has_for_kw = false

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if a.typ === CSTParser.KEYWORD
            if !has_for_kw && a.kind === Tokens.FOR
                has_for_kw = true
            end

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

            if !is_gen(cst.args[i+1])
                tup = p_tupleh(style, cst.args[i+1:length(cst)], s)
                has_for_kw && eq_to_in_normalization!(
                    tup,
                    s.opts.always_for_in,
                    s.opts.for_in_replacement,
                )
                add_node!(t, tup, s, join_lines = true)
                break
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end

        has_for_kw &&
            eq_to_in_normalization!(n, s.opts.always_for_in, s.opts.for_in_replacement)
    end

    t
end
@inline p_filter(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_generator(ys, cst, s)
@inline p_flatten(ys::YASStyle, cst::CSTParser.EXPR, s::State) = p_generator(ys, cst, s)
