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

function is_binaryop_nestable(::YASStyle, cst::CSTParser.EXPR)
    (CSTParser.defines_function(cst) || is_assignment(cst)) && return false
    (cst[2].val == "=>" || cst[2].val == "->") && return false
    return true
end

function p_import(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = p_import(DefaultStyle(style), cst, s)
    idx = findfirst(n -> n.typ === PLACEHOLDER, t.nodes)
    if idx !== nothing && is_colon(t[idx-1])
        t[idx] = Whitespace(1)
    end
    t
end

function p_using(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = p_import(ys, cst, s)
    t.typ = Using
    t
end

function p_export(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = p_import(ys, cst, s)
    t.typ = Export
    t
end

function p_curly(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(Curly, cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if CSTParser.is_comma(a) && i == length(cst) - 1
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(n)
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif i > 1 && is_opener(cst[i-1])
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

function p_braces(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = p_curly(ys, cst, s)
    t.typ = Braces
    t
end

function p_tuple(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(TupleN, cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = if is_binary(a) && a[2].val == "="
            p_kw(style, a, s)
        else
            pretty(style, a, s)
        end

        if CSTParser.is_comma(a) && i + 1 == length(cst)
            if length(cst.args) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(cst[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif is_closer(n)
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif i > 1 && is_opener(cst[i-1])
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

function p_tuple(ys::YASStyle, nodes::Vector{CSTParser.EXPR}, s::State)
    style = getstyle(ys)
    t = FST(TupleN, nspaces(s))
    for (i, a) in enumerate(nodes)
        n = pretty(style, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(nodes)
            if length(nodes) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(nodes[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(nodes) && !is_punc(nodes[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif is_closer(n)
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif i > 1 && is_opener(nodes[i-1])
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

# Brackets
function p_invisbrackets(
    ys::YASStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(ys)
    t = FST(Brackets, cst, nspaces(s))

    if is_block(cst[2]) || (cst[2].head === :generator && is_block(cst[2][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(
        t,
        pretty(style, cst[1], s),
        s,
        join_lines = true,
        override_join_lines_based_on_source = true,
    )

    n = if cst[2].head === :block
        pretty(style, cst[2], s, from_quote = true)
    elseif is_opcall(cst[2])
        pretty(style, cst[2], s, nonest = nonest, nospace = nospace)
    else
        pretty(style, cst[2], s)
    end
    add_node!(t, n, s, join_lines = true, override_join_lines_based_on_source = true)

    add_node!(
        t,
        pretty(style, cst[3], s),
        s,
        join_lines = true,
        override_join_lines_based_on_source = true,
    )

    t
end

function p_call(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(Call, cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif is_closer(n)
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif i > 1 && is_opener(cst[i-1])
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    if !parent_is(cst, is_function_or_macro_def)
        separate_kwargs_with_semicolon!(t)
    end
    t
end
function p_vect(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = p_call(ys, cst, s)
    t.typ = Vect
    t
end

function p_vcat(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(Vcat, nspaces(s))
    st = cst.head === :vcat ? 1 : 2

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if is_closer(n)
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif !is_closer(a) && i > st
            join_lines = i == st + 1 ? true : t.endline == n.startline
            if join_lines && i != st + 1
                add_node!(t, Placeholder(1), s)
            end

            add_node!(
                t,
                n,
                s;
                join_lines = join_lines,
                override_join_lines_based_on_source = i == st + 1,
            )
            if has_semicolon(s.doc, n.startline)
                semicolons = s.doc.semicolons[n.startline]
                count = popfirst!(semicolons)
                if i != length(cst) - 1
                    if count > 1
                        for _ = 1:count
                            add_node!(t, Semicolon(), s)
                        end
                    else
                        add_node!(t, InverseTrailingSemicolon(), s)
                    end
                elseif count > 1
                    for _ = 1:count
                        add_node!(t, Semicolon(), s)
                    end
                end
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
function p_typedvcat(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = p_vcat(ys, cst, s)
    t.typ = TypedVcat
    t
end

function p_ref(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(RefN, cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices

    for (i, a) in enumerate(cst)
        n = if is_binary(a) || is_chain(a) || a.head === :brackets || a.head === :comparison
            pretty(style, a, s, nonest = true, nospace = nospace)
        else
            pretty(style, a, s)
        end

        if CSTParser.is_comma(a) && i + 1 == length(cst)
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif is_closer(n)
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif i > 1 && is_opener(cst[i-1])
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end

function p_comprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(Comprehension, cst, nspaces(s))

    if is_block(cst[2]) || (cst[2].head === :Generator && is_block(cst[2][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    add_node!(
        t,
        pretty(style, cst[2], s),
        s,
        join_lines = true,
        override_join_lines_based_on_source = true,
    )
    add_node!(
        t,
        pretty(style, cst[3], s),
        s,
        join_lines = true,
        override_join_lines_based_on_source = true,
    )
    t
end

function p_typedcomprehension(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(TypedComprehension, cst, nspaces(s))

    if is_block(cst[3]) || (cst[3].head === :generator && is_block(cst[3][1]))
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
    t = FST(MacroCall, cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))
    has_closer = is_closer(cst[end])

    !has_closer && (t.typ = MacroBlock)

    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(cst)
        if CSTParser.is_nothing(a)
            s.offset += a.fullspan
            continue
        end

        n = pretty(style, a, s)
        if CSTParser.ismacroname(a)
            if has_closer || length(args) == 0
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif is_closer(n)
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif i > 1 && is_opener(cst[i-1])
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif t.typ === MacroBlock
            if has_closer
                add_node!(t, n, s, join_lines = true)
                if i < length(cst) - 1 && cst[i+1].head != :parameters
                    add_node!(t, Whitespace(1), s)
                end
            else
                padding = is_block(n) ? 0 : -1
                add_node!(t, n, s, join_lines = true, max_padding = padding)
                i < length(cst) && add_node!(t, Whitespace(1), s)
            end
        else
            if has_closer
                add_node!(t, n, s, join_lines = true)
            else
                padding = is_block(n) ? 0 : -1
                add_node!(t, n, s, join_lines = true, max_padding = padding)
            end
        end
    end
    # move placement of @ to the end
    #
    # @Module.macro -> Module.@macro
    t[1] = move_at_sign_to_the_end(t[1], s)
    t
end

function p_whereopcall(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(Where, cst, nspaces(s))

    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    curly_ctx =
        cst.parent.head === :curly ||
        cst[3].head === :curly ||
        cst[3].head === :bracescat ||
        cst[3].head === :parameters

    add_braces = !curly_ctx && !CSTParser.is_lbrace(cst[3])

    bc = curly_ctx ? t : FST(BracesCat, nspaces(s))

    if add_braces
        brace = FST(PUNCTUATION, -1, t.endline, t.endline, "{")
        add_node!(bc, brace, s, join_lines = true)
    end

    for i = 3:length(cst)
        a = cst[i]
        n = is_binary(a) ? pretty(style, a, s, nospace = true) : pretty(style, a, s)

        if CSTParser.is_comma(a) && i == length(cst) - 1
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(bc, n, s, join_lines = true)
            add_node!(bc, Placeholder(0), s)
        elseif is_closer(n)
            add_node!(
                bc,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif is_opener(cst[i-1])
            add_node!(
                bc,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        else
            add_node!(bc, n, s, join_lines = true)
        end
    end

    if add_braces
        brace = FST(PUNCTUATION, -1, t.endline, t.endline, "}")
        add_node!(bc, brace, s, join_lines = true)
    end

    !curly_ctx && add_node!(t, bc, s, join_lines = true)

    t
end

function p_generator(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(Generator, cst, nspaces(s))
    has_for_kw = false

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if CSTParser.iskeyword(a)
            if !has_for_kw && a.head === :FOR
                has_for_kw = true
            end

            incomp =
                parent_is(a, is_iterable, ignore = n -> is_gen(n) || n.head === :brackets)

            if is_block(cst[i-1])
                add_node!(t, Newline(), s)
            elseif incomp
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)

            if !is_gen(cst[i+1])
                tupargs = CSTParser.EXPR[]
                for j = i+1:length(cst)
                    push!(tupargs, cst[j])
                end

                # verify this is not another for loop
                any(b -> b.head === :FOR, tupargs) && continue

                tup = p_tuple(style, tupargs, s)
                add_node!(t, tup, s, join_lines = true)

                if has_for_kw
                    for nn in tup.nodes
                        eq_to_in_normalization!(nn, s.opts.always_for_in)
                    end
                end
                break
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end

        has_for_kw && eq_to_in_normalization!(n, s.opts.always_for_in)
    end

    t
end

function p_filter(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = p_generator(ys, cst, s)
    t.typ = Filter
    t
end

function p_flatten(ys::YASStyle, cst::CSTParser.EXPR, s::State)
    t = p_generator(ys, cst, s)
    t.typ = Flatten
    t
end
