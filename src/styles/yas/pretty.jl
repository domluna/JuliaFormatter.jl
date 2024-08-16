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
- `join_lines_based_on_source` = true
- `separate_kwargs_with_semicolon` = true
"""
struct YASStyle <: AbstractStyle
    innerstyle::AbstractStyle
end
YASStyle() = YASStyle(NoopStyle())

function options(::YASStyle)
    return (;
        always_for_in = true,
        whitespace_ops_in_indices = true,
        remove_extra_newlines = true,
        import_to_using = true,
        pipe_to_function_call = true,
        short_to_long_function_def = true,
        long_to_short_function_def = false,
        always_use_return = true,
        whitespace_in_kwargs = false,
        join_lines_based_on_source = true,
        separate_kwargs_with_semicolon = true,
        indent = 4,
        margin = 92,
        whitespace_typedefs = false,
        annotate_untyped_fields_with_any = true,
        format_docstrings = false,
        align_struct_field = false,
        align_assignment = false,
        align_conditional = false,
        align_pair_arrow = false,
        conditional_to_if = false,
        normalize_line_endings = "auto",
        align_matrix = false,
        trailing_comma = true,
        trailing_zero = true,
        indent_submodule = false,
        surround_whereop_typeparameters = true,
        variable_call_indent = [],
        yas_style_nesting = false,
    )
end

function is_binaryop_nestable(::YASStyle, cst::JuliaSyntax.GreenNode)
    (defines_function(cst) || is_assignment(cst)) && return false
    op_kind(cst) in KSet"=> ->" && return false
    return true
end

function p_import(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = p_import(DefaultStyle(style), cst, s; kwargs...)
    idx = findfirst(n -> n.typ === PLACEHOLDER, t.nodes)
    if idx !== nothing && is_colon(t[idx-1])
        t[idx] = Whitespace(1)
    end
    t
end

function p_using(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_import(ys, cst, s; kwargs...)
    t.typ = Using
    t
end

function p_export(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_import(ys, cst, s; kwargs...)
    t.typ = Export
    t
end

function p_curly(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    nws = s.opts.whitespace_typedefs ? 1 : 0
    t = FST(Curly, cst, nspaces(s))
    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; kwargs...)
        if kind(a) === K"," && i == length(cst) - 1
            continue
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
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

function p_braces(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_curly(ys, cst, s; kwargs...)
    t.typ = Braces
    t
end

function p_tuple(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(TupleN, cst, nspaces(s))
    for (i, a) in enumerate(children(cst))
        n = if kind(a) === K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        if kind(a) === K"," && i + 1 == length(cst)
            if length(cst.args::Vector{JuliaSyntax.GreenNode}) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(cst[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            end
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
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

function p_tuple(
    ys::YASStyle,
    nodes::Vector{JuliaSyntax.GreenNode{T}},
    s::State;
    kwargs...,
) where {T}
    style = getstyle(ys)
    t = FST(TupleN, nspaces(s))
    for (i, a) in enumerate(nodes)
        n = pretty(style, a, s; kwargs...)
        if kind(a) === K"," && i + 1 == length(nodes)
            if length(nodes) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(nodes[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            end
        elseif kind(a) === K"," && i < length(nodes) && !is_punc(nodes[i+1])
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
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest = false,
    nospace = false,
    kwargs...,
)
    style = getstyle(ys)
    t = FST(Brackets, cst, nspaces(s))

    if is_block(cst[2]) || (kind(cst[2]) === K"generator" && is_block(cst[2][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(
        t,
        pretty(style, cst[1], s),
        s,
        join_lines = true,
        override_join_lines_based_on_source = true,
    )

    n = if kind(cst[2]) === K"block"
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

function p_call(
    ys::YASStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    can_separate_kwargs::Bool = true,
    kwargs...,
)
    style = getstyle(ys)

    # # With `variable_call_indent`, check if the caller is in the list
    # and use `p_call` from `DefaultStyle` instead to allow both
    # `caller(something,...)` and `caller(\n,...)`.
    if caller_in_list(cst, s.opts.variable_call_indent)
        return p_call(DefaultStyle(style), cst, s)
    end

    t = FST(Call, cst, nspaces(s))
    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s)
        if kind(a) === K"," && i + 1 == length(cst)
            continue
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
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

    if s.opts.separate_kwargs_with_semicolon && can_separate_kwargs
        separate_kwargs_with_semicolon!(t)
    end

    t
end
function p_vect(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_call(ys, cst, s; kwargs...)
    t.typ = Vect
    t
end

function p_vcat(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(Vcat, nspaces(s))
    st = kind(cst) === K"vcat" ? 1 : 2
    args = get_args(cst)

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; kwargs...)
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
                if i != length(cst) - 1 || length(args) == 1
                    add_node!(t, InverseTrailingSemicolon(), s)
                end
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
function p_typedvcat(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_vcat(ys, cst, s; kwargs...)
    t.typ = TypedVcat
    t
end

function p_ncat(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(Ncat, nspaces(s))
    st = kind(cst) === K"ncat" ? 2 : 3
    args = get_args(cst)
    n_semicolons = SEMICOLON_LOOKUP[cst[st].head]

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; kwargs...)
        i == st && continue
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

            if i < length(cst) - 1
                for _ in 1:n_semicolons
                    add_node!(t, Semicolon(), s)
                end
            elseif length(args) == 1
                for _ in 1:n_semicolons
                    add_node!(t, Semicolon(), s)
                end
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
function p_typedncat(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_ncat(ys, cst, s; kwargs...)
    t.typ = TypedNcat
    t
end

function p_nrow(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(NRow, cst, nspaces(s))

    n_semicolons = SEMICOLON_LOOKUP[cst[1].head]

    for (i, a) in enumerate(children(cst))
        i == 1 && continue
        if is_opcall(a)
            add_node!(
                t,
                pretty(style, a, s; nospace = true, nonest = true, kwargs...),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
        end
        if i < length(cst)
            for _ in 1:n_semicolons
                add_node!(t, Semicolon(), s)
            end
            add_node!(t, Whitespace(1), s)
        end
    end
    t.nest_behavior = NeverNest
    t
end

function p_ref(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(RefN, cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices

    for (i, a) in enumerate(children(cst))
        n = if is_binary(a) || is_chain(a) || kind(a) in KSet"parens comparison"
            pretty(style, a, s; nonest = true, nospace = nospace, kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        if kind(a) === K"," && i + 1 == length(cst)
            continue
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
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

function p_comprehension(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(Comprehension, cst, nspaces(s))

    if is_block(cst[2]) || (kind(cst[2]) === K"generator" && is_block(cst[2][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(t, pretty(style, cst[1], s; kwargs...), s, join_lines = true)
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

function p_typedcomprehension(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(TypedComprehension, cst, nspaces(s))

    if is_block(cst[3]) || (kind(cst[3]) === K"generator" && is_block(cst[3][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    t
end

function p_macrocall(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(MacroCall, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )
    has_closer = is_closer(cst[end])

    !has_closer && (t.typ = MacroBlock)

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s)
        if JuliaSyntax.is_macro_name(a)
            add_node!(t, n, s, join_lines = true)
            if JuliaSyntax.is_whitespace(kind(cst[i+1]))
                add_node!(t, Whitespace(1), s)
            end
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
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
                if i < length(cst) - 1 && kind(cst[i+1]) !== K"parameters"
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

function p_whereopcall(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(Where, cst, nspaces(s))

    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    curly_ctx =
        kind(cst.parent) === K"curly" || kind(cst[3]) in KSet"curly bracescat parameters"

    add_braces =
        s.opts.surround_whereop_typeparameters && !curly_ctx && kind(cst[3]) !== K"{"

    bc = curly_ctx ? t : FST(BracesCat, nspaces(s))

    if add_braces
        brace = FST(PUNCTUATION, -1, t.endline, t.endline, "{")
        add_node!(bc, brace, s, join_lines = true)
    end

    for i in 3:length(children(cst))
        a = cst[i]
        n = is_binary(a) ? pretty(style, a, s, nospace = true) : pretty(style, a, s)

        if kind(a) === K"," && i == length(cst) - 1
            continue
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
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

function p_generator(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(Generator, cst, nspaces(s))
    has_for_kw = false

    from_iterable = false
    lineage = get(kwargs, :lineage, JuliaSyntax.Kind[])
    for (kind, is_itr) in Iterators.reverse(lineage)
        if kind in KSet"parens generator filter"
            continue
        elseif is_itr
            from_iterable = true
            break
        end
    end

    childs = children(cst)
    has_for_kw = findfirst(n -> kind(n) === K"for", childs) !== nothing

    for (i, a) in enumerate(childs)
        n = pretty(style, a, s)
        if JuliaSyntax.is_keyword(a)
            if is_block(cst[i-1])
                add_node!(t, Newline(), s)
            elseif from_iterable
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)

            if !is_gen(cst[i+1])
                tupargs = JuliaSyntax.GreenNode[]
                for j in i+1:length(cst)
                    push!(tupargs, cst[j])
                end

                # verify this is not another for loop
                any(b -> kind(b) === K"for", tupargs) && continue

                tup = p_tuple(style, tupargs, s; kwargs...)
                if has_for_kw
                    for nn in tup.nodes
                        eq_to_in_normalization!(
                            nn,
                            s.opts.always_for_in,
                            s.opts.for_in_replacement,
                        )
                    end
                end
                add_node!(t, tup, s; join_lines = true)
                break
            end
        elseif kind(a) === K","
            add_node!(t, n, s; join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        else
            add_node!(t, n, s; join_lines = true)
        end

        has_for_kw &&
            eq_to_in_normalization!(n, s.opts.always_for_in, s.opts.for_in_replacement)
    end

    t
end

function p_filter(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_generator(ys, cst, s; kwargs...)
    t.typ = Filter
    t
end

function p_flatten(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_generator(ys, cst, s; kwargs...)
    t.typ = Flatten
    t
end
