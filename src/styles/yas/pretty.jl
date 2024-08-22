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

function p_import(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Import, cst, nspaces(s))

    for a in children(cst)
        if kind(a) in KSet"import export using"
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif kind(a) === K":"
            nodes = children(a)
            for n in nodes
                add_node!(t, pretty(style, n, s; kwargs...), s, join_lines = true)
                if kind(n) in KSet"import export using :"
                    add_node!(t, Whitespace(1), s)
                elseif kind(n) in KSet","
                    add_node!(t, Placeholder(1), s)
                end
            end
        elseif kind(a) === K","
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif kind(a) === K":"
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
        end
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

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs..., from_curly = true)

        override = (i > 1 && kind(childs[i-1]) === K"{") || kind(a) === K"}"

        if kind(a) === K","
            add_node!(t, n, s; join_lines = true)
            if needs_placeholder(childs, i + 1, K"}")
                add_node!(t, Placeholder(nws), s)
            end
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
        end
    end
    t
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

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = if kind(a) === K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        override = (i > 1 && kind(childs[i-1]) === K"(") || kind(a) === K")"

        if kind(a) === K","
            add_node!(t, n, s; join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
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
        n = if kind(a) === K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        override = (i > 1 && kind(nodes[i-1]) === K"(") || kind(a) === K")"

        if kind(a) === K","
            add_node!(t, n, s; join_lines = true)
            if needs_placeholder(nodes, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
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

    args = get_args(cst)
    if length(args) > 0
        arg = args[1]
        if is_block(arg) || (kind(arg) === K"generator" && is_block(arg[1]))
            t.nest_behavior = AlwaysNest
        end
    end

    for c in children(cst)
        n = if kind(c) === K"("
            pretty(style, c, s; kwargs...)
        elseif kind(c) === K")"
            pretty(style, c, s; kwargs...)
        elseif kind(c) === K"block"
            pretty(style, c, s; kwargs..., from_quote = true)
        elseif is_opcall(c)
            pretty(style, c, s; nonest = nonest, nospace = nospace, kwargs...)
        else
            pretty(style, c, s; kwargs...)
        end
        add_node!(t, n, s; join_lines = true, override_join_lines_based_on_source = true)
    end

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
    # if caller_in_list(cst, s.opts.variable_call_indent)
    #     return p_call(DefaultStyle(style), cst, s)
    # end

    t = FST(Call, cst, nspaces(s))

    childs = children(cst)
    for (i, a) in enumerate(childs)
        k = kind(a)
        n = if k == K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        override = (i > 1 && kind(childs[i-1]) === K"(") || k === K")"

        if k === K","
            add_node!(t, n, s; join_lines = true)
            # figure out if we need to put a placeholder
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        elseif k === K"parameters"
            if n_args(cst) == n_args(a)
                # There are no arguments prior to params
                # so we can remove the initial placeholder.
                idx = findfirst(n -> n.typ === PLACEHOLDER, t.nodes)
                idx !== nothing && (t[idx] = Whitespace(0))
            end
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
        end
    end

    if s.opts.separate_kwargs_with_semicolon && can_separate_kwargs
        separate_kwargs_with_semicolon!(t)
    end

    t
end

function p_vect(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)

    t = FST(Vect, cst, nspaces(s))

    childs = children(cst)
    for (i, a) in enumerate(childs)
        k = kind(a)
        n = pretty(style, a, s; kwargs...)

        override = (i > 1 && kind(childs[i-1]) === K"[") || k === K"]"

        if k === K","
            add_node!(t, n, s; join_lines = true)
            # figure out if we need to put a placeholder
            if needs_placeholder(childs, i + 1, K"]")
                add_node!(t, Placeholder(1), s)
            end
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
        end
    end
    t
end

function p_vcat(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(Vcat, nspaces(s))
    st = kind(cst) === K"vcat" ? 1 : 2

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs...)
        override = i == st + 1 || kind(a) === K"]"

        if kind(a) === K"["
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"]"
            add_node!(t, n, s, join_lines = true, override_join_lines_based_on_source=override)
        elseif JuliaSyntax.is_whitespace(a)
            add_node!(t, n, s; join_lines = true)
        elseif kind(a) === K";"
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K"]")
                add_node!(t, Placeholder(1), s)
            end
        else
            join_lines = i == st + 1 ? true : t.endline == n.startline
            add_node!(t, n, s, join_lines = join_lines, override_join_lines_based_on_source = override)

            j = i + 1
            is_last_arg = false
            while j <= length(childs) && !is_last_arg
                k = kind(childs[j])
                if !JuliaSyntax.is_whitespace(k)
                    k === K"]" && (is_last_arg = true)
                    break
                end
                j += 1
            end
            if !is_last_arg && j <= length(childs)
                add_node!(t, Placeholder(1), s)
            end
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

    last_was_semicolon = false

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs...)
        override = i == st + 1 || kind(a) === K"]"

        if kind(a) === K"["
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"]"
            add_node!(t, n, s; join_lines = true, override_join_lines_based_on_source=override)
        else
            if kind(a) === K";"
                add_node!(t, n, s; join_lines = true)
                last_was_semicolon = true
            elseif JuliaSyntax.is_whitespace(a)
                add_node!(t, n, s; join_lines = true)
            else
                if last_was_semicolon
                    add_node!(t, Placeholder(1), s)
                    last_was_semicolon = false
                end
                add_node!(t, n, s; join_lines = true, override_join_lines_based_on_source=override)
            end
        end
    end
    t
end
function p_typedncat(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_ncat(ys, cst, s; kwargs...)
    t.typ = TypedNcat
    t
end

function p_ref(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(RefN, cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = if is_opcall(a)
            pretty(style, a, s; nonest = true, nospace = nospace, kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        override = (i > 1 && kind(childs[i-1]) === K"[") || kind(a) === K"]"

        if kind(a) === K","
            add_node!(t, n, s; join_lines = true)
            if needs_placeholder(childs, i + 1, K"]")
                add_node!(t, Placeholder(1), s)
            end
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
        end
    end
    t
end

function p_comprehension(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(Comprehension, cst, nspaces(s))

    idx = findfirst(
        n -> !JuliaSyntax.is_whitespace(kind(n)) && !(kind(n) in KSet"[ ]"),
        children(cst),
    )
    arg = cst[idx]

    if is_block(arg) || (kind(arg) === K"generator" && is_block(arg[1]))
        t.nest_behavior = AlwaysNest
    end

    for c in children(cst)
        n = pretty(style, c, s; kwargs...)
        if kind(c) === K"["
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        end
    end

    t
end

function p_typedcomprehension(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_comprehension(ys, cst, s; kwargs...)
    t.typ = TypedComprehension
    t
end

function p_macrocall(ys::YASStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ys)
    t = FST(MacroCall, cst, nspaces(s))

    has_closer = is_closer(cst[end])
    is_macroblock = !has_closer

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(
            style,
            a,
            s;
            kwargs...,
            can_separate_kwargs = false,
            standalone_binary_circuit = false,
        )

        override = (i > 1 && kind(childs[i-1]) === K"(") || kind(a) === K")"

        if JuliaSyntax.is_macro_name(a) || kind(a) === K"("
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K","
            add_node!(t, n, s; join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        elseif kind(a) === K"parameters"
            if n_args(cst) == n_args(a)
                # There are no arguments prior to params
                # so we can remove the initial placeholder.
                idx = findfirst(n -> n.typ === PLACEHOLDER, t.nodes)
                idx !== nothing && (t[idx] = Whitespace(0))
            end
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
        elseif is_macroblock
            if n.typ === MacroBlock && t[end].typ === WHITESPACE
                t[end] = Placeholder(length(t[end].val))
            end

            padding = is_block(n) ? 0 : -1

            if haschildren(a) && kind(a[1]) === K"Whitespace" && t[end].typ !== WHITESPACE
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s, join_lines = true, max_padding = padding)
            if kind(a) in KSet"NewlineWs Whitespace" && t[end].typ !== WHITESPACE
                add_node!(t, Whitespace(1), s)
            end
        else
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )
        end
    end

    if is_macroblock
        t.typ = MacroBlock
    end
    # move placement of @ to the end
    #
    # @Module.macro -> Module.@macro
    t[1] = move_at_sign_to_the_end(t[1], s)
    t
end

function p_whereopcall(
    ys::YASStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_curly::Bool = false,
    kwargs...,
)
    style = getstyle(ys)
    t = FST(Where, cst, nspaces(s))

    childs = children(cst)
    where_idx = findfirst(c -> kind(c) === K"where" && !haschildren(c), childs)
    curly_ctx = if where_idx === nothing
        from_curly
    else
        from_curly ||
            any(c -> kind(c) in KSet"curly bracescat braces", childs[where_idx+1:end])
    end
    add_braces = s.opts.surround_whereop_typeparameters && !curly_ctx

    nws = s.opts.whitespace_typedefs ? 1 : 0

    after_where = false
    for (i, a) in enumerate(childs)
        if kind(a) === K"where" && !haschildren(a)
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
            after_where = true
        elseif kind(a) === K"{"
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            s.indent += s.opts.indent
        elseif kind(a) === K"}"
            add_node!(
                t,
                pretty(style, a, s; kwargs...),
                s;
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
            s.indent -= s.opts.indent
        elseif kind(a) === K","
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            if needs_placeholder(childs, i + 1, K"}")
                add_node!(t, Placeholder(nws), s)
            end
        elseif JuliaSyntax.is_whitespace(a)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        else
            n = pretty(
                style,
                a,
                s;
                kwargs...,
                nospace = !s.opts.whitespace_typedefs,
                from_curly = from_curly || add_braces,
            )

            if after_where && add_braces
                brace = FST(PUNCTUATION, -1, n.endline, n.endline, "{")
                add_node!(t, brace, s, join_lines = true)
            end

            override = i > 1 && kind(childs[i-1]) === K"{"
            add_node!(
                t,
                n,
                s;
                join_lines = true,
                override_join_lines_based_on_source = override,
            )

            if after_where && add_braces
                brace = FST(PUNCTUATION, -1, n.endline, n.endline, "}")
                add_node!(t, brace, s, join_lines = true)
            end
        end
    end

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
            if is_block(childs[i-1])
                add_node!(t, Newline(), s)
            elseif from_iterable
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)

            if !is_gen(childs[i+1])
                tupargs = JuliaSyntax.GreenNode[]
                for j in i+1:length(cst)
                    push!(tupargs, childs[j])
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
