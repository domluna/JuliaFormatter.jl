"""
    BlueStyle()

Formatting style based on [BlueStyle](https://github.com/invenia/BlueStyle)
and [JuliaFormatter#283](https://github.com/domluna/JuliaFormatter.jl/issues/283).

!!! note
    This style is still work-in-progress, and does not yet implement all of the
    BlueStyle guide.

Configurable options with different defaults to [`DefaultStyle`](@ref) are:
- `always_use_return` = true
- `short_to_long_function_def` = true
- `whitespace_ops_in_indices` = true
- `remove_extra_newlines` = true
- `always_for_in` = true
- `import_to_using` = true
- `pipe_to_function_call` = true
- `whitespace_in_kwargs` = false
- `annotate_untyped_fields_with_any` = false
- `separate_kwargs_with_semicolon` = true
"""
struct BlueStyle <: AbstractStyle
    innerstyle::AbstractStyle
end
BlueStyle() = BlueStyle(NoopStyle())

function options(::BlueStyle)
    return (;
        always_use_return = true,
        short_to_long_function_def = true,
        long_to_short_function_def = false,
        whitespace_ops_in_indices = true,
        remove_extra_newlines = true,
        always_for_in = true,
        import_to_using = true,
        pipe_to_function_call = true,
        whitespace_in_kwargs = false,
        annotate_untyped_fields_with_any = false,
        conditional_to_if = true,
        indent_submodule = true,
        separate_kwargs_with_semicolon = true,
        indent = 4,
        margin = 92,
        whitespace_typedefs = false,
        format_docstrings = false,
        align_struct_field = false,
        align_assignment = false,
        align_conditional = false,
        align_pair_arrow = false,
        normalize_line_endings = "auto",
        align_matrix = false,
        join_lines_based_on_source = false,
        trailing_comma = true,
        trailing_zero = true,
        surround_whereop_typeparameters = true,
        variable_call_indent = [],
        yas_style_nesting = false,
    )
end

function is_binaryop_nestable(::BlueStyle, cst::JuliaSyntax.GreenNode)
    is_assignment(cst) && is_iterable(cst[end]) && return false
    return is_binaryop_nestable(DefaultStyle(), cst)
end

function p_do(bs::BlueStyle, cst::JuliaSyntax.GreenNode, s::State)
    style = getstyle(bs)
    t = FST(Do, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    nodes = map(cst[3]) do n
        n
    end
    if nodes[1].fullspan != 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, nodes[1], s), s, join_lines = true)
    end
    if kind(nodes[2]) === K"block"
        s.indent += s.opts.indent
        n = pretty(style, nodes[2], s, ignore_single_line = true)
        add_node!(t, n, s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
    end
    add_node!(t, pretty(style, cst[end], s), s)
    t
end

function p_binaryopcall(
    ds::BlueStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest = false,
    nospace = false,
    from_curly = false,
)
    style = getstyle(ds)
    t = FST(Binary, cst, nspaces(s))
    op = findfirst(JuliaSyntax.is_operator, children(cst))
    opkind = op_kind(cst)

    nonest = nonest || opkind === K":"

    # TODO: figure out parent
    if from_curly && opkind in KSet"<: >:" && !s.opts.whitespace_typedefs
        nospace = true
    elseif kind(op) === K":"
        nospace = true
    end
    nospace_args = s.opts.whitespace_ops_in_indices ? false : nospace

    childs = children(cst)
    nodes = map(enumerate(childs)) do a
        idx, c = a
        n = if idx == 1 || idx == length(childs)
            pretty(style, c, s, nonest = nonest, nospace = nospace_args)
        else
            pretty(style, c, s)
        end
        n
    end
    nodes = filter(n -> n.typ !== NONE, nodes)

    if opkind === K":" &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[1]) &&
       !is_iterable(cst[1])
        paren = FST(PUNCTUATION, -1, nodes[1].startline, nodes[1].startline, "(")
        add_node!(t, paren, s)
        add_node!(t, nodes[1], s, join_lines = true)
        paren = FST(PUNCTUATION, -1, nodes[1].startline, nodes[1].startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, nodes[1], s)
    end

    nrhs = nest_rhs(cst)
    nrhs && (t.nest_behavior = AlwaysNest)
    nest = (is_binaryop_nestable(style, cst) && !nonest) || nrhs

    if opkind === K"$"
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif (
        (JuliaSyntax.is_number(cst[1]) || opkind === K"^") && kind(cst) === K"dotcall"
    ) ||
           # 1 .. -2 (can be ., .., ..., etc)
           (
        JuliaSyntax.is_number(cst[end]) &&
        startswith(nodes[end].val, "-") &&
        opkind in KSet".."
    )
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif !(opkind in KSet"in isa ∈") &&
           (nospace || (opkind !== K"->" && opkind in KSet"⥔ :: . //"))
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif JuliaSyntax.is_radical_op(opkind)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    else
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    end

    if opkind === K":" &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[end]) &&
       !is_iterable(cst[end])
        paren = FST(PUNCTUATION, -1, nodes[end].startline, nodes[end].startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(
            t,
            nodes[end],
            s,
            join_lines = true,
            override_join_lines_based_on_source = !nest,
        )
        paren = FST(PUNCTUATION, -1, nodes[end].startline, nodes[end].startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(
            t,
            nodes[end],
            s,
            join_lines = true,
            override_join_lines_based_on_source = !nest,
        )
    end

    if nest
        # for indent, will be converted to `indent` if needed
        insert!(t.nodes::Vector{FST}, length(t.nodes::Vector{FST}), Placeholder(0))
    end

    t
end

function p_return(bs::BlueStyle, cst::JuliaSyntax.GreenNode, s::State)
    style = getstyle(bs)
    t = FST(Return, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    childs = children(cst)
    if length(childs) == 1
        no = FST(IDENTIFIER, -1, t.endline, t.endline, "nothing")
        add_node!(t, no, s, join_lines = true)
    else
        for a in childs[2:end]
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
