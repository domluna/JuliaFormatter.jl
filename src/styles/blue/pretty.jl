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
"""
struct BlueStyle <: AbstractStyle
    innerstyle::Union{Nothing,AbstractStyle}
end
BlueStyle() = BlueStyle(nothing)

@inline getstyle(s::BlueStyle) = s.innerstyle === nothing ? s : s.innerstyle

function options(style::BlueStyle)
    return (;
        always_use_return = true,
        short_to_long_function_def = true,
        whitespace_ops_in_indices = true,
        remove_extra_newlines = true,
        always_for_in = true,
        import_to_using = true,
        pipe_to_function_call = true,
        whitespace_in_kwargs = false,
        annotate_untyped_fields_with_any = false,
        conditional_to_if = true,
        indent_submodule = true,
    )
end

function is_binaryop_nestable(::BlueStyle, cst::CSTParser.EXPR)
    is_assignment(cst) && is_iterable(cst[end]) && return false
    return is_binaryop_nestable(DefaultStyle(), cst)
end

function p_call(bs::BlueStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(bs)
    t = p_call(DefaultStyle(style), cst, s)
    if !parent_is(cst, n -> is_function_or_macro_def(n) || n.head == :macrocall)
        separate_kwargs_with_semicolon!(t)
    end
    t
end

function p_do(bs::BlueStyle, cst::CSTParser.EXPR, s::State)
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
    if nodes[2].head === :block
        s.indent += s.opts.indent
        n = pretty(style, nodes[2], s, ignore_single_line = true)
        add_node!(t, n, s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
    end
    add_node!(t, pretty(style, cst[end], s), s)
    t
end

function p_binaryopcall(
    bs::BlueStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(bs)
    t = FST(Binary, cst, nspaces(s))
    op = cst[2]

    nonest = nonest || CSTParser.is_colon(op)

    if CSTParser.iscurly(cst.parent) &&
       (op.val == "<:" || op.val == ">:") &&
       !s.opts.whitespace_typedefs
        nospace = true
    elseif CSTParser.is_colon(op)
        nospace = true
    end
    nospace_args = s.opts.whitespace_ops_in_indices ? false : nospace

    if is_opcall(cst[1])
        n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[1], s)
    end

    if CSTParser.is_colon(op) &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[1]) &&
       !is_iterable(cst[1])
        paren = FST(PUNCTUATION, -1, n.startline, n.startline, "(")
        add_node!(t, paren, s)
        add_node!(t, n, s, join_lines = true)
        paren = FST(PUNCTUATION, -1, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s)
    end

    nrhs = nest_rhs(cst)
    nrhs && (t.nest_behavior = AlwaysNest)
    nest = (is_binaryop_nestable(style, cst) && !nonest) || nrhs

    if op.fullspan == 0
        # Do nothing - represents a binary op with no textual representation.
        # For example: `2a`, which is equivalent to `2 * a`.
    elseif CSTParser.is_exor(op)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif (
        CSTParser.isnumber(cst[1]) || is_fwdfwd_slash(op) || is_circumflex_accent(op)
    ) && CSTParser.isdotted(op)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif !(CSTParser.is_in(op) || CSTParser.is_elof(op)) && (
        nospace || (
            !CSTParser.is_anon_func(op) && precedence(op) in (
                CSTParser.RationalOp,
                CSTParser.PowerOp,
                CSTParser.DeclarationOp,
                CSTParser.DotOp,
            )
        )
    )
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    else
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    end

    if is_opcall(cst[3])
        n = pretty(style, cst[3], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[3], s)
    end

    if CSTParser.is_colon(op) &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[3]) &&
       !is_iterable(cst[3])
        paren = FST(PUNCTUATION, -1, n.startline, n.startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(t, n, s; join_lines = true, override_join_lines_based_on_source = !nest)
        paren = FST(PUNCTUATION, -1, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s; join_lines = true, override_join_lines_based_on_source = !nest)
    end

    if nest
        # for indent, will be converted to `indent` if needed
        insert!(t.nodes, length(t.nodes), Placeholder(0))
    end

    t
end

function p_return(bs::BlueStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(bs)
    t = FST(Return, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if cst[2].fullspan != 0
        for i = 2:length(cst)
            a = cst[i]
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    elseif CSTParser.is_nothing(cst[2])
        add_node!(t, Whitespace(1), s)
        no = FST(IDENTIFIER, -1, t.endline, t.endline, "nothing")
        add_node!(t, no, s, join_lines = true)
    end
    t
end
