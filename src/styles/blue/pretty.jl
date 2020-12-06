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
    )
end

function nestable(::BlueStyle, cst::CSTParser.EXPR)
    is_assignment(cst) && is_iterable(cst[end]) && return false
    return nestable(DefaultStyle(), cst)
end

function p_do(bs::BlueStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(bs)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if cst[3].fullspan != 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    end
    if cst[4].typ === CSTParser.Block
        s.indent += s.opts.indent
        n = pretty(style, cst[4], s, ignore_single_line = true)
        add_node!(t, n, s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
    end
    add_node!(t, pretty(style, cst.args[end], s), s)
    t
end

function separate_kwargs_with_semicolon!(fst::FST)
    kw_idx = findfirst(n -> n.typ === CSTParser.Kw, fst.nodes)
    kw_idx === nothing && return
    sc_idx = findfirst(n -> n.typ === SEMICOLON, fst.nodes)
    # first "," prior to a kwarg
    comma_idx = findlast(is_comma, fst.nodes[1:kw_idx-1])

    if sc_idx !== nothing && sc_idx > kw_idx
        # move ; prior to first kwarg
        fst[sc_idx].val = ","
        fst[sc_idx].typ = CSTParser.PUNCTUATION
        if comma_idx === nothing
            if fst[kw_idx-1].typ === PLACEHOLDER
                fst[kw_idx-1] = Placeholder(1)
            else
                insert!(fst, kw_idx - 1, Placeholder(1))
            end
        end
        insert!(fst, kw_idx - 1, Semicolon())
    elseif sc_idx === nothing && comma_idx === nothing
        if fst[kw_idx-1].typ === PLACEHOLDER
            fst[kw_idx-1] = Placeholder(1)
        else
            insert!(fst, kw_idx - 1, Placeholder(1))
        end
        insert!(fst, kw_idx - 1, Semicolon())
    elseif sc_idx === nothing
        fst[comma_idx].val = ";"
        fst[comma_idx].typ = SEMICOLON
    end
    return
end

function p_call(bs::BlueStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(bs)
    t = p_call(DefaultStyle(style), cst, s)
    if !parent_is(cst, is_function_or_macro_def)
        separate_kwargs_with_semicolon!(t)
    end
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
    t = FST(cst, nspaces(s))
    op = cst[2]
    nonest = nonest || op.kind === Tokens.COLON
    if cst.parent.typ === CSTParser.Curly &&
       op.kind in (Tokens.ISSUBTYPE, Tokens.ISSUPERTYPE) &&
       !s.opts.whitespace_typedefs
        nospace = true
    elseif op.kind === Tokens.COLON
        nospace = true
    end
    nospace_args = s.opts.whitespace_ops_in_indices ? false : nospace

    if is_opcall(cst[1])
        n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[1], s)
    end

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[1]) &&
       !is_iterable(cst[1])
        paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, "(")
        add_node!(t, paren, s)
        add_node!(t, n, s, join_lines = true)
        paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s)
    end

    nrhs = nest_rhs(cst)
    nrhs && (t.nest_behavior = AlwaysNest)
    nest = (nestable(style, cst) && !nonest) || nrhs

    if op.fullspan == 0
        # Do nothing - represents a binary op with no textual representation.
        # For example: `2a`, which is equivalent to `2 * a`.
    elseif op.kind === Tokens.EX_OR
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif (
        is_number(cst[1]) ||
        op.kind === Tokens.FWDFWD_SLASH ||
        op.kind === Tokens.CIRCUMFLEX_ACCENT
    ) && op.dot
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif op.kind !== Tokens.IN && (
        nospace || (
            op.kind !== Tokens.ANON_FUNC && CSTParser.precedence(op) in (
                CSTParser.ColonOp,
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

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[3]) &&
       !is_iterable(cst[3])
        paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(t, n, s, join_lines = true)
        paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s, join_lines = true)
    end

    if nest
        # for indent, will be converted to `indent` if needed
        insert!(t.nodes, length(t.nodes), Placeholder(0))
    end

    t
end

function p_return(bs::BlueStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(bs)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if cst[2].fullspan != 0
        for a in cst.args[2:end]
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    elseif cst[2].kind === Tokens.NOTHING
        add_node!(t, Whitespace(1), s)
        no = FST(CSTParser.IDENTIFIER, -1, t.endline, t.endline, "nothing")
        add_node!(t, no, s, join_lines = true)
    end
    t
end
