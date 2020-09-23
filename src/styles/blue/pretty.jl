"""
    BlueStyle()

> This style is a WIP/Experimental

Formatting style based on https://github.com/invenia/BlueStyle
and https://github.com/domluna/JuliaFormatter.jl/issues/283

Recommended options are:

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
struct BlueStyle <: AbstractStyle end
@inline getstyle(s::BlueStyle) = s

function nestable(::BlueStyle, cst::CSTParser.EXPR)
    is_assignment(cst) && is_iterable(cst[end]) && return false
    return nestable(DefaultStyle(), cst)
end

function p_do(style::BlueStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if cst[3].fullspan != 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    end
    if cst[4].typ === CSTParser.Block
        s.indent += s.opts.indent_size
        n = pretty(style, cst[4], s, ignore_single_line = true)
        add_node!(t, n, s, max_padding = s.opts.indent_size)
        s.indent -= s.opts.indent_size
    end
    add_node!(t, pretty(style, cst.args[end], s), s)
    t
end

function separate_kwargs_with_semicolon!(fst::FST)
    kw_idx = findfirst(n -> n.typ === CSTParser.Kw, fst.nodes)
    kw_idx === nothing && return
    sc_idx = findfirst(n -> n.typ === SEMICOLON, fst.nodes)
    comma_idx = findlast(is_comma, fst.nodes[1:kw_idx-1])

    # move ; prior to first kwarg
    if kw_idx !== nothing && sc_idx !== nothing && sc_idx > kw_idx
        fst[sc_idx].val = ","
        fst[sc_idx].typ = CSTParser.PUNCTUATION
        comma_idx === nothing && insert!(fst, kw_idx - 1, Placeholder(1))
        insert!(fst, kw_idx - 1, Semicolon())
    elseif kw_idx !== nothing && sc_idx === nothing
        comma_idx = findlast(is_comma, fst.nodes[1:kw_idx-1])
        if comma_idx === nothing
            insert!(fst, kw_idx - 1, Placeholder(1))
            insert!(fst, kw_idx - 1, Semicolon())
        else
            fst[comma_idx].val = ";"
            fst[comma_idx].typ = SEMICOLON
        end
    end
    return
end

function p_call(bs::BlueStyle, cst::CSTParser.EXPR, s::State)
    t = p_call(DefaultStyle(bs), cst, s)
    if !parent_is(cst, is_function_or_macro_def)
        separate_kwargs_with_semicolon!(t)
    end
    t
end
