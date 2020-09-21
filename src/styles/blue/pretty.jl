
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
    separate_kwargs_with_semicolon!(t)
    t
end
