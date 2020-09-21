
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


function p_call(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))

    if nest
        add_node!(t, Placeholder(0), s)
    end

    for (i, a) in enumerate(cst.args[3:end])
        if i + 2 == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end

    kw_idx = findfirst(n -> n.typ === CSTParser.Kw, t.nodes)
    sc_idx = findfirst(n -> n.typ === SEMICOLON, t.nodes)

    # move ; prior to first kwarg
    if kw_idx !== nothing && sc_idx !== nothing && sc_idx > kw_idx
        t[sc_idx].val = ","
        t[sc_idx].typ = CSTParser.PUNCTUATION

        # insert!(t, kw_idx-1, Placeholder(1))
        insert!(t, kw_idx-1, Semicolon())
    elseif kw_idx !== nothing && sc_idx === nothing
        comma_idx = findlast(is_comma, t.nodes[1:kw_idx-1])
        if comma_idx === nothing
            insert!(t, kw_idx-1, Semicolon())
        else
            t[comma_idx].val = ";"
            t[comma_idx].typ = SEMICOLON
        end
    end

    t
end
