function n_call!(ys::YASStyle, fst::FST, s::State)
    style = getstyle(ys)

    f = n -> n.typ === PLACEHOLDER || n.typ === NEWLINE

    for (i, n) in enumerate(fst.nodes)
        if i == 3
            # The indent is set here to handle the edge
            # case where the first argument of Call is
            # nestable.
            # ref https://github.com/domluna/JuliaFormatter.jl/issues/387
            fst.indent = s.line_offset
        end

        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            si = findnext(f, fst.nodes, i + 1)
            nest_if_over_margin!(style, fst, s, i; stop_idx = si)
        elseif n.typ === TRAILINGSEMICOLON
            n.val = ""
            n.len = 0
            nest!(style, n, s)
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(style, n, s)
        else
            diff = fst.indent - fst[i].indent
            add_indent!(n, s, diff)
            n.extra_margin = 1
            nest!(style, n, s)
        end
    end
end
@inline n_curly!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)
@inline n_ref!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)
@inline n_macrocall!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)
@inline n_typedcomprehension!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)
@inline n_typedvcat!(ys::YASStyle, fst::FST, s::State) = n_call!(ys, fst, s)

function n_tuple!(ys::YASStyle, fst::FST, s::State)
    style = getstyle(ys)
    fst.indent = s.line_offset
    length(fst.nodes) > 0 && is_opener(fst[1]) && (fst.indent += 1)

    f = n -> n.typ === PLACEHOLDER || n.typ === NEWLINE

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            si = findnext(f, fst.nodes, i + 1)
            nest_if_over_margin!(style, fst, s, i; stop_idx = si)
        elseif n.typ === TRAILINGSEMICOLON
            n.val = ""
            n.len = 0
            nest!(style, n, s)
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(style, n, s)
        else
            diff = fst.indent - fst[i].indent
            add_indent!(n, s, diff)
            n.extra_margin = 1
            nest!(style, n, s)
        end
    end
end
@inline n_braces!(ys::YASStyle, fst::FST, s::State) = n_tuple!(ys, fst, s)
@inline n_vect!(ys::YASStyle, fst::FST, s::State) = n_tuple!(ys, fst, s)
@inline n_parameters!(ys::YASStyle, fst::FST, s::State) = n_tuple!(ys, fst, s)
@inline n_invisbrackets!(ys::YASStyle, fst::FST, s::State) = n_tuple!(ys, fst, s)
@inline n_comprehension!(ys::YASStyle, fst::FST, s::State) = n_tuple!(ys, fst, s)
@inline n_vcat!(ys::YASStyle, fst::FST, s::State) = n_tuple!(ys, fst, s)
@inline n_bracescat!(ys::YASStyle, fst::FST, s::State) = n_tuple!(ys, fst, s)

function n_generator!(ys::YASStyle, fst::FST, s::State)
    style = getstyle(ys)
    diff = s.line_offset - fst[1].indent

    # if the first argument is not a leaf
    # aligns it to be inside the generator
    # expression
    add_indent!(fst[1], s, diff)

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            si = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
            nest_if_over_margin!(style, fst, s, i; stop_idx = si)
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(style, n, s)
        else
            n.extra_margin = 1
            nest!(style, n, s)
        end
    end
end
@inline n_filter!(ys::YASStyle, fst::FST, s::State) = n_generator!(ys, fst, s)
@inline n_flatten!(ys::YASStyle, fst::FST, s::State) = n_generator!(ys, fst, s)

function n_whereopcall!(ys::YASStyle, fst::FST, s::State)
    style = getstyle(ys)
    fst.indent = s.line_offset
    # after "A where "
    Blen = sum(length.(fst[2:end]))
    fst[1].extra_margin = Blen + fst.extra_margin

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            si = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
            nest_if_over_margin!(style, fst, s, i; stop_idx = si)
        elseif is_opener(n) && n.val == "{"
            fst.indent = s.line_offset + 1
            nest!(style, n, s)
        elseif i == 1 || i == length(fst.nodes)
            nest!(style, n, s)
        else
            n.extra_margin = 1
            nest!(style, n, s)
        end
    end
end

function n_using!(ys::YASStyle, fst::FST, s::State)
    style = getstyle(ys)
    idx = findfirst(n -> n.val == ":", fst.nodes)
    fst.indent = s.line_offset
    if idx === nothing
        fst.indent += sum(length.(fst[1:2]))
    else
        fst.indent += sum(length.(fst[1:idx+1]))
    end
    for (i, n) in enumerate(fst.nodes)
        if n.typ === PLACEHOLDER
            si = findnext(n -> n.typ === PLACEHOLDER, fst.nodes, i + 1)
            nest_if_over_margin!(style, fst, s, i; stop_idx = si)
        elseif n.typ === NEWLINE
            s.line_offset = fst.indent
        else
            nest!(style, n, s)
        end
    end
end
@inline n_export!(ys::YASStyle, fst::FST, s::State) = n_using!(ys, fst, s)
@inline n_import!(ys::YASStyle, fst::FST, s::State) = n_using!(ys, fst, s)

function n_chainopcall!(ys::YASStyle, fst::FST, s::State)
    style = getstyle(ys)
    n_block!(DefaultStyle(style), fst, s, indent = s.line_offset)
end

function n_comparison!(ys::YASStyle, fst::FST, s::State)
    style = getstyle(ys)
    n_block!(DefaultStyle(style), fst, s, indent = s.line_offset)
end

function n_binaryopcall!(ys::YASStyle, fst::FST, s::State; indent::Int = -1)
    style = getstyle(ys)
    if findfirst(n -> n.typ === PLACEHOLDER, fst.nodes) !== nothing
        n_binaryopcall!(DefaultStyle(style), fst, s; indent = indent)
        return
    end

    start_line_offset = s.line_offset
    walk(increment_line_offset!, fst.nodes[1:end-1], s, fst.indent)
    nest!(style, fst[end], s)
end

function n_for!(ys::YASStyle, fst::FST, s::State)
    style = getstyle(ys)
    block_idx = findfirst(n -> !is_leaf(n), fst.nodes)
    if block_idx === nothing || length(fst[block_idx]) == 0
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
        return
    end

    ph_idx = findfirst(n -> n.typ === PLACEHOLDER, fst[block_idx].nodes)
    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE && fst.nodes[i+1].typ === Block
            s.line_offset = fst.nodes[i+1].indent
        elseif n.typ === NOTCODE && fst.nodes[i+1].typ === Block
            s.line_offset = fst.nodes[i+1].indent
        elseif n.typ === NEWLINE
            s.line_offset = fst.indent
        else
            n.extra_margin = fst.extra_margin
            if i == 3 && n.typ === Block
                n_block!(style, n, s, indent = s.line_offset)
            else
                nest!(style, n, s)
            end
        end
    end

    # return if the argument block was nested
    ph_idx !== nothing && fst[3][ph_idx].typ === NEWLINE && return

    idx = 5
    n = fst[idx]
    if n.typ === NOTCODE && n.startline == n.endline
        res = get(s.doc.comments, n.startline, (0, ""))
        res == (0, "") && (fst[idx-1] = Whitespace(0))
    end
end
