function format_check(io::IOBuffer, fst::FST, s::State)
    if length(s.doc.format_skips) == 0
        print_notcode(io, fst, s)
        return
    end

    skip = s.doc.format_skips[1]
    line_range = fst.startline:fst.endline

    if s.on && skip[1] in line_range && skip[2] in line_range
        # weird corner case where off and on toggle
        # are in the same comment block
        fst.endline = skip[1]
        print_notcode(io, fst, s, fmttag = true)
        write(io, skip[3])
        fst.startline = skip[2]
        fst.endline = skip[2]
        print_notcode(io, fst, s, fmttag = true)
    elseif s.on && skip[1] in line_range
        fst.endline = skip[1]
        print_notcode(io, fst, s, fmttag = true)
        write(io, skip[3])
        s.on = false
    elseif !s.on && skip[2] in line_range
        deleteat!(s.doc.format_skips, 1)
        s.on = true
        # change the startline, otherwise lines
        # prior to in the NOTCODE node prior to
        # "format: on" will be reprinted
        fst.startline = skip[2]
        print_notcode(io, fst, s, fmttag = true)
        # previous NEWLINE node won't be printed
    else
        print_notcode(io, fst, s)
    end
end

function print_leaf(io::IOBuffer, fst::FST, s::State)
    if fst.typ === NOTCODE
        format_check(io, fst, s)
    elseif fst.typ === INLINECOMMENT
        print_inlinecomment(io, fst, s)
    else
        s.on && write(io, fst.val)
    end
    s.line_offset += length(fst)
end

function print_tree(io::IOBuffer, fst::FST, s::State)
    notcode_indent = -1
    if (fst.typ === Binary || fst.typ === Conditional || fst.typ === ModuleN)
        notcode_indent = fst.indent
    end
    print_tree(io, fst.nodes, s, fst.indent, notcode_indent = notcode_indent)
end

function print_tree(
    io::IOBuffer,
    nodes::Vector{FST},
    s::State,
    indent::Int;
    notcode_indent = -1,
)
    ws = repeat(" ", max(indent, 0))
    for (i, n) in enumerate(nodes)
        if n.typ === NOTCODE
            if notcode_indent > -1
                n.indent = notcode_indent
            elseif i + 1 < length(nodes) && is_end(nodes[i+2])
                n.indent += s.opts.indent
            elseif i + 1 < length(nodes) &&
                   (nodes[i+2].typ === Block || nodes[i+2].typ === Begin)
                n.indent = nodes[i+2].indent
            elseif i > 2 && (nodes[i-2].typ === Block || nodes[i-2].typ === Begin)
                n.indent = nodes[i-2].indent
            end
        end

        if is_leaf(n)
            print_leaf(io, n, s)
        elseif n.typ === StringN
            print_string(io, n, s)
        else
            print_tree(io, n, s)
        end

        if n.typ === NEWLINE && s.on && i < length(nodes)
            if is_closer(nodes[i+1]) || nodes[i+1].typ === Block || nodes[i+1].typ === Begin
                write(io, repeat(" ", max(nodes[i+1].indent, 0)))
                s.line_offset = nodes[i+1].indent
            elseif !skip_indent(nodes[i+1])
                write(io, ws)
                s.line_offset = indent
            end
        end
    end
end

function print_string(io::IOBuffer, fst::FST, s::State)
    # The indent of StringH is set to the the offset
    # of when the quote is first encountered in the source file.

    # This difference notes the indent change due to formatting.
    diff = s.line_offset - fst.indent

    # The new indent for the string is the index of when a character in
    # the multiline string is FIRST encountered in the source file plus
    # the above difference.
    fst.indent = max(fst[1].indent + diff, 0)
    print_tree(io, fst, s)
end

function print_notcode(io::IOBuffer, fst::FST, s::State; fmttag = false)
    s.on || return
    for l = fst.startline:fst.endline
        ws, v = get(s.doc.comments, l, (0, "\n"))
        !fmttag && (ws = fst.indent)

        # If the current newline is followed by another newline
        # don't print the current newline.
        if s.opts.remove_extra_newlines
            _, vn = get(s.doc.comments, l + 1, (0, "\n"))
            vn == "\n" && v == "\n" && (v = "")
        end

        v == "" && continue
        v == "\n" && (ws = 0)

        if l == fst.endline && v[end] == '\n'
            v = v[1:prevind(v, end)]
        end

        ws > 0 && write(io, repeat(" ", ws))
        write(io, v)

        if l != fst.endline && v[end] != '\n'
            write(io, "\n")
        end
    end
end

function print_inlinecomment(io::IOBuffer, fst::FST, s::State)
    s.on || return
    ws, v = get(s.doc.comments, fst.startline, (0, ""))
    isempty(v) && return
    v = v[end] == '\n' ? v[nextind(v, 1):prevind(v, end)] : v
    ws > 0 && write(io, repeat(" ", ws))
    write(io, v)
end
