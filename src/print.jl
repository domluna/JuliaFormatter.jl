function format_check(io::IOBuffer, x::PTree, s::State)
    if length(s.doc.format_skips) == 0
        print_notcode(io, x, s)
        return
    end

    skip = s.doc.format_skips[1]
    line_range = x.startline:x.endline

    if s.on && skip[1] in line_range && skip[2] in line_range
        # weird corner case where off and on toggle
        # are in the same comment block
        x.endline = skip[1]
        print_notcode(io, x, s)
        write(io, skip[3])
        x.startline = skip[2]
        x.endline = skip[2]
        print_notcode(io, x, s)
    elseif s.on && skip[1] in line_range
        x.endline = skip[1]
        print_notcode(io, x, s)
        write(io, skip[3])
        s.on = false
    elseif !s.on && skip[2] in line_range
        deleteat!(s.doc.format_skips, 1)
        s.on = true
        # change the startline, otherwise lines
        # prior to in the NOTCODE node prior to 
        # "format: on" will be reprinted
        x.startline = skip[2]
        print_notcode(io, x, s)
        # previous NEWLINE node won't be printed
    else
        print_notcode(io, x, s)
    end
end

function print_leaf(io::IOBuffer, x::PTree, s::State)
    if x.typ === NOTCODE
        format_check(io, x, s)
    elseif x.typ === INLINECOMMENT
        print_inlinecomment(io, x, s)
    else
        s.on && write(io, x.val)
    end
    s.line_offset += length(x)
end

function print_tree(io::IOBuffer, x::PTree, s::State)
    if is_leaf(x)
        print_leaf(io, x, s)
        return
    end
    print_tree(io, x.nodes, s, x.indent)
end

function print_tree(io::IOBuffer, nodes::Vector{PTree}, s::State, indent::Int)
    ws = repeat(" ", max(indent, 0))
    for (i, n) in enumerate(nodes)
        if is_leaf(n)
            print_leaf(io, n, s)
        elseif n.typ === CSTParser.StringH
            print_stringh(io, n, s)
        else
            print_tree(io, n, s)
        end

        if n.typ === NEWLINE && s.on && i < length(nodes)
            if is_closer(nodes[i+1]) ||
               nodes[i+1].typ === CSTParser.Block || nodes[i+1].typ === CSTParser.Begin
                write(io, repeat(" ", max(nodes[i+1].indent, 0)))
                s.line_offset = nodes[i+1].indent
            elseif !skip_indent(nodes[i+1])
                write(io, ws)
                s.line_offset = indent
            end
        end
    end
end

function print_stringh(io::IOBuffer, x::PTree, s::State)
    # The indent of StringH is set to the the offset
    # of when the quote is first encountered in the source file.

    # This difference notes if there is a change due to nesting.
    diff = x.indent + 1 - s.line_offset
    # @info "" length(x) s.line_offset

    # The new indent for the string is index of when a character in
    # the multiline string is FIRST encountered in the source file - the above difference
    # +1 since the character is 1 space after the indent
    x.indent = max(x.nodes[1].indent + 1 - diff, 0)
    print_tree(io, x, s)
end

function print_notcode(io::IOBuffer, x::PTree, s::State)
    s.on || return
    for l = x.startline:x.endline
        ws, v = get(s.doc.comments, l, (0, "\n"))
        v == "" && continue
        if l == x.endline && v[end] == '\n'
            v = v[1:prevind(v, end)]
        end
        ws > 0 && write(io, repeat(" ", ws))
        write(io, v)
        if l != x.endline && v[end] != '\n'
            write(io, "\n")
        end
    end
end

function print_inlinecomment(io::IOBuffer, x::PTree, s::State)
    s.on || return
    ws, v = get(s.doc.comments, x.startline, (0, ""))
    isempty(v) && return
    v = v[end] == '\n' ? v[nextind(v, 1):prevind(v, end)] : v
    ws > 0 && write(io, repeat(" ", ws))
    write(io, v)
end
