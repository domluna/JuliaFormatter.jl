function skip_indent(x::PTree)
    if x.typ === CSTParser.LITERAL && x.val == ""
        return true
    elseif x.typ === NEWLINE || x.typ === NOTCODE
        return true
    end
    false
end

function format_check(io::IOBuffer, x::PTree, s::State)
    if length(s.doc.format_skips) == 0
        print_notcode(io, x, s)
        return
    end

    skip = s.doc.format_skips[1]
    if skip[1] in x.startline:x.endline && s.on
        x.endline = skip[1] - 1
        print_notcode(io, x, s)
        x.endline > 1 && write(io, "\n")
        write(io, skip[3])
        s.on = false
    elseif skip[2] in x.startline:x.endline && !s.on
        deleteat!(s.doc.format_skips, 1)
        s.on = true
        # change the startline, otherwise lines
        # prior to in the NOTCODE node prior to 
        # "format: on" will be reprinted
        x.startline = skip[2]
        print_notcode(io, x, s)
        # previous NEWLINE node won't be printed
        # write(io, "\n")
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
end

function print_tree(io::IOBuffer, x::PTree, s::State)
    if is_leaf(x)
        print_leaf(io, x, s)
        return
    end

    ws = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        if is_leaf(n)
            print_leaf(io, n, s)
        else
            print_tree(io, n, s)
        end

        if n.typ === NEWLINE && s.on && i < length(x.nodes)
            if is_closer(x.nodes[i+1]) ||
               x.nodes[i+1].typ === CSTParser.Block || x.nodes[i+1].typ === CSTParser.Begin
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif !skip_indent(x.nodes[i+1])
                write(io, ws)
            end
        end
    end
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

function print_noformat(io::IOBuffer, skip::Tuple{Int,Int}, s::State)
    startline = skip[1] + 1
    endline = skip[2] == -1 ? length(s.doc.ranges) : skip[2] - 1
    # @info "writing noformat" skip startline:endline
    for l = startline:endline
        l == startline && write(io, "\n")
        r = s.doc.line_to_range[l]
        v = s.doc.text[r]
        v == "" && continue
        # @info "writing line" l r v
        if l == endline && v[end] == '\n'
            v = v[1:prevind(v, end)]
        end
        write(io, v)
        if l != endline && v[end] != '\n'
            write(io, "\n")
        end
    end
end
