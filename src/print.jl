is_block(x) = x.typ === CSTParser.Block

function skip_indent(x)
    if x.typ === CSTParser.LITERAL && x.val == ""
        return true
    elseif x.typ === NEWLINE || x.typ === NOTCODE
        return true
    end
    false
end

function print_leaf(io, x, s)
    if x.typ === NOTCODE
        print_notcode(io, x, s)
    elseif x.typ === INLINECOMMENT
        print_inlinecomment(io, x, s)
    else
        write(io, x.val)
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

        if n.typ === NEWLINE && i < length(x.nodes)
            if is_closer(x.nodes[i+1])
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif is_block(x.nodes[i+1])
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif !skip_indent(x.nodes[i+1])
                write(io, ws)
            end
        end
    end
end

@inline function print_notcode(io, x, s)
    prev_nl = true
    for l in x.startline:x.endline
        v = getline(s.doc, l)
        v == "" && continue
        # @info "comment line" l v
        if l == x.endline && v[end] == '\n'
            v = v[1:end-1]
        end
        write(io, v)
        prev_nl = v == "\n" ? true : false
    end
end

@inline function print_inlinecomment(io, x, s)
    v = get(s.doc.comments, x.startline, "")
    v == "" && return
    v = getline(s.doc, x.startline)
    idx = findlast(c -> c == '#', v)
    idx === nothing && return
    idx = findlast(c -> !isspace(c), v[1:idx-1])
    v = v[end] == '\n' ? v[idx+1:end-1] : v[idx+1:end]
    write(io, v)
end


