is_block(x) = x.typ === CSTParser.Block #|| x.typ === CSTParser.StringH

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

function print_notcode(io, x, s)
    ws = repeat(" ", x.indent)
    # `NOTCODE` nodes always follow a `NEWLINE` node.
    prev_nl = true
    for l in x.startline:x.endline
        v = get(s.doc.comments, l, "\n")
        # if l == x.startline && v != "\n"
        #     write(io, ws)
        if l == x.endline && v != "\n"
            write(io, ws)
            write(io, v[1:end])
        elseif l == x.endline && v == "\n"
        elseif v == "\n"
            write(io, v)
            !prev_nl && (write(io, ws))
            prev_nl = true
        else
            write(io, ws)
            write(io, v)
            write(io, "\n")
            prev_nl = false
        end
    end
end

function print_inlinecomment(io, x, s)
    v = get(s.doc.comments, x.startline, "")
    v == "" && (return)
    write(io, " ")
    write(io, v)
end


