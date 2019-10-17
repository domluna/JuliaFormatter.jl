# state has the ranges where formatting is off
# essentially we 

function skip_indent(x)
    if x.typ === CSTParser.LITERAL && x.val == ""
        return true
    elseif x.typ === NEWLINE || x.typ === NOTCODE
        return true
    end
    false
end

function skip_format(x, s)
    length(s.doc.format_skips) > 0 || return false
    skip = s.doc.format_skips[1]
    x.startline > skip[1] && x.endline < skip[2] ? true : false
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
    if skip_format(x, s)
    end

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
            if is_closer(x.nodes[i+1]) ||
                x.nodes[i+1].typ === CSTParser.Block ||
                x.nodes[i+1].typ === CSTParser.Begin
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif !skip_indent(x.nodes[i+1])
                write(io, ws)
            end
        end
    end
end

@inline function print_notcode(io, x, s)
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

@inline function print_inlinecomment(io, x, s)
    ws, v = get(s.doc.comments, x.startline, (0, ""))
    isempty(v) && return
    v = v[end] == '\n' ? v[nextind(v, 1):prevind(v, end)] : v
    ws > 0 && write(io, repeat(" ", ws))
    write(io, v)
end

@inline function print_noformat(io, x, s)
    for l = x.startline:x.endline
        r = s.doc.line_to_range[l]
        v = s.doc.text[s.doc.ranges[r]]
        v == "" && continue
        if l == x.endline && v[end] == '\n'
            v = v[1:prevind(v, end)]
        end
        write(io, v)
        if l != x.endline && v[end] != '\n'
            write(io, "\n")
        end
    end
end
