# state has the ranges where formatting is turned off
# 
# would this work?
#
# if x.startline == skip[1] && s.on
#   skip = popfirst!(s)
#   print_noformat(io, skip, s)
#   s.on = false
# elseif x.endline == skip[1] && !s.on
#   s.on = true
# end
#

function skip_indent(x)
    if x.typ === CSTParser.LITERAL && x.val == ""
        return true
    elseif x.typ === NEWLINE || x.typ === NOTCODE
        return true
    end
    false
end

function format_check(x::PTree, s::State)
    length(s.doc.format_skips) > 0 || return
    skip = s.doc.format_skips[1]
    if x.startline == skip[1] && s.on
  print_noformat(io, skip, s)
  s.on = false
    elseif x.endline == skip[2] && !s.on
        deleteat!(s.doc.format_skips, 1)
        s.on = true
    end
end

function print_leaf(io, x, s)
    s.on || return
    if x.typ === NOTCODE
        print_notcode(io, x, s)
    elseif x.typ === INLINECOMMENT
        print_inlinecomment(io, x, s)
    else
        write(io, x.val)
    end
end

function print_tree(io::IOBuffer, x::PTree, s::State)
    format_check!(x, s)

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
               x.nodes[i+1].typ === CSTParser.Block || x.nodes[i+1].typ === CSTParser.Begin
                s.on && write(io, repeat(" ", x.nodes[i+1].indent))
            elseif !skip_indent(x.nodes[i+1])
                s.on && write(io, ws)
            end
        end
    end
end

@inline function print_notcode(io::IOBuffer, x::PTree, s::State)
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

@inline function print_inlinecomment(io::IOBuffer, x::PTree, s::State)
    ws, v = get(s.doc.comments, x.startline, (0, ""))
    isempty(v) && return
    v = v[end] == '\n' ? v[nextind(v, 1):prevind(v, end)] : v
    ws > 0 && write(io, repeat(" ", ws))
    write(io, v)
end

@inline function print_noformat(io::IOBuffer, skip::UnitRange{Int}, s::State)
    for l = skip[1]:skip[2]
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
