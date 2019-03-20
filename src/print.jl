# Print the PTree.
#
# TODO: comments

print_tree(io::IOBuffer, x::PLeaf, ::State) = write(io, x.text)
print_tree(io::IOBuffer, ::Newline, ::State) = write(io, "\n")
print_tree(io::IOBuffer, ::Semicolon, ::State) = write(io, ";")
print_tree(io::IOBuffer, ::Whitespace, ::State) = write(io, " ")
print_tree(io::IOBuffer, ::Placeholder, ::State) = write(io, "")
print_tree(io::IOBuffer, ::PlaceholderWS, ::State) = write(io, " ")

function print_tree(io::IOBuffer, x::PTree, s::State)
    ws = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            write(io, repeat(" ", x.nodes[i+1].indent))
        elseif n === newline
            write(io, ws)
        end
    end
end

function print_tree(io::IOBuffer, x::PTree{CSTParser.EXPR{CSTParser.If}}, s::State)
    ws = repeat(" ", x.indent)
    n1 = x.nodes[1]
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline
            if x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif x.nodes[i+1] isa PLeaf{CSTParser.KEYWORD}
                v = x.nodes[i+1].text
                if n1 isa PLeaf{CSTParser.KEYWORD} && n1.text == "if " 
                    write(io, ws)
                elseif v == "elseif" || v == "else"
                    if ws != ""
                        write(io, repeat(" ", x.indent - s.indent_width))
                    end
                else
                    write(io, ws)
                end
            else
                write(io, ws)
            end
        end
    end
end

function print_tree(io::IOBuffer, x::Comment, s::State)
    comment_range = x.endline+1:x.startline-1
    ws = repeat(" ", x.indent)
    for (i, l) in enumerate(comment_range)
        v = s.doc.text[s.doc.ranges[l]]

        @info l, v

        # remove extra newlines
        if i < length(comment_range) && v == "\n"
            vn = s.doc.text[s.doc.ranges[l+1]]
            v == vn && (continue)
        end

        if v == "\n"
            #= comment_text = rstrip(comment_text, ' ') * v * w =#
            write(io, v)
            write(io, ws)
            continue
        end

        i = first(findfirst(x -> !isspace(x), v))
        if v[i] == '#'
            write(io, v[i:end])
            write(io, ws)
        else
            # This captures the possible additional indentation in a docstring
            i = max(min(i, s.indents-1 * s.indent_width), 1)
            write(io, v[i:end])
            write(io, ws)
        end
    end
end
