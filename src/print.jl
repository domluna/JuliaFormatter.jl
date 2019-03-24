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
        if n === newline
            if x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif x.nodes[i+1] isa Comment
                write(io, repeat(" ", x.nodes[i+1].indent))
            else
                write(io, ws)
            end
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
            elseif x.nodes[i+1] isa Comment
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
    r = x.startline:x.endline
    @info "PRINTING COMMENT" r x.indent x
    ws = repeat(" ", x.indent)

    for (i, l) in enumerate(r)
        v = s.doc.text[s.doc.ranges[l]]
        idx = findfirst(x -> !isspace(x), v)
        @info "" l v idx
        if idx === nothing
            v == "\n" && (write(io, v); write(io, ws))
        elseif v[idx] == '#'
            write(io, v[idx:end])
            write(io, ws)
        end
    end
end
