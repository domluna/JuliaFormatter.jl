# Print the PTree.

print_tree(io::IOBuffer, x::PLeaf, ::State) = write(io, x.text)
print_tree(io::IOBuffer, ::Newline, ::State) = write(io, "\n")
print_tree(io::IOBuffer, ::Semicolon, ::State) = write(io, ";")
print_tree(io::IOBuffer, ::Whitespace, ::State) = write(io, " ")
print_tree(io::IOBuffer, ::Placeholder, ::State) = write(io, "")
print_tree(io::IOBuffer, ::PlaceholderWS, ::State) = write(io, " ")
print_tree(io::IOBuffer, ::Spaces, ::State) = nothing

function print_tree(io::IOBuffer, x::PTree, s::State)
    wspace = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            write(io, repeat(" ", x.nodes[i+1].indent))
            #= write(io, wspace) =#
        elseif n === newline
            write(io, wspace)
        end
    end
end

function print_tree(io::IOBuffer, x::PTree{CSTParser.EXPR{CSTParser.If}}, s::State)
    wspace = repeat(" ", x.indent)
    n1 = x.nodes[1]
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline
            if x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif x.nodes[i+1] isa PLeaf{CSTParser.KEYWORD}
                v = x.nodes[i+1].text
                if n1 isa PLeaf{CSTParser.KEYWORD} && n1.text == "if " 
                    write(io, wspace)
                elseif v == "elseif" || v == "else"
                    if wspace != ""
                        write(io, repeat(" ", x.indent - s.indent_width))
                    end
                else
                    write(io, wspace)
                end
            else
                write(io, wspace)
            end
        end
    end
end
