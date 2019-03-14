# Print the PTree.
#

function print_tree(io::IOBuffer, x::PTree, s::State)
    wspace = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === Newline && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            write(io, repeat(" ", x.nodes[i+1].indent))
        elseif n === Newline
            write(io, wspace)
        end
    end
end

function print_tree(io::IOBuffer, x::PTree{CSTParser.EXPR{CSTParser.If}}, s::State)
    wspace = repeat(" ", x.indent)
    n1 = x.nodes[1]
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === Newline
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

print_tree(io::IOBuffer, x::PLeaf, ::State) = write(io, x.text)
