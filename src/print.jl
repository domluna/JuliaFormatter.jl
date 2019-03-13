function print_tree(io::IOBuffer, x::PTree)
    wspace = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n)
        if is_nl(n) && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            write(io, repeat(" ", x.nodes[i+1].indent))
        elseif is_nl(n)
            write(io, wspace)
        end
    end
end

function print_tree(io::IOBuffer, x::PTree{CSTParser.EXPR{CSTParser.If}})
    wspace = repeat(" ", x.indent)
    n1 = x.nodes[1]
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n)
        if is_nl(n) 
            if x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif x.nodes[i+1] isa PLeaf{CSTParser.KEYWORD}
                s = x.nodes[i+1].text
                if n1 isa PLeaf{CSTParser.KEYWORD} && n1.text == "if " 
                    write(io, wspace)
                elseif s == "elseif" || s == "else"
                    if wspace != ""
                        write(io, repeat(" ", x.indent - 4))
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

print_tree(io::IOBuffer, x::PLeaf) = write(io, x.text)

Base.write(io::IOBuffer, x::PTree) = print_tree(io, x)
