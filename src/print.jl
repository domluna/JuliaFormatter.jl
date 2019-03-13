function print_tree(io::IOBuffer, x::PTree)
    #= indent == -1 ? x.indent : indent =#
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
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n)
        if is_nl(n) && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            write(io, repeat(" ", x.nodes[i+1].indent))
        elseif is_nl(n)
            write(io, wspace)
        end
    end
end

function print_tree(io::IOBuffer, x::PLeaf)
    write(io, x.text)
end

Base.write(io::IOBuffer, x::PTree) = print_tree(io, x)
