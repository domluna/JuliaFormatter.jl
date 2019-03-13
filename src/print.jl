function print_tree(io::IOBuffer, x::PTree)
    #= indent == -1 ? x.indent : indent =#
    wspace = repeat(" ", x.indent)
    for n in x.nodes
        print_tree(io, n)
        if is_nl(n)
            write(io, wspace)
        end
    end
end

function print_tree(io::IOBuffer, x::PLeaf)
    write(io, x.text)
end

Base.write(io::IOBuffer, x::PTree) = print_tree(io, x)
