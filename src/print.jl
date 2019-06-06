is_block(_) = false
is_block(::PTree{CSTParser.EXPR{CSTParser.Block}}) = true
is_block(::PTree{CSTParser.EXPR{CSTParser.StringH}}) = true

skip_indent(_) = false
skip_indent(x::PLeaf{CSTParser.LITERAL}) = x.text == ""
skip_indent(::Newline) = true
skip_indent(::NotCode) = true

print_tree(io::IOBuffer, x::PLeaf, ::State) = write(io, x.text)
print_tree(io::IOBuffer, ::Newline, ::State) = write(io, "\n")
print_tree(io::IOBuffer, ::Semicolon, ::State) = write(io, ";")
print_tree(io::IOBuffer, x::Whitespace, ::State) = write(io, repeat(" ", x.n))
print_tree(io::IOBuffer, x::PlaceholderWS, ::State) = write(io, repeat(" ", x.n))
print_tree(io::IOBuffer, ::Placeholder, ::State) = write(io, "")
print_tree(io::IOBuffer, x::TrailingComment, ::State) = write(io, x.text)

function print_tree(io::IOBuffer, x::PTree, s::State)
    ws = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline && i < length(x.nodes)
            if is_block(x.nodes[i+1])
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif !skip_indent(x.nodes[i+1])
                write(io, ws)
            end
        end
    end
end

function print_tree(io::IOBuffer, x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.Call,CSTParser.Curly,CSTParser.MacroCall}
    # @info "" x.indent x.nodes[1]
    ws = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline && i < length(x.nodes)
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

function print_tree(io::IOBuffer, x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.Braces,CSTParser.Vect,CSTParser.InvisBrackets,CSTParser.Parameters}
    # @info "" x.indent x.nodes[1]
    ws = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline && i < length(x.nodes)
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

function print_tree(io::IOBuffer, x::PTree{CSTParser.WhereOpCall}, s::State)
    ws = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline && i < length(x.nodes)
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

function print_tree(io::IOBuffer, x::NotCode, s::State)
    ws = repeat(" ", x.indent)
    # `NotCode` nodes always follow a `Newline` node.
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
            print_tree(io, newline, s)
            prev_nl = false
        end
    end
end


