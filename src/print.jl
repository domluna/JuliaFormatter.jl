
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
print_tree(io::IOBuffer, ::Whitespace, ::State) = write(io, " ")
print_tree(io::IOBuffer, ::Placeholder, ::State) = write(io, "")
print_tree(io::IOBuffer, ::PlaceholderWS, ::State) = write(io, " ")

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
    ws = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline && i < length(x.nodes)
            if is_closer(x.nodes[i+1])
                w = min(s.indent_size, length(x.nodes[1]) + length(x.nodes[2]))
                write(io, ws[1:end-w])
            elseif is_block(x.nodes[i+1])
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif !skip_indent(x.nodes[i+1])
                write(io, ws)
            end
        end
    end
end

function print_tree(io::IOBuffer, x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.Braces,CSTParser.Vect}
    ws = repeat(" ", x.indent)
    for (i, n) in enumerate(x.nodes)
        print_tree(io, n, s)
        if n === newline && i < length(x.nodes)
            if is_closer(x.nodes[i+1])
                write(io, ws[1:end-1])
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
                write(io, ws[1:end-1])
            elseif is_block(x.nodes[i+1])
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif !skip_indent(x.nodes[i+1])
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
        if n === newline && i < length(x.nodes)
            if is_block(x.nodes[i+1])
                write(io, repeat(" ", x.nodes[i+1].indent))
            elseif x.nodes[i+1] isa PLeaf{CSTParser.KEYWORD}
                v = x.nodes[i+1].text
                if n1 isa PLeaf{CSTParser.KEYWORD} && n1.text == "if " 
                    write(io, ws)
                elseif v == "elseif" || v == "else"
                    if ws != ""
                        write(io, repeat(" ", x.indent - s.indent_size))
                    end
                else
                    write(io, ws)
                end
            elseif !skip_indent(x.nodes[i+1])
                write(io, ws)
            end
        end
    end
end


function print_tree(io::IOBuffer, x::NotCode, s::State)
    @info "PRINTING NOTCODE" x
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


