# Nest
#
# If the line exceeds the maximum width it will be nested.
# 
# This is by replacing `Placeholder` nodes with `Newline` nodes
# and updated the PTree's indent.

nest!(::PLeaf, ::State) = nothing

function nest!(x::PTree, s::State)
    @info "" typeof(x) s.line_offset
    for (i, n) in enumerate(x.nodes)
        if n === Newline
            s.line_offset = x.indent
        else
            nest!(n, s)
            s.line_offset += length(n)
        end
    end
end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.Curly,CSTParser.Call}
    @info "" typeof(x) s.line_offset x
    if s.line_offset + length(x) > s.max_width
        x.indent = s.line_offset + length(x.nodes[1]) + length(x.nodes[2])
        for i in 1:length(x.nodes)
            n = x.nodes[i]
            #= @info "HERE" typeof(n) =#
            #= n isa PLeaf && (@info "" n.text n.text == "") =#
            if n === Placeholder
                x.nodes[i] = Newline
            elseif n === Semicolon
                x.nodes[i+1] = Newline
            end

            nest!(n, s)
            s.line_offset += length(n)
        end
    end
end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State) where T <:  Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.InvisBrackets,CSTParser.Braces}
    @info "" typeof(x) s.line_offset
    if s.line_offset + length(x) > s.max_width
        x.indent = s.line_offset
        x.nodes[1] isa PLeaf{CSTParser.PUNCTUATION} && (x.indent += 1)
        for i in 1:length(x.nodes)
            n = x.nodes[i]

            if n === Placeholder
                x.nodes[i] = Newline
            end

            nest!(n, s)
            s.line_offset += length(n)
        end
    end
end

# a where b
#
# nest b prior to nesting a
# only nest a if the line still exceeds the maximum width
function nest!(x::PTree{CSTParser.WhereOpCall}, s::State)
    bidx = findlast(n -> n === Placeholder, x.nodes)[1] + 1
    line_offset = s.line_offset
    alen = sum(length.(x.nodes[1:bidx-1]))
    blen = sum(length.(x.nodes[bidx:end]))

    @info "" typeof(x) s.line_offset bidx x alen blen

    if s.line_offset + length(x) > s.max_width
        #= x.nodes[bidx-1] = Newline =#
        for i in bidx:length(x.nodes)
            n = x.nodes[i]
            nest!(n, s)
            s.line_offset += length(n)
        end
    end

    s.line_offset = line_offset
    if s.line_offset + alen > s.max_width
        x.indent = s.line_offset
        for i in 1:bidx-1
            n = x.nodes[i]
            nest!(n, s)
            s.line_offset += length(n)
        end
    end
    s.line_offset += blen
end

#= function nest!(x::PTree{CSTParser.ConditionalOpCall}, s::State) =#
#=     @info "" typeof(x) s.line_offset =#
#=     if s.line_offset + length(x) > s.max_width =#
#=         x.indent = s.line_offset =#
#=         x.nodes[1] isa PLeaf{CSTParser.PUNCTUATION} && (x.indent += 1) =#
#=         for i in 1:length(x.nodes) =#
#=             n = x.nodes[i] =#
#=  =#
#=             if n === Placeholder =#
#=                 x.nodes[i] = Newline =#
#=             end =#
#=  =#
#=             nest!(n, s) =#
#=             s.line_offset += length(n) =#
#=         end =#
#=     end =#
#= end =#
#=  =#
#= function nest(x::Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}, s::State) =#
#=     @info "" typeof(x) s.line_offset =#
#=     if s.line_offset + length(x) > s.max_width =#
#=         x.indent = s.line_offset =#
#=         x.nodes[1] isa PLeaf{CSTParser.PUNCTUATION} && (x.indent += 1) =#
#=         for i in 1:length(x.nodes) =#
#=             n = x.nodes[i] =#
#=  =#
#=             if n === Placeholder =#
#=                 x.nodes[i] = Newline =#
#=             end =#
#=  =#
#=             nest!(n, s) =#
#=             s.line_offset += length(n) =#
#=         end =#
#=     end =#
#= end =#
