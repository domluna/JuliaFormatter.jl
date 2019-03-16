# Nest
#
# If the line exceeds the maximum width it will be nested.
# 
# This is by replacing `Placeholder` nodes with `Newline` nodes
# and updated the PTree's indent.

#= nest!(::PLeaf, ::State) = nothing =#


rem_length(x::AbstractVector) = reverse(cumsum(reverse(length.(x))))

# nest is a versionf of nest! that doesn't mutate the PTree
# it just increments/resets the line_offset.
# 
# This is useful for correctly resetting the line_offset after
# nesting a recursive node

nest(x::PLeaf, s::State) = (s.line_offset += length(x); nothing)

function nest(x::PTree, s::State)
    @info "" typeof(x) s.line_offset
    for (i, n) in enumerate(x.nodes)
        if n === Newline
            s.line_offset = x.indent
        else
            nest(n, s)
        end
    end
end

nest!(x::PLeaf, s::State) = (s.line_offset += length(x); nothing)

function nest!(x::PTree, s::State)
    @info "" typeof(x) s.line_offset
    for (i, n) in enumerate(x.nodes)
        if n === Newline
            s.line_offset = x.indent
        else
            nest!(n, s)
        end
    end
end


# 
function nest!(x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.Curly,CSTParser.Call}
    @info "" typeof(x) s.line_offset x length(x)
    if s.line_offset + length(x) > s.max_width
        x.indent = s.line_offset + length(x.nodes[1]) + length(x.nodes[2])
        s.line_offset = x.indent

        lens = rem_length(x.nodes[3:end])
        @info "" lens s.line_offset
        for (i, n) in enumerate(x.nodes[3:end])
            #= @info "" typeof(n) n s.line_offset =#
            if n === Newline
                s.line_offset = x.indent
            elseif is_placeholder(n) && s.line_offset + lens[i] > s.max_width
                x.nodes[i+2] = Newline
                s.line_offset = x.indent
            elseif n === Semicolon && s.line_offset + lens[i] > s.max_width
                x.nodes[i+3] = Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    else
        for (i, n) in enumerate(x.nodes)
            if n === Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State) where T <:  Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.InvisBrackets,CSTParser.Braces}
    @info "" typeof(x) s.line_offset length(x)
    if s.line_offset + length(x) > s.max_width
        x.indent = s.line_offset
        x.nodes[1] isa PLeaf{CSTParser.PUNCTUATION} && (x.indent += 1)

        lens = rem_length(x.nodes)
        for (i, n) in enumerate(x.nodes)
            if n == Newline
                s.line_offset = x.indent
            elseif is_placeholder(n) && s.line_offset + lens[i] > s.max_width
                x.nodes[i] = Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    else
        for (i, n) in enumerate(x.nodes)
            if n === Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

# A where B
#
# nest B prior to nesting A
# only nest A if the line still exceeds the maximum width
function nest!(x::PTree{CSTParser.WhereOpCall}, s::State)
    @info "" typeof(x) s.line_offset length(x)
    if s.line_offset + length(x) > s.max_width
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> is_placeholder(n), x.nodes)[1]
        Alens = rem_length(x.nodes[1:idx-1])
        Blens = rem_length(x.nodes[idx+1:end])

        s.line_offset += Alens[1]
        x.indent = s.line_offset
        is_lbrace(x.nodes[idx+1]) && (x.indent += 1)

        @info "" typeof(x) s.line_offset x length(x.nodes)
        @info "" idx Alens Blens

        # B
        for (i, n) in enumerate(x.nodes[idx+1:end])
            if n == Newline
                s.line_offset = x.indent
            elseif is_placeholder(n) && s.line_offset + Blens[i] > s.max_width
                x.nodes[i+idx] = Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end

        # A where
        s.line_offset = line_offset
        for (i, n) in enumerate(x.nodes[1:idx-1])
            nest!(n, s)
        end

        # If A was nested we need to redo the indent for B
        # TODO: need to propagate new indent
        if line_offset + Alens[1] > s.max_width
            x.indent = s.line_offset
            if is_lbrace(x.nodes[idx+1])
                x.indent += 1
            elseif x.nodes[idx+1] isa PTree{CSTParser.EXPR{CSTParser.Curly}}
                # reset the indent of the Curly node, +1 for {
                x.nodes[idx+1].indent = x.indent + length(x.nodes[idx+1].nodes[1]) + 1
                #= x.indent = x.nodes[idx+1].indent =#
            elseif x.nodes[idx+1] isa PTree{<:Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}}
                # NOTE: don't need this if a binary call of ISSUBTYPE is not nestable
                # T <: S
                x.nodes[idx+1].indent = x.indent
            end
        end

        s.line_offset = line_offset
        nest(x, s)
        @info "EXIT" typeof(x) s.line_offset
    else
        for (i, n) in enumerate(x.nodes)
            @info "WHEREOP" typeof(n) s.line_offset length(n)
            if n === Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

# C ? E1 : E2
function nest!(x::PTree{CSTParser.ConditionalOpCall}, s::State)
    @info "" typeof(x) s.line_offset x
    if s.line_offset + length(x) > s.max_width
        #= idx1 = findfirst(n -> is_placeholder(n), x.nodes)[1] =#
        #= idx2 = findlast(n -> is_placeholder(n), x.nodes)[1] =#
        #= Clens = rem_length(x.nodes[1:idx1]) =#
        #= E1lens = rem_length(x.nodes[idx1+1:idx2]) =#
        #= E2lens = rem_length(x.nodes[idx2+1:end]) =#
        
        x.indent = s.line_offset
        lens = length.(x.nodes)
        alllens = rem_length(x.nodes)
        idx = length(lens)
        for (i, n) in enumerate(reverse(x.nodes))
            s.line_offset = x.indent + alllens[i]
            l = sum(lens[end-i+2:idx])
            #= @info "" typeof(n) s.line_offset n l =#
            if n == Newline
                s.line_offset = x.indent
            elseif is_placeholder(n) && s.line_offset + l > s.max_width
                x.nodes[end-i+1] = Newline
                idx = length(lens)-i
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
        # TODO: set line_offset
        s.line_offset = x.indent + length(x)
    else
        for (i, n) in enumerate(x.nodes)
            @info "" typeof(n) s.line_offset length(n)
            if n === Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

# arg1 op arg2
function nest!(x::PTree{T}, s::State; indent=-1) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    @info "ENTER" typeof(x) s.line_offset x length(x)
    if s.line_offset + length(x) > s.max_width && findfirst(n -> is_placeholder(n), x.nodes) !== nothing
        line_offset = s.line_offset
        # idx op is 1 before the placeholder
        idx = findfirst(n -> is_placeholder(n), x.nodes)[1] - 1

        arg1lens = rem_length(x.nodes[1:idx])
        arg2lens = rem_length(x.nodes[idx+1:end])

        x.indent = s.line_offset
        s.line_offset += arg1lens[1]

        @info "" typeof(x) s.line_offset x arg2lens[1]
        @info "" arg1lens arg2lens

        if s.line_offset + arg2lens[1] > s.max_width
            x.nodes[idx+1] = Newline
            s.line_offset = x.indent
        end
        # arg2
        @info "nesting arg2"
        nest!(x.nodes[end], s)

        # arg1 op
        s.line_offset = line_offset
        @info "nesting arg1"
        for (i, n) in enumerate(x.nodes[1:idx])
            nest!(n, s)
        end

        s.line_offset = line_offset
        nest(x, s)
        @info "EXIT" typeof(x) s.line_offset
    else
        for (i, n) in enumerate(x.nodes)
            @info "" typeof(n) s.line_offset length(n)
            if n === Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end
