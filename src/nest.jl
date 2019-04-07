# Nest
#
# If the line exceeds the maximum length it will be nested.
# 
# This is done by replacing `Placeholder` nodes with `Newline` 
# nodes and updating the PTree's indent.
#
# `addlen` provides additional length to consider from
# the top-level node.
#
# Example:
#
# arg1 op arg2
#
# the length of " op" will be considered when nesting arg1

remaining_lengths(x::AbstractVector) = reverse(cumsum(reverse(length.(x))))

walk(f, x::AbstractPLeaf, s::State) = f(x, s)
function walk(f, x::PTree, s::State)
    f(x, s)
    for n in x.nodes
        if n === newline
            s.line_offset = x.indent
        else
            walk(f, n, s)
        end
    end
end

# Used to correctly reset the State's line_offset. 
reset_line_offset(_, _) = nothing
reset_line_offset(x::AbstractPLeaf, s::State) = (s.line_offset += length(x); nothing)
nest!(x::AbstractPLeaf, s::State; addlen=0) = (s.line_offset += length(x); nothing)

function nest!(x::PTree, s::State; addlen=0)
    for (i, n) in enumerate(x.nodes)
        if n === newline && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            # correct the offset
            s.line_offset = x.nodes[i+1].indent
        elseif n === newline
            s.line_offset = x.indent
        else
            nest!(n, s, addlen=addlen)
        end
    end
end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; addlen=0) where T <: Union{CSTParser.Import,CSTParser.Using,CSTParser.Export}
    #= @info "ENTER" typeof(x) s.line_offset x =#
    line_length = s.line_offset + length(x) + addlen
    idx = findfirst(n -> is_placeholder(n), x.nodes)
    if idx !== nothing && line_length > s.max_line_length
        # -3 due to the placeholder being ahead of a comma
        # and another node
        x.indent = s.line_offset + sum(length.(x.nodes[1:idx-3]))
        s.line_offset = x.indent
        line_offset = s.line_offset

        for (i, n) in enumerate(x.nodes[idx-2:end])
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                x.nodes[i+idx-3] = newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    else
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
    #= @info "EXIT" typeof(x) s.line_offset x =#
end

# TODO: InvisBrackets might not need to be nested at all
function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; addlen=0) where T <: Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.Braces,CSTParser.InvisBrackets,CSTParser.Parameters}
    line_length = s.line_offset + length(x) + addlen
    idx = findlast(n -> is_placeholder(n), x.nodes)
    #= @info "ENTER" typeof(x) s.line_offset line_length =#
    if idx !== nothing && line_length > s.max_line_length
        line_offset = s.line_offset
        x.indent = is_opener(x.nodes[1]) ? s.line_offset + 1 : s.line_offset
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                x.nodes[i] = newline
                s.line_offset = x.indent
            else
                nest!(n, s, addlen=1)
            end
        end
        s.line_offset = is_closer(x.nodes[end]) ? line_offset + 1 : line_offset
    else
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; addlen=0) where T <: Union{CSTParser.Curly,CSTParser.Call}
    line_length = s.line_offset + length(x) + addlen
    idx = findlast(n -> is_placeholder(n), x.nodes)
    # @info "ENTER" typeof(x) s.line_offset line_length addlen
    if idx !== nothing && line_length > s.max_line_length
        line_offset = s.line_offset
        name_width = length(x.nodes[1]) + length(x.nodes[2])
        x.indent = s.line_offset + min(name_width, s.indent_size)

        for (i, n) in enumerate(x.nodes)
            # closing punctuation should be aligned with the
            # call name:
            #
            # foo(
            #      a,
            #      b,
            #      c
            # )
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                x.nodes[i] = newline
                s.line_offset = x.indent
            else
                nest!(n, s, addlen=1)
            end
        end
        s.line_offset = line_offset + 1
    else
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

function nest!(x::PTree{CSTParser.EXPR{CSTParser.MacroCall}}, s::State; addlen=0)
    line_length = s.line_offset + length(x) + addlen
    idx = findlast(n -> is_placeholder(n), x.nodes)
    #= @info "ENTER" typeof(x) s.line_offset line_length addlen =#
    if idx !== nothing && line_length > s.max_line_length && !(x.nodes[1] isa PTree{CSTParser.EXPR{CSTParser.GlobalRefDoc}})
        line_offset = s.line_offset
        name_width = length(x.nodes[1]) + length(x.nodes[2])
        x.indent = s.line_offset + min(name_width, s.indent_size)

        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                x.nodes[i] = newline
                s.line_offset = x.indent
            else
                nest!(n, s, addlen=1)
            end
        end
        s.line_offset = line_offset
        is_closer(x.nodes[end]) && (s.line_offset += 1)
    else
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

function nest!(x::PTree{CSTParser.WhereOpCall}, s::State; addlen=0)
    #= @info "ENTER" typeof(x) s.line_offset x =#
    line_length = s.line_offset + length(x) + addlen
    if line_length > s.max_line_length
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> is_placeholder(n), x.nodes)
        #= Blens = length.(x.nodes[idx+1:end]) =#
        Blens = remaining_lengths(x.nodes[idx+1:end])

        # A, +7 is the length of " where "
        nest!(x.nodes[1], s, addlen=Blens[1] + 7)

        for (i, n) in enumerate(x.nodes[2:idx-1])
            nest!(n, s)
        end

        x.indent = s.line_offset
        has_brace = is_opener(x.nodes[idx+1])
        has_brace && (x.indent += 1)
        # where B
        over = s.line_offset + Blens[1] > s.max_line_length
        for (i, n) in enumerate(x.nodes[idx+1:end])
            @info "" s.line_offset n
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) && over
                x.nodes[i+idx] = newline
                s.line_offset = x.indent
            elseif has_brace
                nest!(n, s, addlen=1)
            else
                nest!(n, s, addlen=0)
            end
        end
        (over && has_brace) && (s.line_offset -= 1)
    else
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
    #= @info "EXIT" typeof(x) s.line_offset =#
end

# C ? E1 : E2
#
# nest in order of
#
# C ? E1 :
# E2
#
# C ?
# E1 :
# E2
function nest!(x::PTree{CSTParser.ConditionalOpCall}, s::State; addlen=0)
    #= @info "ENTER" typeof(x) s.line_offset =#
    line_length = s.line_offset + length(x) + addlen
    if line_length > s.max_line_length
        idx1 = findfirst(n -> is_placeholder(n), x.nodes)
        idx2 = findlast(n -> is_placeholder(n), x.nodes)
        Clens = remaining_lengths(x.nodes[1:idx1])
        E1lens = remaining_lengths(x.nodes[idx1+1:idx2])

        line_offset = s.line_offset
        x.indent = s.line_offset

        x.nodes[idx2] = newline
        nest!(x.nodes[end], s)

        # E1
        s.line_offset = line_offset + Clens[1]
        if s.line_offset + E1lens[1] > s.max_line_length
            x.nodes[idx1] = newline
            s.line_offset = x.indent
        end
        nest!(x.nodes[idx1+1], s, addlen=2)
        for n in x.nodes[idx1+2:idx2-1]
            nest!(n, s)
        end

        # C
        s.line_offset = line_offset
        nest!(x.nodes[1], s, addlen=2)
        for n in x.nodes[2:idx1-1]
            nest!(n, s)
        end

        # reset line_offset
        s.line_offset = line_offset
        walk(reset_line_offset, x, s)
    else
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

# arg1 op arg2
#
# nest in order of
#
# arg1 op
# arg2
function nest!(x::PTree{T}, s::State; addlen=0) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    # If there's no placeholder the binary call is not nestable
    #= @info "ENTER" typeof(x) s.line_offset x.indent length(x) =#
    idx = findlast(n -> is_placeholder(n), x.nodes) 
    line_length = s.line_offset + length(x) + addlen
    if idx !== nothing && line_length > s.max_line_length
        line_offset = s.line_offset
        # idx op is 1 before the placeholder
        idx -= 1
        lens = remaining_lengths(x.nodes[1:idx])

        x.indent = s.line_offset
        s.line_offset += lens[1]

        x.nodes[idx+1] = newline
        s.line_offset = x.indent

        # arg2
        nest!(x.nodes[end], s)

        # arg1 op
        s.line_offset = line_offset
        nest!(x.nodes[1], s, addlen=lens[2])
        for n in x.nodes[2:idx]
            nest!(n, s)
        end

        s.line_offset = line_offset
        walk(reset_line_offset, x, s)
    else
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
end

