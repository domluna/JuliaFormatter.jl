# Nest
#
# If the line exceeds the print width it will be nested.
# 
# This is done by replacing `Placeholder` nodes with `Newline` 
# nodes and updating the PTree's indent.
#
# `extra_width` provides additional width to consider from
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

nest!(x::AbstractPLeaf, s::State; extra_width=0) = (s.line_offset += length(x); nothing)
function nest!(x::PTree, s::State; extra_width=0)
    for (i, n) in enumerate(x.nodes)
        if n === newline && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            # correct the offset
            s.line_offset = x.nodes[i+1].indent
        elseif n === newline
            s.line_offset = x.indent
        elseif n isa NotCode && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            s.line_offset = x.nodes[i+1].indent
        else
            nest!(n, s, extra_width=extra_width)
        end
    end
end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; extra_width=0) where T <: Union{CSTParser.Import,CSTParser.Using,CSTParser.Export}
    line_width = s.line_offset + length(x) + extra_width
    idx = findfirst(n -> is_placeholder(n), x.nodes)
    if idx !== nothing && line_width > s.print_width
        # -3 due to the placeholder being ahead of a comma
        # and another node
        x.indent = s.line_offset + sum(length.(x.nodes[1:idx-3]))
        s.line_offset = x.indent

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
end

# TODO: skip nesting anything in InvisBrackets?
# function nest!(x::PTree{CSTParser.EXPR{CSTParser.InvisBrackets}}, s::State; extra_width=0)
# end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; extra_width=0) where T <: Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.Braces,CSTParser.Parameters,CSTParser.InvisBrackets}
    line_width = s.line_offset + length(x) + extra_width
    idx = findlast(n -> is_placeholder(n), x.nodes)
    if idx !== nothing && line_width > s.print_width
        @info "" x.indent length(x.nodes) typeof(x)
        if is_closer(x.nodes[end])
            x.nodes[end].indent = x.indent
        end

        line_offset = s.line_offset
        x.indent += s.indent_size

        @info "" x.indent s.line_offset

        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            elseif i == 1 && is_opener(n)
                if x.indent - s.line_offset > 1
                    x.indent = s.line_offset + 1
                    x.nodes[end].indent = s.line_offset
                end
                nest!(n, s)
            elseif i == 1
                if x.indent - s.line_offset > 1
                    x.indent = s.line_offset
                end
                nest!(n, s)
            elseif is_placeholder(n) 
                x.nodes[i] = newline
                s.line_offset = x.indent
            else
                nest!(n, s, extra_width=1)
            end
        end

        s.line_offset = x.nodes[end].indent
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

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; extra_width=0) where T <: Union{CSTParser.Curly,CSTParser.Call,CSTParser.MacroCall}
    line_width = s.line_offset + length(x) + extra_width
    idx = findlast(n -> is_placeholder(n), x.nodes)
    if idx !== nothing && line_width > s.print_width
        line_offset = s.line_offset
        x.nodes[end].indent = x.indent
        x.indent += s.indent_size

        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            elseif is_opener(n)
                if x.indent - s.line_offset > 1
                    x.indent = s.line_offset + 1
                    x.nodes[end].indent = line_offset
                end
                nest!(n, s)
            elseif is_placeholder(n) 
                x.nodes[i] = newline
                s.line_offset = x.indent
            else
                nest!(n, s, extra_width=1)
            end
        end

        s.line_offset = x.nodes[end].indent + 1
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

function nest!(x::PTree{CSTParser.WhereOpCall}, s::State; extra_width=0)
    line_width = s.line_offset + length(x) + extra_width
    if line_width > s.print_width
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> is_placeholder(n), x.nodes)
        Blens = remaining_lengths(x.nodes[idx+1:end])

        # A, +7 is the length of " where "
        nest!(x.nodes[1], s, extra_width=Blens[1] + 7)

        for (i, n) in enumerate(x.nodes[2:idx-1])
            nest!(n, s)
        end
        
        # where B
        over = s.line_offset + Blens[1] > s.print_width

        has_brace = false
        if is_closer(x.nodes[end]) 
            x.nodes[end].indent = x.indent
            has_brace = true
        end

        line_offset = s.line_offset
        x.indent += s.indent_size

        @info "" s.line_offset Blens[1] x.indent
        for (i, n) in enumerate(x.nodes[idx+1:end])
            if n === newline
                s.line_offset = x.indent
            elseif is_opener(n)
                if x.indent - s.line_offset > 1
                    x.indent = s.line_offset + 1
                    x.nodes[end].indent = s.line_offset
                end
                nest!(n, s)
            elseif is_placeholder(n) && over
                x.nodes[i+idx] = newline
                s.line_offset = x.indent
            elseif has_brace
                nest!(n, s, extra_width=1)
            else
                nest!(n, s, extra_width=0)
            end
        end

        if over && is_closer(x.nodes[end])
            s.line_offset = x.nodes[end].indent + 1
        end

        @info "" s.line_offset
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
function nest!(x::PTree{CSTParser.ConditionalOpCall}, s::State; extra_width=0)
    line_width = s.line_offset + length(x) + extra_width
    if line_width > s.print_width
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
        if s.line_offset + E1lens[1] > s.print_width
            x.nodes[idx1] = newline
            s.line_offset = x.indent
        end
        nest!(x.nodes[idx1+1], s, extra_width=2)
        for n in x.nodes[idx1+2:idx2-1]
            nest!(n, s)
        end

        # C
        s.line_offset = line_offset
        nest!(x.nodes[1], s, extra_width=2)
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
function nest!(x::PTree{T}, s::State; extra_width=0) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    # If there's no placeholder the binary call is not nestable
    idx = findlast(n -> is_placeholder(n), x.nodes) 
    line_width = s.line_offset + length(x) + extra_width
    if idx !== nothing && line_width > s.print_width
        @info "START" s.line_offset
        line_offset = s.line_offset
        lens = remaining_lengths(x.nodes[1:idx-1])
        x.nodes[idx] = newline

        if x.nodes[idx-1].text == "="
            s.line_offset = x.indent + s.indent_size
        else
            x.indent = s.line_offset
        end

        # arg2
        nest!(x.nodes[end], s)

        # arg1 op
        s.line_offset = line_offset
        nest!(x.nodes[1], s, extra_width=lens[2])
        for n in x.nodes[2:idx-1]
            nest!(n, s)
        end

        @info "" s.line_offset
        walk(reset_line_offset, x, s)
        @info "" s.line_offset
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

