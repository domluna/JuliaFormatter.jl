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

function nest!(x::PTree{CSTParser.EXPR{CSTParser.If}}, s::State; extra_width=0)
    n1 = x.nodes[1]
    for (i, n) in enumerate(x.nodes)
        if n === newline && x.nodes[i+1] isa PTree{CSTParser.EXPR{CSTParser.Block}}
            s.line_offset = x.nodes[i+1].indent
        elseif n === newline && x.nodes[i+1] isa PLeaf{CSTParser.KEYWORD}
            v = x.nodes[i+1].text
            if n1 isa PLeaf{CSTParser.KEYWORD} && n1.text == "if " 
                s.line_offset = x.indent
            elseif v == "elseif" || v == "else"
                s.line_offset = x.indent - s.indent_size
            else
                s.line_offset = x.indent
            end
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
end

# TODO: skip nesting anything in InvisBrackets?
# function nest!(x::PTree{CSTParser.EXPR{CSTParser.InvisBrackets}}, s::State; extra_width=0)
# end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; extra_width=0) where T <: Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.Braces,CSTParser.Parameters}
    line_width = s.line_offset + length(x) + extra_width
    idx = findlast(n -> is_placeholder(n), x.nodes)
    if idx !== nothing && line_width > s.print_width
        line_offset = s.line_offset
        x.indent = is_opener(x.nodes[1]) ? s.line_offset + 1 : s.line_offset
        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                x.nodes[i] = newline
                s.line_offset = x.indent
            else
                nest!(n, s, extra_width=1)
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

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; extra_width=0) where T <: Union{CSTParser.Curly,CSTParser.Call}
    line_width = s.line_offset + length(x) + extra_width
    idx = findlast(n -> is_placeholder(n), x.nodes)
    if idx !== nothing && line_width > s.print_width
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
                nest!(n, s, extra_width=1)
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

function nest!(x::PTree{CSTParser.EXPR{CSTParser.MacroCall}}, s::State; extra_width=0)
    line_width = s.line_offset + length(x) + extra_width
    idx = findlast(n -> is_placeholder(n), x.nodes)
    if idx !== nothing && line_width > s.print_width && !(x.nodes[1] isa PTree{CSTParser.EXPR{CSTParser.GlobalRefDoc}})
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
                nest!(n, s, extra_width=1)
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

        x.indent = s.line_offset
        has_brace = is_opener(x.nodes[idx+1])
        has_brace && (x.indent += 1)
        # where B
        over = s.line_offset + Blens[1] > s.print_width
        for (i, n) in enumerate(x.nodes[idx+1:end])
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) && over
                x.nodes[i+idx] = newline
                s.line_offset = x.indent
            elseif has_brace
                nest!(n, s, extra_width=1)
            else
                nest!(n, s, extra_width=0)
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
        line_offset = s.line_offset
        lens = remaining_lengths(x.nodes[1:idx-1])

        x.indent = s.line_offset
        s.line_offset += lens[1]

        x.nodes[idx] = newline
        # this will only be true if it's a function definition
        if x.nodes[idx-1].text == "="
            s.line_offset = x.indent + s.indent_size
            # insert additional whitespace nodes
            for _ in 1:s.indent_size
                insert!(x.nodes, idx+1, whitespace)
            end
        else
            s.line_offset = x.indent
        end

        # arg2
        nest!(x.nodes[end], s)

        # arg1 op
        s.line_offset = line_offset
        nest!(x.nodes[1], s, extra_width=lens[2])
        for n in x.nodes[2:idx-1]
            nest!(n, s)
        end

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

