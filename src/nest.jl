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

function walk(f, x::PTree, s::State)
    f(x, s)
    is_leaf(x) && (return)
    for n in x.nodes
        if n.typ === NEWLINE
            s.line_offset = x.indent
        else
            walk(f, n, s)
        end
    end
end


function reset_line_offset!(x::PTree, s::State)
    !is_leaf(x) && (return)
    s.line_offset += length(x)
end


function add_indent!(x, s, indent)
    indent == 0 && (return)
    lo = s.line_offset
    f = (x::PTree, s::State) -> x.indent += indent
    walk(f, x, s)
    s.line_offset = lo
end

function nest!(nodes::Vector{PTree}, s::State, indent; extra_width=0)
    for n in nodes
        if n.typ === NEWLINE
            s.line_offset = indent
        else
            nest!(n, s)
        end
    end
end

function nest!(x::PTree, s::State; extra_width=0)
    if is_leaf(x)
        s.line_offset += length(x)
        return
    end

    if x.typ === CSTParser.Import
        n_import!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.Export
        n_import!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.Using
        n_import!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.ImportAll
        n_import!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.WhereOpCall
        n_wherecall!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.ConditionalOpCall
        n_condcall!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.BinaryOpCall
        n_binarycall!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.Curly
        n_curly!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.Call
        n_call!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.MacroCall
        n_macrocall!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.TupleH
        n_tuple!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.Vect
        n_vect!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.Parameters
        n_params!(x, s, extra_width=extra_width)
    elseif x.typ === CSTParser.Braces
        n_braces!(x, s, extra_width=extra_width)
    else
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE && x.nodes[i+1].typ === CSTParser.Block
                s.line_offset = x.nodes[i+1].indent
            elseif n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === NOTCODE && x.nodes[i+1].typ === CSTParser.Block
                s.line_offset = x.nodes[i+1].indent
            elseif is_leaf(n)
                s.line_offset += length(n)
            else
                nest!(n, s, extra_width=extra_width)
            end
        end
    end
end

# Import,Using,Export,ImportAll
function n_import!(x, s; extra_width=0)
    line_width = s.line_offset + length(x) + extra_width
    idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
    if idx !== nothing && line_width > s.print_width
        # -3 due to the placeholder being ahead of a comma
        # and another node
        x.indent = s.line_offset + sum(length.(x.nodes[1:idx-3]))
        s.line_offset = x.indent

        for (i, n) in enumerate(x.nodes[idx-2:end])
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x.nodes[i+idx-3] = Newline()
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    else
        nest!(x.nodes, s, x.indent)
    end
end

function n_tuple!(x, s; extra_width=0)
    line_width = s.line_offset + length(x) + extra_width
    idx = findlast(n -> n.typ === PLACEHOLDER, x.nodes)
    if idx !== nothing && line_width > s.print_width
        # @info "ENTERING" x.indent s.line_offset x.typ
        has_parens = is_opener(x.nodes[1])
        if has_parens
            x.nodes[end].indent = x.indent
        end
        line_offset = s.line_offset

        x.indent += s.indent_size
        if x.indent - s.line_offset > 1
            x.indent = s.line_offset
            if has_parens
                x.indent += 1
                x.nodes[end].indent = s.line_offset
            end
        end

        # @info "DURING" x.indent s.line_offset x.typ
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x.nodes[i] = Newline()
                s.line_offset = x.indent
            elseif has_parens && (i == 1 || i == length(x.nodes))
                nest!(n, s, extra_width=1)
            else
                diff = x.indent - x.nodes[i].indent
                add_indent!(n, s, diff)
                nest!(n, s, extra_width=1)
            end
        end

        s.line_offset = x.nodes[end].indent
        has_parens && (s.line_offset += 1)
    else
        nest!(x.nodes, s, x.indent)
    end
end

function n_braces!(x, s; extra_width=0)
    n_tuple!(x, s, extra_width=extra_width)
end

function n_vect!(x, s; extra_width=0)
    n_tuple!(x, s, extra_width=extra_width)
end

function n_params!(x, s; extra_width=0)
    n_tuple!(x, s, extra_width=extra_width)
end

# TODO: skip nesting anything in InvisBrackets?
# function n_invisbrackets!(x, s; extra_width=0)
# end

function n_call!(x, s; extra_width=0)
    line_width = s.line_offset + length(x) + extra_width
    idx = findlast(n -> n.typ === PLACEHOLDER, x.nodes)
    if idx !== nothing && line_width > s.print_width
        line_offset = s.line_offset
        x.nodes[end].indent = x.indent
        x.indent += s.indent_size

        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif is_opener(n)
                if x.indent - s.line_offset > 1
                    x.indent = s.line_offset + 1
                    x.nodes[end].indent = line_offset
                end
                nest!(n, s)
            elseif n.typ === PLACEHOLDER
                x.nodes[i] = Newline()
                s.line_offset = x.indent
            else
                nest!(n, s, extra_width=1)
            end
        end

        s.line_offset = x.nodes[end].indent + 1
    else
        nest!(x.nodes, s, x.indent)
    end
end

function n_curly!(x, s; extra_width=0)
    n_call!(x, s, extra_width=extra_width)
end

function n_macrocall!(x, s; extra_width=0)
    n_call!(x, s, extra_width=extra_width)
end

function n_wherecall!(x, s; extra_width=0)
    line_width = s.line_offset + length(x) + extra_width
    if line_width > s.print_width
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
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
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif is_opener(n)
                if x.indent - s.line_offset > 1
                    x.indent = s.line_offset + 1
                    x.nodes[end].indent = s.line_offset
                end
                nest!(n, s)
            elseif n.typ === PLACEHOLDER && over
                x.nodes[i+idx] = Newline()
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
        nest!(x.nodes, s, x.indent)
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
function n_condcall!(x, s; extra_width=0)
    line_width = s.line_offset + length(x) + extra_width
    if line_width > s.print_width
        idx1 = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
        idx2 = findlast(n -> n.typ === PLACEHOLDER, x.nodes)
        Clens = remaining_lengths(x.nodes[1:idx1])
        E1lens = remaining_lengths(x.nodes[idx1+1:idx2])

        line_offset = s.line_offset
        x.indent = s.line_offset

        x.nodes[idx2] = Newline()
        nest!(x.nodes[end], s)

        # E1
        s.line_offset = line_offset + Clens[1]
        if s.line_offset + E1lens[1] > s.print_width
            x.nodes[idx1] = Newline()
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
        walk(reset_line_offset!, x, s)
    else
        nest!(x.nodes, s, x.indent)
    end
end

# arg1 op arg2
#
# nest in order of
#
# arg1 op
# arg2
function n_binarycall!(x, s; extra_width=0)
    # If there's no placeholder the binary call is not nestable
    idx = findlast(n -> n.typ === PLACEHOLDER, x.nodes) 
    line_width = s.line_offset + length(x) + extra_width
    if idx !== nothing && line_width > s.print_width
        @info "START" s.line_offset
        line_offset = s.line_offset
        lens = remaining_lengths(x.nodes[1:idx-1])
        x.nodes[idx-1] = Newline()

        if x.nodes[idx-2].val == "="
            s.line_offset = x.indent + s.indent_size
            x.nodes[idx] = Whitespace(s.indent_size)
        else
            x.indent = s.line_offset
        end

        # arg2
        nest!(x.nodes[end], s)

        # arg1 op
        s.line_offset = line_offset
        @info "" s.line_offset lens[2] x.nodes[idx] x.nodes[2:3]
        # nest!(x.nodes[1], s, extra_width=lens[2])
        # extra_width for the op
        extra_width = length(x.nodes[2]) + length(x.nodes[3])
        nest!(x.nodes[1], s, extra_width=extra_width)
        for n in x.nodes[2:idx-1]
            nest!(n, s)
        end

        @info "before reset" s.line_offset
        walk(reset_line_offset!, x, s)
        @info "after reset" s.line_offset
    else
        nest!(x.nodes, s, x.indent)
    end
end

