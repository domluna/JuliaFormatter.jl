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

remaining_length(x::AbstractVector) = reverse(cumsum(reverse(length.(x))))

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
    if s.line_offset + length(x) > s.max_line_length
        idx = findfirst(n -> is_placeholder(n), x.nodes)
        # -3 due to the placeholder being ahead of a comma
        # and another node
        x.indent = s.line_offset + sum(length.(x.nodes[1:idx-3]))
        s.line_offset = x.indent
        line_offset = s.line_offset

        lens = length.(x.nodes[idx-2:end])
        #= @info "" lens s.line_offset line_offset =#
        for (i, n) in enumerate(x.nodes[idx-2:end])
            #= @info "" typeof(n) n s.line_offset =#
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                # Check if the additional length of the nodes 
                # before the next placholder warrant a nest.
                j = i + 1 == length(lens) ? 1 : 2
                if s.line_offset + sum(lens[i:i+j]) > s.max_line_length
                    x.nodes[i+idx-3] = newline
                    s.line_offset = x.indent
                else
                    nest!(n, s)
                end
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

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; addlen=0) where T <: Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.InvisBrackets,CSTParser.Braces,CSTParser.Parameters}
    line_length = s.line_offset + length(x) + addlen
    idx = findlast(n -> is_placeholder(n), x.nodes)
    #= @info "ENTER" typeof(x) s.line_offset line_length =#

    if idx !== nothing && line_length > s.max_line_length
        x.indent = s.line_offset
        lens = length.(x.nodes)
        has_brackets = x.nodes[1] isa PLeaf{CSTParser.PUNCTUATION}
        has_brackets && (x.indent += 1)

        for (i, n) in enumerate(x.nodes)
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n)
                # Check if the additional length of the nodes 
                # before the next placholder warrant a nest.
                j = i + 1 == length(lens) ? 1 : 2
                line_length = s.line_offset + sum(lens[i:i+j])
                i == idx && (line_length += addlen)
                if line_length > s.max_line_length
                    x.nodes[i] = newline
                    s.line_offset = x.indent
                else
                    nest!(n, s)
                end
            else
                addlen0 = !has_brackets && i > idx ? 0 : 1
                nest!(n, s, addlen=addlen0)
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

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State; addlen=0) where T <: Union{CSTParser.Curly,CSTParser.Call}
    line_length = s.line_offset + length(x) + addlen
    idx = findlast(n -> is_placeholder(n), x.nodes)
    #= @info "ENTER" typeof(x) s.line_offset line_length addlen =#
    if idx !== nothing && line_length > s.max_line_length
        x.indent = s.line_offset + length(x.nodes[1]) + length(x.nodes[2])
        s.line_offset = x.indent
        lens = length.(x.nodes[3:end])

        for (i, n) in enumerate(x.nodes[3:end])
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                # Check if the additional length of the nodes 
                # before the next placholder warrant a nest.
                j = i + 1 == length(lens) ? 1 : 2
                line_length = s.line_offset + sum(lens[i:i+j])
                i + 2 == idx && (line_length += addlen)
                if line_length > s.max_line_length
                    x.nodes[i+2] = newline
                    s.line_offset = x.indent
                else
                    nest!(n, s)
                end
            else
                nest!(n, s, addlen=1)
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

function nest!(x::PTree{CSTParser.EXPR{CSTParser.MacroCall}}, s::State; addlen=0)
    line_length = s.line_offset + length(x) + addlen
    idx = findlast(n -> is_placeholder(n), x.nodes)
    #= @info "ENTER" typeof(x) s.line_offset line_length addlen =#
    if idx !== nothing && line_length > s.max_line_length && !(x.nodes[1] isa PTree{CSTParser.EXPR{CSTParser.GlobalRefDoc}})
        x.indent = s.line_offset + length(x.nodes[1]) + length(x.nodes[2])
        s.line_offset = x.indent
        lens = length.(x.nodes[3:end])

        for (i, n) in enumerate(x.nodes[3:end])
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                # Check if the additional length of the nodes 
                # before the next placholder warrant a nest.
                j = i + 1 == length(lens) ? 1 : 2
                line_length = s.line_offset + sum(lens[i:i+j])
                i + 2 == idx && (line_length += addlen)
                if line_length > s.max_line_length
                    x.nodes[i+2] = newline
                    s.line_offset = x.indent
                else
                    nest!(n, s)
                end
            else
                nest!(n, s, addlen=1)
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

# A where B
#
# nest B prior to nesting A
# only nest A if the line still exceeds the maximum length
function nest!(x::PTree{CSTParser.WhereOpCall}, s::State; addlen=0)
    #= @info "ENTER" typeof(x) s.line_offset x =#
    line_length = s.line_offset + length(x) + addlen
    if line_length > s.max_line_length
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> is_placeholder(n), x.nodes)
        A_where_len = sum(length.(x.nodes[1:idx-1]))
        Blens = length.(x.nodes[idx+1:end])

        s.line_offset += A_where_len
        x.indent = s.line_offset
        is_lbrace(x.nodes[idx+1]) && (x.indent += 1)
        ph_offset = 0

        # B
        for (i, n) in enumerate(x.nodes[idx+1:end])
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                ph_offset = max(ph_offset, s.line_offset)
                j = i + 1 == length(Blens) ? 1 : 2
                line_length = s.line_offset + sum(Blens[i:i+j])
                #= line_length = s.line_offset + Blens[i] =#
                i + 2 == length(Blens) && (line_length += addlen)
                if line_length > s.max_line_length
                    x.nodes[i+idx] = newline
                    s.line_offset = x.indent
                end
            else
                nest!(n, s, addlen=1)
            end
        end

        ph_offset = max(ph_offset, s.line_offset)
        addlen = ph_offset - length(x.nodes[1])

        # A where
        s.line_offset = line_offset
        nest!(x.nodes[1], s, addlen=addlen)
        for (i, n) in enumerate(x.nodes[2:idx-1])
            nest!(n, s)
        end

        x.indent = s.line_offset
        # The difference between the unnested pretty length
        # of "A where" and the length of the last line, i.e.:
        #
        # f(a, b) where B
        #
        # vs.
        #
        # f(a,
        #   b) where B
        #
        # would be 3. 
        #
        # All CSTParser.Curly and CSTParser.Braces nodes
        # would have to be detented by 3.
        diff = A_where_len - s.line_offset
        f = (x, s) -> begin
            if x isa PTree{CSTParser.EXPR{CSTParser.Curly}}
                x.indent -= diff + line_offset
            elseif x isa PTree{CSTParser.EXPR{CSTParser.Braces}}
                x.indent -= diff + line_offset
            end
        end

        for (i, n) in enumerate(x.nodes[idx+1:end])
            if is_lbrace(n)
                x.indent += 1
            else
                walk(f, n, s)
            end
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
        Clens = remaining_length(x.nodes[1:idx1])
        E1lens = remaining_length(x.nodes[idx1+1:idx2])

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
        lens = remaining_length(x.nodes[1:idx])

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

