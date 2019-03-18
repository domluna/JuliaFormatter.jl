# Nest
#
# If the line exceeds the maximum width it will be nested.
# 
# This is by replacing `Placeholder` nodes with `Newline` nodes
# and updated the PTree's indent.
#
#
# TODO: When nest! recurses into arg1
# it doesn't consider of length of " op"
#
# This would at most be 4, i.e. " ==="
# so not a dealbreaker.
#
# NOTE: this also applies to other nodes
#
#
remaining_length(x::AbstractVector) = reverse(cumsum(reverse(length.(x))))

walk(f, x::AbstractLeaf, s::State) = f(x, s)
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
reset_line_offset(x::AbstractLeaf, s::State) = (s.line_offset += length(x); nothing)

nest!(x::AbstractLeaf, s::State) = (s.line_offset += length(x); nothing)
function nest!(x::PTree, s::State)
    @info "ENTER" typeof(x) s.line_offset
    for (i, n) in enumerate(x.nodes)
        if n === newline
            s.line_offset = x.indent
        else
            nest!(n, s)
        end
    end
    @info "EXIT" typeof(x) s.line_offset
end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.Import,CSTParser.Using,CSTParser.Export}
    @info "ENTER" typeof(x) s.line_offset x
    if s.line_offset + length(x) > s.max_width
        idx = findfirst(n -> is_placeholder(n), x.nodes)
        # -3 due to the placeholder being ahead of a comma
        # and another node
        x.indent = s.line_offset + sum(length.(x.nodes[1:idx-3]))
        s.line_offset = x.indent
        line_offset = s.line_offset

        lens = length.(x.nodes[idx:end])
        #= lens = rem_length(x.nodes[3:end]) =#
        @info "" lens s.line_offset line_offset
        for (i, n) in enumerate(x.nodes[idx:end])
            @info "" typeof(n) n s.line_offset
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) && s.line_offset + lens[i] + lens[i+1] > s.max_width
                x.nodes[i+idx-1] = newline
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
    @info "EXIT" typeof(x) s.line_offset x
end

function nest!(x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.InvisBrackets,CSTParser.Braces,CSTParser.Parameters}
    @info "ENTER" typeof(x) s.line_offset
    if s.line_offset + length(x) > s.max_width
        x.indent = s.line_offset
        x.nodes[1] isa PLeaf{CSTParser.PUNCTUATION} && (x.indent += 1)

        @info "" x.indent

        lens = length.(x.nodes)
        for (i, n) in enumerate(x.nodes)
            @info "" typeof(n) n s.line_offset
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                # Check if the additional length of the nodes 
                # before the next placholder warrant a nest.
                j = i + 1 == length(lens) ? 1 : 2
                if s.line_offset + sum(lens[i:i+j]) > s.max_width
                    x.nodes[i] = newline
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
    @info "EXIT" typeof(x) s.line_offset
end

#= function nest!(x::PTree{CSTParser.EXPR{CSTParser.MacroCall}}, s::State) =#
function nest!(x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.Curly,CSTParser.Call,CSTParser.MacroCall}
    @info "ENTER" typeof(x) s.line_offset x
    if s.line_offset + length(x) > s.max_width && !(x.nodes[1] isa PTree{CSTParser.EXPR{CSTParser.GlobalRefDoc}})
        x.indent = s.line_offset + length(x.nodes[1]) + length(x.nodes[2])
        s.line_offset = x.indent

        lens = length.(x.nodes[3:end])
        #= @info "" lens s.line_offset =#
        for (i, n) in enumerate(x.nodes[3:end])
            @info "" typeof(n) n s.line_offset
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) 
                # Check if the additional length of the nodes 
                # before the next placholder warrant a nest.
                j = i + 1 == length(lens) ? 1 : 2
                if s.line_offset + sum(lens[i:i+j]) > s.max_width
                    x.nodes[i+2] = newline
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
    @info "EXIT" typeof(x) s.line_offset x
end

# A where B
#
# nest B prior to nesting A
# only nest A if the line still exceeds the maximum width
function nest!(x::PTree{CSTParser.WhereOpCall}, s::State)
    @info "ENTER" typeof(x) s.line_offset x
    if s.line_offset + length(x) > s.max_width
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> is_placeholder(n), x.nodes)[1]
        Alens = remaining_length(x.nodes[1:idx-1])
        Blens = remaining_length(x.nodes[idx+1:end])

        s.line_offset += Alens[1]
        x.indent = s.line_offset
        is_lbrace(x.nodes[idx+1]) && (x.indent += 1)

        #= @info "" typeof(x) s.line_offset x length(x.nodes) =#
        @info "" idx Alens Blens length.(x.nodes[1:idx-1])

        # B
        for (i, n) in enumerate(x.nodes[idx+1:end])
            if n === newline
                s.line_offset = x.indent
            elseif is_placeholder(n) && s.line_offset + Blens[i] > s.max_width
                x.nodes[i+idx] = newline
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
        if line_offset + Alens[1] > s.max_width
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
            diff = Alens[1] - s.line_offset
            #= @info "WHERE DIFF" diff Alens[1] s.line_offset =#
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
        end

        s.line_offset = line_offset
        walk(reset_line_offset, x, s)
    else
        for (i, n) in enumerate(x.nodes)
            #= @info "WHEREOP" typeof(n) s.line_offset length(n) =#
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
    @info "EXIT" typeof(x) s.line_offset
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
function nest!(x::PTree{CSTParser.ConditionalOpCall}, s::State)
    @info "ENTER" typeof(x) s.line_offset
    if s.line_offset + length(x) > s.max_width
        idx1 = findfirst(n -> is_placeholder(n), x.nodes)[1]
        idx2 = findlast(n -> is_placeholder(n), x.nodes)[1]
        Clens = remaining_length(x.nodes[1:idx1])
        E1lens = remaining_length(x.nodes[idx1+1:idx2])

        line_offset = s.line_offset
        x.indent = s.line_offset

        x.nodes[idx2] = newline
        nest!(x.nodes[end], s)

        # E1
        s.line_offset = line_offset + Clens[1]
        if s.line_offset + E1lens[1] > s.max_width
            x.nodes[idx1] = newline
            s.line_offset = x.indent
        end

        for n in x.nodes[idx1+1:idx2-1]
            nest!(n, s)
        end

        # C
        s.line_offset = line_offset
        for n in x.nodes[1:idx1-1]
            nest!(n, s)
        end

        # reset line_offset
        s.line_offset = line_offset
        walk(reset_line_offset, x, s)
    else
        for (i, n) in enumerate(x.nodes)
            #= @info "" typeof(n) s.line_offset length(n) =#
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
    @info "EXIT" typeof(x) s.line_offset
end

# arg1 op arg2
#
# nest in order of
#
# arg1 op
# arg2
function nest!(x::PTree{T}, s::State) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    @info "ENTER" typeof(x) s.line_offset x
    # If there's no placeholder the binary call is not nestable
    if s.line_offset + length(x) > s.max_width && findfirst(n -> is_placeholder(n), x.nodes) !== nothing
        line_offset = s.line_offset
        # idx op is 1 before the placeholder
        idx = findfirst(n -> is_placeholder(n), x.nodes)[1] - 1

        lens = remaining_length(x.nodes[1:idx])

        x.indent = s.line_offset
        s.line_offset += lens[1]

        @info "" s.line_offset x.indent

        x.nodes[idx+1] = newline

        if is_placeholder(x.nodes[idx+2])
            @info "BINARY FUNC0"
            x.nodes[idx+2] = Spaces(s.indent_width)
            s.line_offset = x.indent + s.indent_width
            x.indent += s.indent_width
        else
            s.line_offset = x.indent
        end

        # arg2
        nest!(x.nodes[end], s)

        # arg1 op
        s.line_offset = line_offset
        for (i, n) in enumerate(x.nodes[1:idx])
            nest!(n, s)
        end

        s.line_offset = line_offset
        walk(reset_line_offset, x, s)
        #= is_bfunc && (s.line_offset += s.indent_width) =#
    else
        for (i, n) in enumerate(x.nodes)
            #= @info "" typeof(n) s.line_offset length(n) =#
            if n === newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
    @info "EXIT" typeof(x) s.line_offset
end

