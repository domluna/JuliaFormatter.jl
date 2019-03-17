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
# NOTE: this also applies to WhereOpCall nodes
#

#= nest!(::PLeaf, ::State) = nothing =#

rem_length(x::AbstractVector) = reverse(cumsum(reverse(length.(x))))

walk(f, x::PLeaf, s::State) = nothing
function walk(f, x::PTree, s::State)
    #= @info "" typeof(x) s.line_offset =#
    for n in x.nodes
        if n === Newline
            s.line_offset = x.indent
        else
            f(n, s)
            walk(f, n, s)
        end
    end
end

# Used to correctly reset the State's line_offset. 
reset_line_offset(_, _) = nothing
reset_line_offset(x::PLeaf, s::State) = (s.line_offset += length(x); nothing)

nest!(x::PLeaf, s::State) = (s.line_offset += length(x); nothing)
function nest!(x::PTree, s::State)
    #= @info "" typeof(x) s.line_offset =#
    for (i, n) in enumerate(x.nodes)
        if n === Newline
            s.line_offset = x.indent
        else
            nest!(n, s)
        end
    end
end
 
function nest!(x::PTree{CSTParser.EXPR{T}}, s::State) where T <: Union{CSTParser.Curly,CSTParser.Call}
    #= @info "" typeof(x) s.line_offset x length(x) =#
    if s.line_offset + length(x) > s.max_width
        x.indent = s.line_offset + length(x.nodes[1]) + length(x.nodes[2])
        s.line_offset = x.indent

        lens = rem_length(x.nodes[3:end])
        #= @info "" lens s.line_offset =#
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
    #= @info "" typeof(x) s.line_offset length(x) =#
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
    #= @info "" typeof(x) s.line_offset length(x) =#
    if s.line_offset + length(x) > s.max_width
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> is_placeholder(n), x.nodes)[1]
        Alens = rem_length(x.nodes[1:idx-1])
        Blens = rem_length(x.nodes[idx+1:end])

        s.line_offset += Alens[1]
        x.indent = s.line_offset
        is_lbrace(x.nodes[idx+1]) && (x.indent += 1)

        #= @info "" typeof(x) s.line_offset x length(x.nodes) =#
        #= @info "" idx Alens Blens =#

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
            f = (x, s) -> begin
                if x isa PTree{CSTParser.EXPR{CSTParser.Curly}}
                    x.indent -= diff
                elseif x isa PTree{CSTParser.EXPR{CSTParser.Braces}}
                    x.indent -= diff
                end
            end
            for (i, n) in enumerate(x.nodes[idx+1:end])
                if is_lbrace(n)
                    x.indent += 1
                else
                    f(n, s)
                    walk(f, n, s)
                end
            end
        end

        s.line_offset = line_offset
        walk(reset_line_offset, x, s)
    else
        for (i, n) in enumerate(x.nodes)
            #= @info "WHEREOP" typeof(n) s.line_offset length(n) =#
            if n === Newline
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
# fold in order of
#
# C ? E1 :
# E2
#
# C ?
# E1 :
# E2
function nest!(x::PTree{CSTParser.ConditionalOpCall}, s::State)
    #= @info "ENTER" typeof(x) s.line_offset x =#
    if s.line_offset + length(x) > s.max_width
        idx1 = findfirst(n -> is_placeholder(n), x.nodes)[1]
        idx2 = findlast(n -> is_placeholder(n), x.nodes)[1]
        Clens = rem_length(x.nodes[1:idx1])
        E1lens = rem_length(x.nodes[idx1+1:idx2])
        E2lens = rem_length(x.nodes[idx2+1:end])

        line_offset = s.line_offset
        x.indent = s.line_offset
        s.line_offset = Clens[1] + E1lens[1]

        # E2
        if s.line_offset + E2lens[1] > s.max_width
            x.nodes[idx2] = Newline
            s.line_offset = x.indent
        end
        nest!(x.nodes[end], s)

        # E1
        s.line_offset = line_offset + Clens[1]
        if s.line_offset + E1lens[1] > s.max_width
            x.nodes[idx1] = Newline
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
            if n === Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
    #= @info "EXIT" typeof(x) s.line_offset =#
end

# arg1 op arg2
#
function nest!(x::PTree{T}, s::State) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    #= @info "ENTER" typeof(x) s.line_offset x length(x) =#
    if s.line_offset + length(x) > s.max_width && findfirst(n -> is_placeholder(n), x.nodes) !== nothing
        line_offset = s.line_offset
        # idx op is 1 before the placeholder
        idx = findfirst(n -> is_placeholder(n), x.nodes)[1] - 1

        arg1lens = rem_length(x.nodes[1:idx])
        arg2lens = rem_length(x.nodes[idx+1:end])

        x.indent = s.line_offset
        s.line_offset += arg1lens[1]

        #= @info "" typeof(x) s.line_offset x arg2lens[1] =#
        #= @info "" arg1lens arg2lens =#

        if s.line_offset + arg2lens[1] > s.max_width
            x.nodes[idx+1] = Newline
            s.line_offset = x.indent
        end
        # arg2
        #= @info "nesting arg2" =#
        nest!(x.nodes[end], s)

        # arg1 op
        s.line_offset = line_offset
        #= @info "nesting arg1" =#
        for (i, n) in enumerate(x.nodes[1:idx])
            nest!(n, s)
        end

        s.line_offset = line_offset
        walk(reset_line_offset, x, s)
    else
        for (i, n) in enumerate(x.nodes)
            #= @info "" typeof(n) s.line_offset length(n) =#
            if n === Newline
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    end
    #= @info "EXIT" typeof(x) s.line_offset =#
end
