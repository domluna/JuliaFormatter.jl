# Nest
#
# If the line exceeds the print width it will be nested.
# 
# This is done by replacing `PLACEHOLDER` nodes with `NEWLINE` 
# nodes and updating the PTree's indent.
#
# `extra_width` provides additional width to consider from
# the top-level node.
#
# Example:
#
#     arg1 op arg2
#
# the length of " op" will be considered when nesting arg1

function skip_indent(x::PTree)
    if x.typ === CSTParser.LITERAL && x.val == ""
        return true
    elseif x.typ === NEWLINE || x.typ === NOTCODE
        return true
    end
    false
end

function walk(f, x::PTree, s::State)
    f(x, s)
    is_leaf(x) && return
    for (i, n) in enumerate(x.nodes)
        if n.typ === NEWLINE && i < length(x.nodes)
            if is_closer(x.nodes[i+1])
                s.line_offset = x.nodes[i+1].indent
            elseif !skip_indent(x.nodes[i+1])
                s.line_offset = x.indent
            end
        else
            walk(f, n, s)
        end
    end
end

function reset_line_offset!(x::PTree, s::State)
    !is_leaf(x) && return
    s.line_offset += length(x)
end

function add_indent!(x::PTree, s::State, indent)
    indent == 0 && return
    lo = s.line_offset
    f = (x::PTree, s::State) -> x.indent += indent
    walk(f, x, s)
    s.line_offset = lo
end

function nest!(nodes::Vector{PTree}, s::State, indent; extra_width = 0)
    for (i, n) in enumerate(nodes)
        if n.typ === NEWLINE && nodes[i+1].typ === CSTParser.Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NOTCODE && nodes[i+1].typ === CSTParser.Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NEWLINE
            s.line_offset = indent
        else
            nest!(n, s, extra_width = extra_width)
        end
    end
end

function nest!(x::PTree, s::State; extra_width = 0)
    if is_leaf(x)
        s.line_offset += length(x)
        return
    end

    if x.typ === CSTParser.Import
        n_import!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Export
        n_import!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Using
        n_import!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.ImportAll
        n_import!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.WhereOpCall
        n_wherecall!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.ConditionalOpCall
        n_condcall!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.BinaryOpCall
        n_binarycall!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Curly
        n_call!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Call
        n_call!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.MacroCall
        n_call!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Ref
        n_call!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.ChainOpCall
        n_chainopcall!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Comparison
        n_comparison!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.TupleH
        n_tuple!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Vect
        n_tuple!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Vcat
        n_tuple!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Block
        n_block!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.For
        n_for!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Let
        n_for!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.TypedVcat
        n_call!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Parameters
        n_tuple!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Braces
        n_tuple!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.InvisBrackets
        n_tuple!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.UnaryOpCall && x.nodes[2].typ === CSTParser.OPERATOR
        nest!(x.nodes[1], s, extra_width = extra_width + length(x.nodes[2]))
        nest!(x.nodes[2], s)
    elseif x.typ === CSTParser.Do
        n_do!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Generator
        n_gen!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Filter
        n_gen!(x, s, extra_width = extra_width)
    elseif x.typ === CSTParser.Comprehension
        n_tuple!(x, s, extra_width = extra_width)
    else
        nest!(x.nodes, s, x.indent, extra_width = extra_width)
    end
end

function n_do!(x, s; extra_width = 0)
    ew = sum(length.(x.nodes[2:3]))
    # make sure there are nodes after "do"
    if x.nodes[4].typ === WHITESPACE
        ew += length(x.nodes[4])
        ew += length(x.nodes[5])
    end
    nest!(x.nodes[1], s, extra_width = extra_width + ew)
    nest!(x.nodes[2:end], s, x.indent, extra_width = extra_width)
end


# Import,Using,Export,ImportAll
function n_import!(x, s; extra_width = 0)
    line_margin = s.line_offset + length(x) + extra_width
    idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
    if idx !== nothing && (line_margin > s.margin || x.force_nest)
        sidx = findfirst(is_colon, x.nodes)
        if sidx === nothing
            sidx = 2
        else
            # for the WHITESPACE node after the colon
            sidx += 1
        end
        x.indent = s.line_offset + sum(length.(x.nodes[1:sidx]))
        s.line_offset = x.indent

        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x.nodes[i] = Newline()
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    else
        nest!(x.nodes, s, x.indent, extra_width = extra_width)
    end
end

function n_tuple!(x, s; extra_width = 0)
    line_margin = s.line_offset + length(x) + extra_width
    idx = findlast(n -> n.typ === PLACEHOLDER, x.nodes)
    # @info "ENTERING" idx x.typ s.line_offset length(x) extra_width
    opener = length(x.nodes) > 0 ? is_opener(x.nodes[1]) : false
    if idx !== nothing && (line_margin > s.margin || x.force_nest)
        if opener
            x.nodes[end].indent = x.indent
        end
        line_offset = s.line_offset

        x.indent += s.indent_size
        if x.indent - s.line_offset > 1
            x.indent = s.line_offset
            opener && (x.indent += 1)
        end

        # @debug "DURING" x.indent s.line_offset x.typ
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x.nodes[i] = Newline()
                s.line_offset = x.indent
            elseif n.typ === TRAILINGCOMMA
                x.nodes[i].val = ","
                x.nodes[i].len = 1
                nest!(x.nodes[i], s)
            elseif n.typ === TRAILINGSEMICOLON
                x.nodes[i].val = ""
                x.nodes[i].len = 0
                nest!(x.nodes[i], s)
            elseif opener && (i == 1 || i == length(x.nodes))
                nest!(n, s)
            else
                diff = x.indent - x.nodes[i].indent
                add_indent!(n, s, diff)
                nest!(n, s, extra_width = 1)
            end
        end

        if opener
            s.line_offset = x.nodes[end].indent + 1
        end
        # @info "EXITING" x.typ s.line_offset x.indent x.nodes[end].indent
    else
        opener && (extra_width += 1)
        nest!(x.nodes, s, x.indent, extra_width = extra_width)
    end
end

function n_for!(x, s; extra_width = 0)
    block_idx = findfirst(n -> !is_leaf(n), x.nodes)
    if block_idx === nothing
        nest!(x.nodes, s, x.indent, extra_width = extra_width)
        return
    end
    ph_idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes[block_idx].nodes)
    nest!(x.nodes, s, x.indent, extra_width = extra_width)

    # return if the argument block was nested
    ph_idx !== nothing && x.nodes[3].nodes[ph_idx].typ === NEWLINE && return

    idx = 5
    n = x.nodes[idx]
    if n.typ === NOTCODE && n.startline == n.endline
        res = get(s.doc.comments, n.startline, (0, ""))
        res == (0, "") && deleteat!(x.nodes, idx - 1)
    end
end

function n_call!(x, s; extra_width = 0)
    line_margin = s.line_offset + length(x) + extra_width
    idx = findlast(n -> n.typ === PLACEHOLDER, x.nodes)
    # @info "ENTERING" x.typ s.line_offset length(x) extra_width s.margin x.nodes[1].val
    if idx !== nothing && (line_margin > s.margin || x.force_nest)
        x.nodes[end].indent = x.indent
        line_offset = s.line_offset

        caller_len = length(x.nodes[1])

        x.indent += s.indent_size
        # @debug "ENTERING" x.indent s.line_offset x.typ
        if x.indent - s.line_offset > caller_len + 1
            x.indent = s.line_offset + caller_len + 1
            x.nodes[end].indent = s.line_offset
        end

        # @debug "DURING" x.indent s.line_offset x.typ
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x.nodes[i] = Newline()
                s.line_offset = x.indent
            elseif n.typ === TRAILINGCOMMA
                x.nodes[i].val = ","
                x.nodes[i].len = 1
                nest!(x.nodes[i], s)
            elseif n.typ === TRAILINGSEMICOLON
                x.nodes[i].val = ""
                x.nodes[i].len = 0
                nest!(x.nodes[i], s)
            elseif i == 1 || i == length(x.nodes)
                n.typ === CSTParser.Parameters && (n.force_nest = true)
                nest!(n, s, extra_width = 1)
            else
                diff = x.indent - x.nodes[i].indent
                add_indent!(n, s, diff)
                n.typ === CSTParser.Parameters && (n.force_nest = true)
                nest!(n, s, extra_width = 1)
            end
        end

        s.line_offset = x.nodes[end].indent + 1
    else
        is_closer(x.nodes[end]) && (extra_width += 1)
        nest!(x.nodes, s, x.indent, extra_width = extra_width)
    end
end

# "A where B"
function n_wherecall!(x, s; extra_width = 0)
    line_margin = s.line_offset + length(x) + extra_width
    # @debug "" s.line_offset x.typ line_margin extra_width length(x)
    if line_margin > s.margin || x.force_nest
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
        Blen = sum(length.(x.nodes[idx+1:end]))

        # A, +7 is the length of " where "
        ew = Blen + 7 + extra_width
        nest!(x.nodes[1], s, extra_width = ew)

        for (i, n) in enumerate(x.nodes[2:idx-1])
            nest!(n, s)
        end

        # "B"

        has_braces = is_closer(x.nodes[end])
        if has_braces
            x.nodes[end].indent = x.indent
        end

        over = (s.line_offset + Blen + extra_width > s.margin) || x.force_nest
        x.indent += s.indent_size

        last_typ::Union{CSTParser.Head,Nothing} = nothing
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
            elseif n.typ === TRAILINGCOMMA && over
                x.nodes[i+idx].val = ","
                x.nodes[i+idx].len = 1
                nest!(x.nodes[i+idx], s)
            elseif has_braces
                nest!(n, s, extra_width = 1 + extra_width)
            else
                nest!(n, s, extra_width = extra_width)
            end
            (!is_leaf(n) || n.typ === CSTParser.IDENTIFIER) && (last_typ = n.typ)
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, x, s)
    else
        nest!(x.nodes, s, x.indent, extra_width = extra_width)
    end
end

function n_condcall!(x, s; extra_width = 0)
    line_margin = s.line_offset + length(x) + extra_width
    # @info "ENTERING" x.typ s.line_offset line_margin x.force_nest
    if line_margin > s.margin || x.force_nest
        line_offset = s.line_offset
        x.indent = s.line_offset
        phs = reverse(findall(n -> n.typ === PLACEHOLDER, x.nodes))
        for (i, idx) in enumerate(phs)
            if i == 1
                x.nodes[idx] = Newline()
            else
                nidx = phs[i-1]
                l1 = sum(length.(x.nodes[1:idx-1]))
                l2 = sum(length.(x.nodes[idx:nidx-1]))
                width = line_offset + l1 + l2
                if x.force_nest || width > s.margin
                    x.nodes[idx] = Newline()
                end
            end
        end

        for (i, n) in enumerate(x.nodes)
            if i == length(x.nodes)
                nest!(n, s, extra_width = extra_width)
            elseif x.nodes[i+1].typ === WHITESPACE
                ew = length(x.nodes[i+1]) + length(x.nodes[i+2])
                nest!(n, s, extra_width = ew)
            elseif n.typ === NEWLINE
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end

        s.line_offset = line_offset
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE && !is_comment(x.nodes[i+1]) && !is_comment(x.nodes[i-1])
                # +1 for newline to whitespace conversion
                width = s.line_offset + 1
                if i == length(x.nodes) - 1
                    width += sum(length.(x.nodes[i+1:end])) + extra_width
                else
                    width += sum(length.(x.nodes[i+1:i+3]))
                end
                # @debug "" s.line_offset l  s.margin
                if width <= s.margin
                    x.nodes[i] = Whitespace(1)
                else
                    s.line_offset = x.indent
                end
            end
            s.line_offset += length(x.nodes[i])
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, x, s)
    else
        nest!(x.nodes, s, x.indent, extra_width = extra_width)
    end
end
n_comparison!(x, s; extra_width = 0) = n_block!(x, s, extra_width = extra_width)
n_chainopcall!(x, s; extra_width = 0) = n_block!(x, s, extra_width = extra_width)

function dedent!(x::PTree, s::State, pindent::Int, line_margin::Int)
    is_leaf(x) && return
    x.typ === CSTParser.ConditionalOpCall && return
    x.typ === CSTParser.Comparison && return
    x.typ === CSTParser.ChainOpCall && return
    x.typ === CSTParser.BinaryOpCall && return
    if x.typ === CSTParser.InvisBrackets &&
       length(x.nodes) == 3 && is_iterable(x.nodes[2])
        dedent!(x.nodes[2], s, pindent, line_margin)
        return
    end

    if is_iterable(x)
        diff = min(line_margin, pindent + s.indent_size) - x.indent
        # @info "" pindent diff x.indent x.typ
        add_indent!(x, s, diff)
        x.nodes[end].indent = pindent
    else
        add_indent!(x, s, -s.indent_size)
    end
end

# arg1 op arg2
#
# nest in order of
#
# arg1 op
# arg2
function n_binarycall!(x, s; extra_width = 0)
    # If there's no placeholder the binary call is not nestable
    idxs = findall(n -> n.typ === PLACEHOLDER, x.nodes)
    line_margin = s.line_offset + length(x) + extra_width
    # @info "ENTERING" x.typ extra_width s.line_offset length(x) idxs x.ref[][2]
    if length(idxs) == 2 && (line_margin > s.margin || x.force_nest)
        line_offset = s.line_offset
        i1 = idxs[1]
        i2 = idxs[2]
        x.nodes[i1] = Newline()
        cst = x.ref[]

        has_eq = CSTParser.defines_function(cst) || nest_assignment(cst)
        if has_eq
            s.line_offset = x.indent + s.indent_size
            x.nodes[i2] = Whitespace(s.indent_size)
            add_indent!(x.nodes[end], s, s.indent_size)
        else
            x.indent = s.line_offset
        end

        # arg2
        nest!(x.nodes[end], s, extra_width = extra_width)

        # arg1 op
        s.line_offset = line_offset
        # extra_width for " op"
        ew = length(x.nodes[2]) + length(x.nodes[3])
        # @debug "DURING" ew s.line_offset x.typ
        nest!(x.nodes[1], s, extra_width = ew)
        for n in x.nodes[2:i1]
            nest!(n, s)
        end

        # Undo nest if possible
        if !x.force_nest
            arg2 = x.nodes[end]
            arg2.typ === CSTParser.Block && (arg2 = arg2.nodes[1])
            cst = arg2.ref[]

            if (
                arg2.typ === CSTParser.BinaryOpCall && (
                    !(is_lazy_op(cst) && !has_eq) && cst[2].kind !== Tokens.IN
                )
               ) || arg2.typ === CSTParser.UnaryOpCall || is_block(cst)
                line_margin = s.line_offset + 1 + length(x.nodes[end])
            else
                rw, _ = length_to(x, [NEWLINE], start = i2 + 1)
                line_margin = s.line_offset + 1 + rw
            end
            # @info "" can_unnest arg2.typ has_eq

            if line_margin + extra_width <= s.margin
                x.nodes[i1] = Whitespace(1)
                if has_eq
                    x.nodes[i2] = Placeholder(0)
                    # recursive dedent
                    if arg2.typ === CSTParser.BinaryOpCall
                        dedent!(arg2.nodes[1], s, x.indent, line_margin)
                        dedent!(arg2.nodes[end], s, x.indent, line_margin)
                    else
                        dedent!(arg2, s, x.indent, line_margin)
                    end
                end
            end
        end

        # @info "before reset" line_offset s.line_offset

        s.line_offset = line_offset
        walk(reset_line_offset!, x, s)

        # @info "after reset" line_offset s.line_offset
    else
        # Handles the case of a function def defined
        # as "foo(a)::R where {A,B} = body".
        #
        # In this case instead of it being parsed as:
        #
        # CSTParser.BinaryOpCall
        #  - CSTParser.WhereOpCall
        #  - OP
        #  - arg2
        #
        # It's parsed as:
        #
        # CSTParser.BinaryOpCall
        #  - CSTParser.BinaryOpCall
        #   - arg1
        #   - OP
        #   - CSTParser.WhereOpCall
        #    - R
        #    - ...
        #  - OP
        #  - arg2
        #
        # The result being extra width is trickier to deal with.
        idx = findfirst(n -> n.typ === CSTParser.WhereOpCall, x.nodes)
        return_width = 0
        if idx !== nothing && idx > 1
            return_width = length(x.nodes[idx].nodes[1]) + length(x.nodes[2])
        elseif idx === nothing
            return_width, _ = length_to(x, [NEWLINE], start = 2)
        end

        # @info "" return_width
        # @info "" s.line_offset return_width length(x.nodes[1])

        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif i == 1
                nest!(n, s, extra_width = return_width)
            elseif i == idx
                nest!(n, s, extra_width = extra_width)
            else
                nest!(n, s, extra_width = extra_width)
            end
        end
    end
end

function n_block!(x, s; extra_width = 0, custom_indent = 0)
    line_margin = s.line_offset + length(x) + extra_width
    idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
    # @info "ENTERING" idx x.typ s.line_offset length(x) extra_width
    if idx !== nothing && (line_margin > s.margin || x.force_nest)
        line_offset = s.line_offset

        x.indent = custom_indent > 0 ? custom_indent : s.line_offset

        # @info "DURING" x.indent s.line_offset x.typ
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x.nodes[i] = Newline()
                s.line_offset = x.indent
            elseif n.typ === TRAILINGCOMMA
                x.nodes[i].val = ","
                x.nodes[i].len = 1
                nest!(x.nodes[i], s)
            elseif n.typ === TRAILINGSEMICOLON
                x.nodes[i].val = ""
                x.nodes[i].len = 0
                nest!(x.nodes[i], s)
            else
                diff = x.indent - x.nodes[i].indent
                add_indent!(n, s, diff)
                nest!(n, s, extra_width = 1)
            end
        end

        # @info "EXITING" x.typ s.line_offset x.indent x.nodes[end].indent
    else
        nest!(x.nodes, s, x.indent, extra_width = extra_width)
    end
end

n_gen!(x, s; extra_width = 0) =
    n_block!(x, s, extra_width = extra_width, custom_indent = x.indent)
