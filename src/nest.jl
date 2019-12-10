# Nest
#
# If the line exceeds the print width it will be nested.
#
# This is done by replacing `PLACEHOLDER` nodes with `NEWLINE`
# nodes and updating the FST's indent.
#
# `extra_margin` provides additional width to consider from
# the top-level node.
#
# Example:
#
#     arg1 op arg2
#
# the length of " op" will be considered when nesting arg1

# unnest, converts newlines to whitespace
unnest!(fst::FST, ind::Int) = fst[ind] = Whitespace(fst[ind].len)
function unnest!(fst::FST, nl_inds::Vector{Int})
    for (i, ind) in enumerate(nl_inds)
        unnest!(fst, ind)
        i == length(nl_inds) || continue
        ind2 = ind - 1
        if fst[ind2].typ === TRAILINGCOMMA
            fst[ind2].val = ""
            fst[ind2].len = 0
        elseif fst[ind-1].typ === TRAILINGSEMICOLON
            fst[ind2].val = ";"
            fst[ind2].len = 1
        end
    end
end

function skip_indent(fst::FST)
    if fst.typ === CSTParser.LITERAL && fst.val == ""
        return true
    elseif fst.typ === NEWLINE || fst.typ === NOTCODE
        return true
    end
    false
end

function walk(f, fst::FST, s::State)
    f(fst, s)
    is_leaf(fst) && return
    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE && i < length(fst.nodes)
            if is_closer(fst[i+1])
                s.line_offset = fst[i+1].indent
            elseif !skip_indent(fst[i+1])
                s.line_offset = fst.indent
            end
        else
            walk(f, n, s)
        end
    end
end

function reset_line_offset!(fst::FST, s::State)
    is_leaf(fst) || return
    s.line_offset += length(fst)
end

function add_indent!(fst::FST, s::State, indent)
    indent == 0 && return
    lo = s.line_offset
    f = (fst::FST, s::State) -> fst.indent += indent
    walk(f, fst, s)
    s.line_offset = lo
end

function dedent!(fst::FST, s::State)
    if is_leaf(fst)
        s.line_offset += length(fst)
        is_closer(fst) && (fst.indent -= s.indent_size)
        return
    end
    fst.typ === CSTParser.ConditionalOpCall && return
    fst.typ === CSTParser.StringH && return

    # dedent
    fst.indent -= s.indent_size
    fst.force_nest && return

    nl_inds = findall(n -> n.typ === NEWLINE && !n.force_nest, fst.nodes)
    length(nl_inds) > 0 || return
    margin = s.line_offset + fst.extra_margin + length(fst)
    # @info "" fst.typ margin s.line_offset fst.extra_margin length(fst) length(nl_inds)
    margin <= s.margin || return
    unnest!(fst, nl_inds)
end

function nest!(nodes::Vector{FST}, s::State, indent::Int; extra_margin = 0)
    for (i, n) in enumerate(nodes)
        if n.typ === NEWLINE && nodes[i+1].typ === CSTParser.Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NOTCODE && nodes[i+1].typ === CSTParser.Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NEWLINE
            s.line_offset = indent
        else
            n.extra_margin = extra_margin
            nest!(n, s)
        end
    end
end

function nest!(fst::FST, s::State)
    if is_leaf(fst)
        s.line_offset += length(fst)
        return
    end

    if fst.typ === CSTParser.Import
        n_import!(fst, s)
    elseif fst.typ === CSTParser.Export
        n_export!(fst, s)
    elseif fst.typ === CSTParser.Using
        n_using!(fst, s)
    elseif fst.typ === CSTParser.WhereOpCall
        n_whereopcall!(fst, s)
    elseif fst.typ === CSTParser.ConditionalOpCall
        n_conditionalopcall!(fst, s)
    elseif fst.typ === CSTParser.BinaryOpCall
        n_binaryopcall!(fst, s)
    elseif fst.typ === CSTParser.Curly
        n_curly!(fst, s)
    elseif fst.typ === CSTParser.Call
        n_call!(fst, s)
    elseif fst.typ === CSTParser.MacroCall
        n_macrocall!(fst, s)
    elseif fst.typ === CSTParser.Ref
        n_ref!(fst, s)
    elseif fst.typ === CSTParser.TypedVcat
        n_typedvcat!(fst, s)
    elseif fst.typ === CSTParser.TupleH
        n_tupleh!(fst, s)
    elseif fst.typ === CSTParser.Vect
        n_vect!(fst, s)
    elseif fst.typ === CSTParser.Vcat
        n_vcat!(fst, s)
    elseif fst.typ === CSTParser.Parameters
        n_parameters!(fst, s)
    elseif fst.typ === CSTParser.Braces
        n_braces!(fst, s)
    elseif fst.typ === CSTParser.InvisBrackets
        n_invisbrackets!(fst, s)
    elseif fst.typ === CSTParser.Comprehension
        n_comphrehension!(fst, s)
    elseif fst.typ === CSTParser.Do
        n_do!(fst, s)
    elseif fst.typ === CSTParser.Generator
        n_generator!(fst, s)
    elseif fst.typ === CSTParser.Filter
        n_filter!(fst, s)
    elseif fst.typ === CSTParser.Block
        n_block!(fst, s)
    elseif fst.typ === CSTParser.ChainOpCall
        n_chainopcall!(fst, s)
    elseif fst.typ === CSTParser.Comparison
        n_comparison!(fst, s)
    elseif fst.typ === CSTParser.For
        n_for!(fst, s)
    elseif fst.typ === CSTParser.Let
        n_let!(fst, s)
    elseif fst.typ === CSTParser.UnaryOpCall && fst[2].typ === CSTParser.OPERATOR
        n_unaryopcall!(fst, s)
    else
        nest!(fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end

function n_unaryopcall!(fst::FST, s::State)
    fst[1].extra_margin = fst.extra_margin + length(fst[2])
    nest!(fst[1], s)
    nest!(fst[2], s)
end

function n_do!(fst::FST, s::State)
    extra_margin = sum(length.(fst[2:3]))
    # make sure there are nodes after "do"
    if fst[4].typ === WHITESPACE
        extra_margin += length(fst[4])
        extra_margin += length(fst[5])
    end
    fst[1].extra_margin = fst.extra_margin + extra_margin
    nest!(fst[1], s)
    nest!(fst[2:end], s, fst.indent, extra_margin = fst.extra_margin)
end


# Import,Using,Export
function n_import!(fst::FST, s::State)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        fst.indent += s.indent_size

        if !fst.force_nest
            if fst.indent + sum(length.(fst[idx+1:end])) <= s.margin
                fst[idx] = Newline(length = fst[idx].len)
                walk(reset_line_offset!, fst, s)
                return
            end
        end

        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            else
                nest!(n, s)
            end
        end
    else
        nest!(fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
n_export!(fst::FST, s::State) = n_import!(fst, s)
n_using!(fst::FST, s::State) = n_import!(fst, s)
n_importall!(fst::FST, s::State) = n_import!(fst, s)

function n_tupleh!(fst::FST, s::State)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes)
    # @info "ENTERING" idx fst.typ s.line_offset length(fst) fst.extra_margin
    opener = length(fst.nodes) > 0 ? is_opener(fst[1]) : false
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        if opener
            fst[end].indent = fst.indent
        end
        if fst.typ !== CSTParser.Parameters && !(fst.typ === CSTParser.TupleH && !opener)
            fst.indent += s.indent_size
        end

        # @info "DURING" fst.typ fst.indent s.line_offset
        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(n, s)
            elseif opener && (i == 1 || i == length(fst.nodes))
                nest!(n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(n, s)
            end
        end

        if opener
            s.line_offset = fst[end].indent + 1
        end
        # @info "EXITING" fst.typ s.line_offset fst.indent fst[end].indent
    else
        extra_margin = fst.extra_margin
        opener && (extra_margin += 1)
        nest!(fst.nodes, s, fst.indent, extra_margin = extra_margin)
    end
end
n_vect!(fst::FST, s::State) = n_tupleh!(fst, s)
n_vcat!(fst::FST, s::State) = n_tupleh!(fst, s)
n_braces!(fst::FST, s::State) = n_tupleh!(fst, s)
n_parameters!(fst::FST, s::State) = n_tupleh!(fst, s)
n_invisbrackets!(fst::FST, s::State) = n_tupleh!(fst, s)
n_comphrehension!(fst::FST, s::State) = n_tupleh!(fst, s)


function n_call!(fst::FST, s::State)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes)
    # @info "ENTERING" fst.typ s.line_offset length(fst) fst.extra_margin s.margin fst[1].val
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        fst[end].indent = fst.indent
        fst.indent += s.indent_size

        # @info "DURING" fst.typ fst.indent s.line_offset
        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(n, s)
            elseif i == 1 || i == length(fst.nodes)
                n.typ === CSTParser.Parameters && (n.force_nest = true)
                n.extra_margin = 1
                nest!(n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.typ === CSTParser.Parameters && (n.force_nest = true)
                n.extra_margin = 1
                nest!(n, s)
            end
        end

        s.line_offset = fst[end].indent + 1
    else
        extra_margin = fst.extra_margin
        is_closer(fst[end]) && (extra_margin += 1)
        nest!(fst.nodes, s, fst.indent, extra_margin = extra_margin)
    end
end
n_curly!(fst::FST, s::State) = n_call!(fst, s)
n_macrocall!(fst::FST, s::State) = n_call!(fst, s)
n_ref!(fst::FST, s::State) = n_call!(fst, s)
n_typedvcat!(fst::FST, s::State) = n_call!(fst, s)

function n_whereopcall!(fst::FST, s::State)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    # @info "" s.line_offset fst.typ line_margin fst.extra_margin length(fst) fst.force_nest line_margin > s.margin
    if line_margin > s.margin || fst.force_nest
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
        Blen = sum(length.(fst[idx+1:end]))

        # A, +7 is the length of " where "
        fst[1].extra_margin = Blen + 7 + fst.extra_margin
        nest!(fst[1], s)

        for (i, n) in enumerate(fst[2:idx-1])
            nest!(n, s)
        end

        # "B"

        has_braces = is_closer(fst[end])
        if has_braces
            fst[end].indent = fst.indent
        end

        over = (s.line_offset + Blen + fst.extra_margin > s.margin) || fst.force_nest
        fst.indent += s.indent_size

        for (i, n) in enumerate(fst[idx+1:end])
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif is_opener(n)
                if fst.indent - s.line_offset > 1
                    fst.indent = s.line_offset + 1
                    fst[end].indent = s.line_offset
                end
                nest!(n, s)
            elseif n.typ === PLACEHOLDER && over
                fst[i+idx] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA && over
                n.val = ","
                n.len = 1
                nest!(n, s)
            elseif has_braces
                n.extra_margin = 1 + fst.extra_margin
                nest!(n, s)
            else
                n.extra_margin = fst.extra_margin
                nest!(n, s)
            end
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, fst, s)
    else
        nest!(fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end

function n_conditionalopcall!(fst::FST, s::State)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    # @info "ENTERING" fst.typ s.line_offset line_margin fst.force_nest
    if line_margin > s.margin || fst.force_nest
        line_offset = s.line_offset
        fst.indent = s.line_offset
        phs = reverse(findall(n -> n.typ === PLACEHOLDER, fst.nodes))
        for (i, idx) in enumerate(phs)
            if i == 1
                fst[idx] = Newline(length = fst[idx].len)
            else
                nidx = phs[i-1]
                l1 = sum(length.(fst[1:idx-1]))
                l2 = sum(length.(fst[idx:nidx-1]))
                width = line_offset + l1 + l2
                if fst.force_nest || width > s.margin
                    fst[idx] = Newline(length = fst[idx].len)
                end
            end
        end

        for (i, n) in enumerate(fst.nodes)
            if i == length(fst.nodes)
                n.extra_margin = fst.extra_margin
                nest!(n, s)
            elseif fst[i+1].typ === WHITESPACE
                n.extra_margin = length(fst[i+1]) + length(fst[i+2])
                nest!(n, s)
            elseif n.typ === NEWLINE
                s.line_offset = fst.indent
            else
                nest!(n, s)
            end
        end

        s.line_offset = line_offset
        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE && !is_comment(fst[i+1]) && !is_comment(fst[i-1])
                # +1 for newline to whitespace conversion
                width = s.line_offset + 1
                if i == length(fst.nodes) - 1
                    width += sum(length.(fst[i+1:end])) + fst.extra_margin
                else
                    width += sum(length.(fst[i+1:i+3]))
                end
                # @debug "" s.line_offset l  s.margin
                if width <= s.margin
                    fst[i] = Whitespace(1)
                else
                    s.line_offset = fst.indent
                end
            end
            s.line_offset += length(fst[i])
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, fst, s)
    else
        nest!(fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end



# arg1 op arg2
#
# nest in order of
#
# arg1 op
# arg2
function n_binaryopcall!(fst::FST, s::State)
    # If there's no placeholder the binary call is not nestable
    idxs = findall(n -> n.typ === PLACEHOLDER, fst.nodes)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    # @info "ENTERING" fst.typ fst.extra_margin s.line_offset length(fst) idxs fst.ref[][2]
    if length(idxs) == 2 && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        i1 = idxs[1]
        i2 = idxs[2]
        fst[i1] = Newline(length = fst[i1].len)
        cst = fst.ref[]

        has_eq = CSTParser.defines_function(cst) || nest_assignment(cst)
        has_arrow = cst[2].kind === Tokens.PAIR_ARROW || cst[2].kind === Tokens.ANON_FUNC
        indent_nest = has_eq || has_arrow

        if indent_nest
            s.line_offset = fst.indent + s.indent_size
            fst[i2] = Whitespace(s.indent_size)
            add_indent!(fst[end], s, s.indent_size)
        else
            fst.indent = s.line_offset
        end

        # arg2
        fst[end].extra_margin = fst.extra_margin
        nest!(fst[end], s)

        # "arg1 op" arg2
        s.line_offset = line_offset

        # extra margin for " op"
        fst[1].extra_margin = length(fst[2]) + length(fst[3])
        nest!(fst[1], s)
        for n in fst[2:i1]
            nest!(n, s)
        end

        # Undo nest if possible
        if !fst.force_nest
            arg2 = fst[end]
            arg2.typ === CSTParser.Block && (arg2 = arg2[1])
            cst = arg2.ref[]

            line_margin = s.line_offset
            if (
                arg2.typ === CSTParser.BinaryOpCall &&
                (!(is_lazy_op(cst) && !indent_nest) && cst[2].kind !== Tokens.IN)
            ) || arg2.typ === CSTParser.UnaryOpCall ||
               arg2.typ === CSTParser.ChainOpCall || arg2.typ === CSTParser.Comparison
                line_margin += length(fst[end])
            elseif arg2.typ === CSTParser.Do && is_iterable(arg2[1])
                rw, _ = length_to(fst, [NEWLINE], start = i2 + 1)
                line_margin += rw
            elseif is_block(cst)
                idx = findfirst(n -> n.typ === NEWLINE, arg2.nodes)
                if idx === nothing
                    line_margin += length(fst[end])
                else
                    line_margin += sum(length.(arg2[1:idx-1]))
                end
            else
                rw, _ = length_to(fst, [NEWLINE], start = i2 + 1)
                line_margin += rw
            end

            # @info "" arg2.typ indent_nest s.line_offset line_margin fst.extra_margin length(fst[end])
            if line_margin + fst.extra_margin <= s.margin
                fst[i1] = Whitespace(1)
                if indent_nest
                    fst[i2] = Whitespace(0)
                    walk(dedent!, arg2, s)
                end
            end
        end

        # @info "before reset" line_offset s.line_offset
        s.line_offset = line_offset
        walk(reset_line_offset!, fst, s)
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
        idx = findfirst(n -> n.typ === CSTParser.WhereOpCall, fst.nodes)
        return_width = 0
        if idx !== nothing && idx > 1
            return_width = length(fst[idx]) + length(fst[2])
        elseif idx === nothing
            return_width, _ = length_to(fst, [NEWLINE], start = 2)
        end

        # @info "" idx map(n -> n.typ, fst.nodes)
        # @info "" s.line_offset length(fst[1]) return_width fst.extra_margin

        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif i == 1
                n.extra_margin = return_width + fst.extra_margin
                nest!(n, s)
            elseif i == idx
                n.extra_margin = fst.extra_margin
                nest!(n, s)
            else
                n.extra_margin = fst.extra_margin
                nest!(n, s)
            end
        end
    end
end

function n_for!(fst::FST, s::State)
    block_idx = findfirst(n -> !is_leaf(n), fst.nodes)
    if block_idx === nothing
        nest!(fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
        return
    end
    ph_idx = findfirst(n -> n.typ === PLACEHOLDER, fst[block_idx].nodes)
    nest!(fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)

    # return if the argument block was nested
    ph_idx !== nothing && fst[3][ph_idx].typ === NEWLINE && return

    idx = 5
    n = fst[idx]
    if n.typ === NOTCODE && n.startline == n.endline
        res = get(s.doc.comments, n.startline, (0, ""))
        res == (0, "") && deleteat!(fst.nodes, idx - 1)
    end
end
n_let!(fst::FST, s::State) = n_for!(fst, s)

function n_block!(fst::FST, s::State; custom_indent = 0)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
    # @info "ENTERING" idx fst.typ s.line_offset length(fst) fst.extra_margin
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset

        fst.indent = custom_indent > 0 ? custom_indent : s.line_offset

        # @info "DURING" fst.indent s.line_offset fst.typ
        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(n, s)
            end
        end

        # @info "EXITING" fst.typ s.line_offset fst.indent fst[end].indent
    else
        nest!(fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
n_generator!(fst::FST, s::State) = n_block!(fst, s, custom_indent = fst.indent)
n_filter!(fst::FST, s::State) = n_block!(fst, s, custom_indent = fst.indent)
n_comparison!(fst::FST, s::State) = n_block!(fst, s)
n_chainopcall!(fst::FST, s::State) = n_block!(fst, s)
