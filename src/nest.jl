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
#     LHS op RHS
#
# the length of " op" will be considered when nesting LHS

# unnest, converts newlines to whitespace
unnest!(fst::FST, ind::Int) = fst[ind] = Whitespace(fst[ind].len)
function unnest!(fst::FST, nl_inds::Vector{Int})
    for (i, ind) in enumerate(nl_inds)
        unnest!(fst, ind)
        i == length(nl_inds) || continue
        ind2 = ind - 1
        if fst[ind2] isa FST{TRAILINGCOMMA}
            fst[ind2].val = ""
            fst[ind2].len = 0
        elseif fst[ind-1] isa FST{TRAILINGSEMICOLON}
            fst[ind2].val = ";"
            fst[ind2].len = 1
        end
    end
end

skip_indent(::FST) = false
skip_indent(fst::FST{Literal}) = fst.val == ""
skip_indent(::FST{NEWLINE}) = true
skip_indent(::FST{NOTCODE}) = true

function walk(f, fst::FST, s::State)
    f(fst, s)
    is_leaf(fst) && return
    for (i, n) in enumerate(fst.nodes)
        if n isa FST{NEWLINE} && i < length(fst.nodes)
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

function dedent!(fst::FST{T}, s::State) where {T<:Union{AbstractCSTLeaf,AbstractFormatLeaf}}
    s.line_offset += length(fst)
    is_closer(fst) && (fst.indent -= s.indent_size)
end
function dedent!(fst::FST{NOTCODE}, s::State)
    s.line_offset += length(fst)
    fst.indent -= s.indent_size
end
dedent!(::FST{Conditionalopcall}, ::State) = nothing
dedent!(::FST{StringFN}, ::State) = nothing
dedent!(::FST{Binaryopcall}, ::State) = nothing

function dedent!(fst::FST, s::State)
    fst.indent -= s.indent_size
    fst.force_nest && return

    nl_inds = findall(n -> n isa FST{NEWLINE} && !n.force_nest, fst.nodes)
    length(nl_inds) > 0 || return
    margin = s.line_offset + fst.extra_margin + length(fst)
    # @info "" typeof(fst) margin s.line_offset fst.extra_margin length(fst) length(nl_inds)
    margin <= s.margin || return
    unnest!(fst, nl_inds)
end

function nest!(
    style::DefaultStyle,
    nodes::Vector{FST},
    s::State,
    indent::Int;
    extra_margin = 0,
)
    style = getstyle(style)
    for (i, n) in enumerate(nodes)
        if n isa FST{NEWLINE} && nodes[i+1] isa FST{Block}
            s.line_offset = nodes[i+1].indent
        elseif n isa FST{NOTCODE} && nodes[i+1] isa FST{Block}
            s.line_offset = nodes[i+1].indent
        elseif n isa FST{NEWLINE}
            s.line_offset = indent
        else
            n.extra_margin = extra_margin
            nest!(style, n, s)
        end
    end
end
nest!(style::DefaultStyle, fst::FST, s::State) =
    nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
nest!(
    ::DefaultStyle,
    fst::FST{T},
    s::State,
) where {T<:Union{AbstractCSTLeaf,AbstractFormatLeaf}} = s.line_offset += length(fst)

function nest!(style::DefaultStyle, fst::FST{Unaryopcall}, s::State)
    style = getstyle(style)
    if !(fst[2] isa FST{Operator})
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
        return
    end
    fst[1].extra_margin = fst.extra_margin + length(fst[2])
    nest!(style, fst[1], s)
    nest!(style, fst[2], s)
end

function nest!(style::DefaultStyle, fst::FST{Do}, s::State)
    style = getstyle(style)
    extra_margin = sum(length.(fst[2:3]))
    # make sure there are nodes after "do"
    if fst[4] isa FST{WHITESPACE}
        extra_margin += length(fst[4])
        extra_margin += length(fst[5])
    end
    fst[1].extra_margin = fst.extra_margin + extra_margin
    nest!(style, fst[1], s)
    nest!(style, fst[2:end], s, fst.indent, extra_margin = fst.extra_margin)
end

function n_import!(style::DefaultStyle, fst::FST{T}, s::State) where {T}
    style = getstyle(style)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findfirst(is_placeholder, fst.nodes)
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
            if n isa FST{NEWLINE}
                s.line_offset = fst.indent
            elseif n isa FST{PLACEHOLDER}
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            else
                nest!(style, n, s)
            end
        end
    else
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
nest!(style::DefaultStyle, fst::FST{Import}, s::State) = n_import!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Using}, s::State) = n_import!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Export}, s::State) = n_import!(style, fst, s)

function n_tuple!(style::DefaultStyle, fst::FST{T}, s::State) where {T}
    style = getstyle(style)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findlast(is_placeholder, fst.nodes)
    # @info "ENTERING" idx fst.typ s.line_offset length(fst) fst.extra_margin
    opener = length(fst.nodes) > 0 && is_opener(fst[1])
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        if opener
            fst[end].indent = fst.indent
        end
        if !(fst isa FST{Parameters}) && !(fst isa FST{TupleFN} && !opener)
            fst.indent += s.indent_size
        end

        # @info "DURING" fst.typ fst.indent s.line_offset
        for (i, n) in enumerate(fst.nodes)
            if n isa FST{NEWLINE}
                s.line_offset = fst.indent
            elseif n isa FST{PLACEHOLDER}
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n isa FST{TRAILINGCOMMA}
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif n isa FST{TRAILINGSEMICOLON}
                n.val = ""
                n.len = 0
                nest!(style, n, s)
            elseif opener && (i == 1 || i == length(fst.nodes))
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(style, n, s)
            end
        end

        if opener
            s.line_offset = fst[end].indent + 1
        end
        # @info "EXITING" fst.typ s.line_offset fst.indent fst[end].indent
    else
        extra_margin = fst.extra_margin
        opener && (extra_margin += 1)
        nest!(style, fst.nodes, s, fst.indent, extra_margin = extra_margin)
    end
end
nest!(style::DefaultStyle, fst::FST{TupleFN}, s::State) = n_tuple!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Vect}, s::State) = n_tuple!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Vcat}, s::State) = n_tuple!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Braces}, s::State) = n_tuple!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Parameters}, s::State) = n_tuple!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Invisbrackets}, s::State) = n_tuple!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Comprehension}, s::State) = n_tuple!(style, fst, s)

function n_call!(style::DefaultStyle, fst::FST{T}, s::State) where {T}
    style = getstyle(style)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findlast(is_placeholder, fst.nodes)
    # @info "ENTERING" fst.typ s.line_offset length(fst) fst.extra_margin s.margin fst[1].val
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        fst[end].indent = fst.indent
        fst.indent += s.indent_size

        # @info "DURING" fst.typ fst.indent s.line_offset
        for (i, n) in enumerate(fst.nodes)
            if n isa FST{NEWLINE}
                s.line_offset = fst.indent
            elseif n isa FST{PLACEHOLDER}
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n isa FST{TRAILINGCOMMA}
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif n isa FST{TRAILINGSEMICOLON}
                n.val = ""
                n.len = 0
                nest!(style, n, s)
            elseif i == 1 || i == length(fst.nodes)
                n isa FST{Parameters} && (n.force_nest = true)
                n.extra_margin = 1
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n isa FST{Parameters} && (n.force_nest = true)
                n.extra_margin = 1
                nest!(style, n, s)
            end
        end

        s.line_offset = fst[end].indent + 1
    else
        extra_margin = fst.extra_margin
        is_closer(fst[end]) && (extra_margin += 1)
        nest!(style, fst.nodes, s, fst.indent, extra_margin = extra_margin)
    end
end
nest!(style::DefaultStyle, fst::FST{Call}, s::State) = n_call!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Curly}, s::State) = n_call!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Macrocall}, s::State) = n_call!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{RefFN}, s::State) = n_call!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Typedvcat}, s::State) = n_call!(style, fst, s)

function nest!(style::DefaultStyle, fst::FST{Whereopcall}, s::State)
    style = getstyle(style)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    # @info "" s.line_offset fst.typ line_margin fst.extra_margin length(fst) fst.force_nest line_margin > s.margin
    if line_margin > s.margin || fst.force_nest
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(is_placeholder, fst.nodes)
        Blen = sum(length.(fst[idx+1:end]))

        # A, +7 is the length of " where "
        fst[1].extra_margin = Blen + 7 + fst.extra_margin
        nest!(style, fst[1], s)

        for (i, n) in enumerate(fst[2:idx-1])
            nest!(style, n, s)
        end

        has_braces = is_closer(fst[end])
        if has_braces
            fst[end].indent = fst.indent
        end

        over = (s.line_offset + Blen + fst.extra_margin > s.margin) || fst.force_nest
        fst.indent += s.indent_size

        for (i, n) in enumerate(fst[idx+1:end])
            if n isa FST{NEWLINE}
                s.line_offset = fst.indent
            elseif is_opener(n)
                if fst.indent - s.line_offset > 1
                    fst.indent = s.line_offset + 1
                    fst[end].indent = s.line_offset
                end
                nest!(style, n, s)
            elseif n isa FST{PLACEHOLDER} && over
                fst[i+idx] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n isa FST{TRAILINGCOMMA} && over
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif has_braces
                n.extra_margin = 1 + fst.extra_margin
                nest!(style, n, s)
            else
                n.extra_margin = fst.extra_margin
                nest!(style, n, s)
            end
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, fst, s)
    else
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end

function nest!(style::DefaultStyle, fst::FST{Conditionalopcall}, s::State)
    style = getstyle(style)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    # @info "ENTERING" fst.typ s.line_offset line_margin fst.force_nest
    if line_margin > s.margin || fst.force_nest
        line_offset = s.line_offset
        fst.indent = s.line_offset
        phs = reverse(findall(is_placeholder, fst.nodes))
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
                nest!(style, n, s)
            elseif fst[i+1] isa FST{WHITESPACE}
                n.extra_margin = length(fst[i+1]) + length(fst[i+2])
                nest!(style, n, s)
            elseif n isa FST{NEWLINE}
                s.line_offset = fst.indent
            else
                nest!(style, n, s)
            end
        end

        s.line_offset = line_offset
        for (i, n) in enumerate(fst.nodes)
            if n isa FST{NEWLINE} && !is_comment(fst[i+1]) && !is_comment(fst[i-1])
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
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end

no_unnest(::FST) = false
no_unnest(fst::FST{Binaryopcall}) = contains_comment(fst)

function nest!(style::DefaultStyle, fst::FST{Binaryopcall}, s::State)
    style = getstyle(style)
    # If there's no placeholder the binary call is not nestable
    idxs = findall(is_placeholder, fst.nodes)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    rhs = fst[end]
    rhs isa FST{Block} && (rhs = rhs[1])
    # @info "ENTERING" typeof(fst) fst.extra_margin s.line_offset length(fst) idxs fst.ref[][2]
    if length(idxs) == 2 && (line_margin > s.margin || fst.force_nest || rhs.force_nest)
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

        # rhs
        fst[end].extra_margin = fst.extra_margin
        nest!(style, fst[end], s)

        # "lhs op" rhs
        s.line_offset = line_offset

        # extra margin for " op"
        fst[1].extra_margin = length(fst[2]) + length(fst[3])
        nest!(style, fst[1], s)
        for n in fst[2:i1]
            nest!(style, n, s)
        end

        # Undo nest if possible
        if !fst.force_nest && !no_unnest(rhs)
            cst = rhs.ref[]
            line_margin = s.line_offset

            if (
                rhs isa FST{Binaryopcall} &&
                (!(is_lazy_op(cst) && !indent_nest) && cst[2].kind !== Tokens.IN)
            ) || rhs isa FST{Unaryopcall} ||
               rhs isa FST{Chainopcall} || rhs isa FST{Comparison}
                line_margin += length(fst[end])
            elseif rhs isa FST{Do} && is_iterable(rhs[1])
                rw, _ = length_to(fst, [NEWLINE], start = i2 + 1)
                line_margin += rw
            elseif is_block(cst)
                idx = findfirst(n -> n isa FST{NEWLINE}, rhs.nodes)
                if idx === nothing
                    line_margin += length(fst[end])
                else
                    line_margin += sum(length.(rhs[1:idx-1]))
                end
            else
                rw, _ = length_to(fst, [NEWLINE], start = i2 + 1)
                line_margin += rw
            end

            # @info "" rhs.typ indent_nest s.line_offset line_margin fst.extra_margin length(fst[end])
            if line_margin + fst.extra_margin <= s.margin
                fst[i1] = Whitespace(1)
                if indent_nest
                    fst[i2] = Whitespace(0)
                    walk(dedent!, rhs, s)
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
        #  - rhs
        #
        # It's parsed as:
        #
        # CSTParser.BinaryOpCall
        #  - CSTParser.BinaryOpCall
        #   - lhs
        #   - OP
        #   - CSTParser.WhereOpCall
        #    - R
        #    - ...
        #  - OP
        #  - rhs
        #
        # The result being extra width is trickier to deal with.
        idx = findfirst(n -> n isa FST{Whereopcall}, fst.nodes)
        return_width = 0
        if idx !== nothing && idx > 1
            return_width = length(fst[idx]) + length(fst[2])
        elseif idx === nothing
            return_width, _ = length_to(fst, [NEWLINE], start = 2)
        end

        # @info "" idx map(n -> typeof(n), fst.nodes)
        # @info "" s.line_offset length(fst[1]) return_width fst.extra_margin

        for (i, n) in enumerate(fst.nodes)
            if n isa FST{NEWLINE}
                s.line_offset = fst.indent
            elseif i == 1
                n.extra_margin = return_width + fst.extra_margin
                nest!(style, n, s)
            elseif i == idx
                n.extra_margin = fst.extra_margin
                nest!(style, n, s)
            else
                n.extra_margin = fst.extra_margin
                nest!(style, n, s)
            end
        end
    end
end

function nest!(style::DefaultStyle, fst::FST{T}, s::State) where {T<:Union{For,Let}}
    style = getstyle(style)
    block_idx = findfirst(n -> !is_leaf(n), fst.nodes)
    if block_idx === nothing
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
        return
    end
    ph_idx = findfirst(is_placeholder, fst[block_idx].nodes)
    nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)

    # return if the argument block was nested
    ph_idx !== nothing && fst[3][ph_idx] isa FST{NEWLINE} && return

    idx = 5
    n = fst[idx]
    if n isa FST{NOTCODE} && n.startline == n.endline
        res = get(s.doc.comments, n.startline, (0, ""))
        res == (0, "") && deleteat!(fst.nodes, idx - 1)
    end
end

function n_block!(style::DefaultStyle, fst::FST{T}, s::State; custom_indent = 0) where {T}
    style = getstyle(style)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findfirst(is_placeholder, fst.nodes)
    # @info "ENTERING" idx typeof(fst) s.line_offset length(fst) fst.extra_margin
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        custom_indent > 0 && (fst.indent = custom_indent)
        # @info "" fst.indent s.line_offset custom_indent

        # @info "DURING" fst.indent s.line_offset typeof(fst)
        for (i, n) in enumerate(fst.nodes)
            if n isa FST{NEWLINE}
                s.line_offset = fst.indent
            elseif n isa FST{PLACEHOLDER}
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n isa FST{TRAILINGCOMMA}
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif n isa FST{TRAILINGSEMICOLON}
                n.val = ""
                n.len = 0
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(style, n, s)
            end
        end
    else
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
nest!(style::DefaultStyle, fst::FST{Block}, s::State) = n_block!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Comparison}, s::State) = n_block!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Chainopcall}, s::State) = n_block!(style, fst, s)
nest!(style::DefaultStyle, fst::FST{Generator}, s::State) =
    n_block!(style, fst, s, custom_indent = fst.indent)
nest!(style::DefaultStyle, fst::FST{Filter}, s::State) =
    n_block!(style, fst, s, custom_indent = fst.indent)
