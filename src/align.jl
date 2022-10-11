function align_fst!(fst::FST, opts::Options)
    is_leaf(fst) && return
    const_inds = Int[]
    assignment_inds = Int[]
    pair_arrow_inds = Int[]

    for (i, n) in enumerate(fst.nodes)
        if is_leaf(n)
            continue
        elseif opts.align_struct_field && (n.typ === Struct || n.typ === Mutable)
            align_struct!(n)
        elseif opts.align_conditional && n.typ === Conditional
            align_conditional!(n)
        elseif opts.align_matrix && (
            n.typ === Vcat || n.typ === TypedVcat || n.typ === Ncat || n.typ === TypedNcat
        )
            align_matrix!(n)
        else
            align_fst!(n, opts)
        end

        if opts.align_assignment && (is_assignment(n) || n.typ === Kw)
            # Gather all assignments within the current code block
            # they will be aligned at the end
            push!(assignment_inds, i)
        elseif opts.align_pair_arrow && n.typ === Binary && op_kind(n) === Tokens.PAIR_ARROW
            push!(pair_arrow_inds, i)
        end
    end

    align_binaryopcalls!(fst, assignment_inds)
    align_binaryopcalls!(fst, pair_arrow_inds)
end

"""
    AlignGroup

Group of FST node indices and required metadata to potentially align them.

    - `node_inds`. Indices of FST nodes affected by alignment.
    - `nodes`. FST nodes affected by alignment.
    - `line_offsets`. Line offset of the character nodes may be aligned to
    in the source file.
    - `lens`. Length of the FST node prior to the alignment character. Used
    to calculate extra whitespace padding.
    - `whitespaces`. Number of whitespaces between the alignment character and
    the prior FST node. If this is > 1 it signifies additional whitespace was
    manually added by the user since the formatter would only use 0 or 1 whitespaces.
"""
struct AlignGroup
    nodes::Vector{Ref{FST}}
    node_inds::Vector{Int}
    line_offsets::Vector{Int}
    lens::Vector{Int}
    whitespaces::Vector{Int}
end
AlignGroup() = AlignGroup(Vector{Ref{FST}}[], Int[], Int[], Int[], Int[])

function Base.push!(g::AlignGroup, n::FST, ind::Int, line_offset::Int, len::Int, ws::Int)
    push!(g.nodes, Ref(n))
    push!(g.node_inds, ind)
    push!(g.line_offsets, line_offset)
    push!(g.lens, len)
    push!(g.whitespaces, ws)
    return
end

function align_to(g::AlignGroup)::Union{Nothing,Int}
    length(g.lens) > 0 || return nothing
    # determine whether alignment might be warranted
    max_len, max_ind = findmax(g.lens)
    max_inds = findall(==(g.line_offsets[max_ind]), g.line_offsets)
    length(max_inds) > 1 || return nothing

    # Is there custom whitespace?
    # Formatter would only add 0 or 1 whitespaces.
    # > 2 implies a manual edit in the source file.
    for i in max_inds
        g.whitespaces[i] > 1 && return max_len
    end

    return nothing
end

function align_binaryopcall!(fst::FST, diff::Int)
    # insert whitespace before and after operator
    find = findfirst(x -> x.typ === WHITESPACE, fst.nodes)
    lind = findlast(x -> x.typ === WHITESPACE, fst.nodes)

    if find === nothing
        insert!(fst, 2, Whitespace(diff))
    else
        fst[find] = Whitespace(diff)
    end

    if lind === nothing
        insert!(fst, 4, Whitespace(1))
    end
end

"""
    align_binaryopcalls!(fst::FST, op_inds::Vector{Int})

Aligns binary operator expressions.

Additionally handles the case where a keyword such as `const` is used
prior to the binary op call.
"""
function align_binaryopcalls!(fst::FST, op_inds::Vector{Int})
    length(op_inds) > 1 || return
    prev_endline = fst[op_inds[1]].endline
    groups = AlignGroup[]
    g = AlignGroup()

    for i in op_inds
        n = fst[i]
        if n.startline - prev_endline > 1
            push!(groups, g)
            g = AlignGroup()
        end

        binop, nlen, ws = if n.typ === Binary || n.typ === Kw
            nlen = length(n[1])
            n, nlen, (n[3].line_offset - n.line_offset) - nlen
        else
            binop::Union{FST,Nothing} = nothing
            sn = n
            nlen = 0
            ws = 0

            # the binary op may not be at the top level
            # for instance, @call1 @call2 @call3 x = 10
            while sn.nodes !== nothing
                binop_ind = findfirst(nn -> nn.typ === Binary, sn.nodes)
                nlen += sum(length.(sn[1:end-1]))
                if binop_ind === nothing
                    sn = sn.nodes[end]
                else
                    binop = sn.nodes[binop_ind]
                    nlen += length(binop[1])
                    ws = (binop[3].line_offset - n.line_offset) - nlen
                    break
                end
            end
            binop, nlen, ws
        end

        if binop === nothing
            @warn "Binary operator not found in node" n.typ 
            continue
        end

        push!(g, binop, i, binop[3].line_offset, nlen, ws)
        prev_endline = n.endline
    end
    push!(groups, g)

    for g in groups
        align_len = align_to(g)
        align_len === nothing && continue

        for (i, nr) in enumerate(g.nodes)
            diff = align_len - g.lens[i] + 1
            # reference to node
            n = nr[]
            align_binaryopcall!(n, diff)
            n.nest_behavior = NeverNest
        end
    end

    return
end

"""
    align_struct!(fst::FST)

Aligns struct fields.
"""
function align_struct!(fst::FST)
    ind = findfirst(n -> n.typ === Block, fst.nodes)
    ind === nothing && return
    length(fst[ind]) == 0 && return

    block_fst = fst[ind]
    prev_endline = block_fst[1].endline
    groups = AlignGroup[]
    g = AlignGroup()

    for (i, n) in enumerate(block_fst.nodes)
        if n.typ === Binary
            if n.startline - prev_endline > 1
                push!(groups, g)
                g = AlignGroup()
            end

            nlen = length(n[1])
            ind = findfirst(x -> x.typ === OPERATOR, n.nodes)
            ws = n[ind].line_offset - (n.line_offset + nlen)

            push!(g, n, i, n[ind].line_offset, nlen, ws)
            prev_endline = n.endline
        end
    end
    push!(groups, g)

    for g in groups
        align_len = align_to(g)
        align_len === nothing && continue
        for (i, nr) in enumerate(g.nodes)
            n = nr[]
            diff = align_len - g.lens[i] + 1
            align_binaryopcall!(n, diff)
            n.nest_behavior = NeverNest
        end
    end
end

"""
    align_conditional!(fst::FST)

Aligns a conditional expression.
"""
function align_conditional!(fst::FST)
    nodes = flatten_conditionalopcall(fst)

    cond_group = AlignGroup()
    cond_prev_endline = 0

    colon_group = AlignGroup()
    colon_prev_endline = 0

    for (i, n) in enumerate(nodes)
        if n.typ === OPERATOR && n.val == "?"
            if cond_prev_endline != n.endline
                nlen = length(nodes[i-2])
                ws = n.line_offset - (nodes[i-2].line_offset + nlen)
                push!(cond_group, n, i, n.line_offset, nlen, ws)
            end
            cond_prev_endline = n.endline
        elseif n.typ === OPERATOR && n.val == ":"
            if colon_prev_endline != n.endline
                nlen = length(nodes[i-2])
                ws = n.line_offset - (nodes[i-2].line_offset + nlen)
                push!(colon_group, n, i, n.line_offset, nlen, ws)
            end
            colon_prev_endline = n.endline
        end
    end
    length(cond_group.lens) > 1 || return

    cond_len = align_to(cond_group)
    colon_len = align_to(colon_group)

    cond_len === nothing && colon_len === nothing && return

    if cond_len !== nothing
        for (i, ind) in enumerate(cond_group.node_inds)
            diff = cond_len - cond_group.lens[i] + 1
            nodes[ind-1] = Whitespace(diff)
        end
    end

    for (i, ind) in enumerate(colon_group.node_inds)
        # the placeholder would be i+1 if not for a possible inline comment
        nind = findnext(n -> n.typ === PLACEHOLDER, nodes, ind + 1)
        if nodes[nind+1].startline != nodes[nind].startline
            nodes[nind] = Newline(nest_behavior = AlwaysNest)
        end

        if colon_len !== nothing
            diff = colon_len - colon_group.lens[i] + 1
            nodes[ind-1] = Whitespace(diff)
        end
    end

    fst.nodes = nodes
    fst.nest_behavior = NeverNest
    fst.indent = fst.line_offset - 1

    return
end

"""
Adjust whitespace in between matrix elements such that it's the same as the original source file.
"""
function align_matrix!(fst::FST)
    rows = filter(n -> n.typ === Row, fst.nodes)
    length(rows) == 0 && return

    min_offset = minimum(map(rows) do r
        r[1].line_offset
    end)

    line = 0
    # add whitespace prior to initial element if elements are aligned to the right and
    # it's the first row on that line.
    for r in rows
        if r[1].line_offset > min_offset && line != r.startline
            diff = r[1].line_offset - min_offset
            insert!(r.nodes, 1, Whitespace(diff))
        end
        line = r.startline
    end

    for r in rows
        for (i, n) in enumerate(r.nodes)
            # skip whitespace nodes appearing before initial element
            if i > 1 && n.typ === WHITESPACE
                n1 = r[i-1]
                n2 = r[i+1]

                diff = n2.line_offset - n1.line_offset - length(n1)

                r[i] = Whitespace(diff)
            end
        end
    end

    return
end
