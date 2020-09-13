
function align_fst!(fst::FST, opts::Options)
    is_leaf(fst) && return
    const_idxs = Int[]
    assignment_idxs = Int[]

    for (i, n) in enumerate(fst.nodes)
        if is_leaf(n)
            continue
        elseif opts.align_struct_field &&
               (n.typ === CSTParser.Struct || n.typ === CSTParser.Mutable)
            align_struct!(n)
        elseif opts.align_conditional && n.typ === CSTParser.ConditionalOpCall
            align_conditional!(n)
        else
            align_fst!(n, opts)
        end

        if is_assignment(n)
            push!(assignment_idxs, i)
        end
    end

    if opts.align_assignment
        align_assignments!(fst, assignment_idxs)
    end
end

"""
Aligns nodes of an FST (`node_idxs`) to the maximum `op_line_offsets`
"""
struct AlignGroup
    node_idxs::Vector{Int}
    src_line_offsets::Vector{Int}
    lens::Vector{Int}
    whitespaces::Vector{Int}
end
AlignGroup() = AlignGroup(Int[], Int[], Int[], Int[])

function Base.push!(g::AlignGroup, idx::Int, src_line_offset::Int, len::Int, ws::Int)
    push!(g.node_idxs, idx)
    push!(g.src_line_offsets, src_line_offset)
    push!(g.lens, len)
    push!(g.whitespaces, ws)
    return
end

function align_to(g::AlignGroup)::Union{Nothing,Int}
    # determine whether alignment might be warranted
    max_len, max_idx = findmax(g.lens)
    max_idxs = findall(==(g.src_line_offsets[max_idx]), g.src_line_offsets)
    length(max_idxs) > 1 || return nothing

    # is there custom whitespace?
    for i in max_idxs
        g.whitespaces[i] > 1 && return max_len
    end

    return nothing
end

function align_binaryopcall!(fst::FST, diff::Int)
    # insert whitespace before and after operator
    fidx = findfirst(x -> x.typ === WHITESPACE, fst.nodes)
    lidx = findlast(x -> x.typ === WHITESPACE, fst.nodes)

    if fidx === nothing
        insert!(fst, 2, Whitespace(diff))
    else
        fst[fidx] = Whitespace(diff)
    end

    if lidx === nothing
        insert!(fst, 4, Whitespace(1))
    end
end

"""
    align_struct!(fst::FST)

Aligns struct fields.

#### Example 1

```julia
struct Foo
    a::T
    longfieldname::B
end
```

aligns into

```julia
struct Foo
    a             :: T
    longfieldname :: B
end
```

#### Example 2

```julia
Base.@kwdef struct Foo
    a::T = 10
    longfieldname::B = false
end
```

aligns into

```julia
Base.@kwdef struct Foo
    a::T             = 10
    longfieldname::B = false
end
```
"""
function align_struct!(fst::FST)
    idx = findfirst(n -> n.typ === CSTParser.Block, fst.nodes)
    idx === nothing && return
    length(fst[idx]) == 0 && return

    block_fst = fst[idx]
    prev_endline = block_fst[1].endline
    groups = AlignGroup[]
    g = AlignGroup()

    for (i, n) in enumerate(block_fst.nodes)
        if n.typ === CSTParser.BinaryOpCall
            if n.startline - prev_endline > 1
                push!(groups, g)
                g = AlignGroup()
            end

            nlen = length(n[1])
            idx = findfirst(x -> x.typ === CSTParser.OPERATOR, n.nodes)
            ws = n[idx].line_offset - (n.line_offset + nlen)

            push!(g, i, n[idx].line_offset, nlen, ws)
            prev_endline = n.endline
        end
    end
    push!(groups, g)

    for g in groups
        align_len = align_to(g)
        align_len === nothing && continue
        for (i, idx) in enumerate(g.node_idxs)
            diff = align_len - g.lens[i] + 1
            align_binaryopcall!(block_fst[idx], diff)
            block_fst[idx].nest_behavior = NeverNest
        end
    end
end

function align_assignments!(fst::FST, assignment_idxs::Vector{Int})
    length(assignment_idxs) > 1 || return
    prev_endline = fst[assignment_idxs[1]].endline
    groups = AlignGroup[]
    g = AlignGroup()

    for i in assignment_idxs
        n = fst[i]
        if n.startline - prev_endline > 1
            push!(groups, g)
            g = AlignGroup()
        end

        binop = n.typ === CSTParser.BinaryOpCall ? n : n[3]
        nlen = length(binop[1])

        ws = binop[3].line_offset - (binop.line_offset + nlen)
        push!(g, i, binop[3].line_offset, nlen, ws)

        prev_endline = n.endline
    end
    push!(groups, g)

    for g in groups
        align_len = align_to(g)
        align_len === nothing && continue

        for (i, idx) in enumerate(g.node_idxs)
            diff = align_len - g.lens[i] + 1

            if fst[idx].typ === CSTParser.BinaryOpCall
                align_binaryopcall!(fst[idx], diff)
            else
                align_binaryopcall!(fst[idx][3], diff)
            end
            fst[idx].nest_behavior = NeverNest
        end
    end

    return
end

"""
"""
function align_conditional!(fst::FST)
    nodes = flatten_conditionalopcall(fst)

    cond_group = AlignGroup()
    cond_prev_endline = 0

    colon_group = AlignGroup()
    colon_prev_endline = 0

    for (i, n) in enumerate(nodes)
        if n.typ === CSTParser.OPERATOR && n.val == "?"
            if cond_prev_endline != n.endline
                nlen = length(nodes[i-2])
                ws = n.line_offset - (nodes[i-2].line_offset + nlen)
                push!(cond_group, i, n.line_offset, nlen, ws)
            end
            cond_prev_endline = n.endline
        elseif n.typ === CSTParser.OPERATOR && n.val == ":"
            if colon_prev_endline != n.endline
                nlen = length(nodes[i-2])
                ws = n.line_offset - (nodes[i-2].line_offset + nlen)
                push!(colon_group, i, n.line_offset, nlen, ws)
            end
            colon_prev_endline = n.endline
        end
    end
    length(cond_group.lens) > 1 || return

    cond_len = align_to(cond_group)
    colon_len = align_to(colon_group)

    cond_len === nothing && colon_len === nothing && return

    if cond_len !== nothing
        for (i, idx) in enumerate(cond_group.node_idxs)
            diff = cond_len - cond_group.lens[i] + 1
            nodes[idx-1] = Whitespace(diff)
        end
    end

    for (i, idx) in enumerate(colon_group.node_idxs)
        # the placeholder would be i+1 if not for a possible inline comment
        nidx = findnext(n -> n.typ === PLACEHOLDER, nodes, idx + 1)
        if nodes[nidx+1].startline != nodes[nidx].startline
            nodes[nidx] = Newline(nest_behavior = AlwaysNest)
        end

        if colon_len !== nothing
            diff = colon_len - colon_group.lens[i] + 1
            nodes[idx-1] = Whitespace(diff)
        end
    end

    fst.nodes = nodes
    fst.nest_behavior = NeverNest
    fst.indent = fst.line_offset - 1

    return
end
