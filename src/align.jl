
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

struct AlignGroup
    lens::Vector{Int}
    line_offsets::Vector{Int}
    node_idxs::Vector{Int}
end

function align_to(g::AlignGroup)::Union{Nothing,Int}
    # determine whether alignment might be warranted
    max_len, max_idx = findmax(g.lens)
    max_idxs = findall(==(g.line_offsets[max_idx]), g.line_offsets)
    length(max_idxs) > 1 || return nothing

    # is there custom whitespace?
    for i in max_idxs
        if max_len - g.lens[i] + 1 > 1
            return max_len
        end
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
    groups = AlignGroup[]
    src_line_offsets = Int[]
    idxs = Int[]
    lens = Int[]
    prev_endline = block_fst[1].endline

    for (i, n) in enumerate(block_fst.nodes)
        if n.typ === CSTParser.BinaryOpCall
            if n.startline - prev_endline > 1
                if length(lens) > 1
                    push!(groups, AlignGroup(lens, src_line_offsets, idxs))
                end
                idxs = Int[]
                src_line_offsets = Int[]
                lens = Int[]
            end
            push!(idxs, i)
            push!(src_line_offsets, n.line_offset)
            push!(lens, length(n[1]))

            prev_endline = n.endline
        end
    end
    if length(lens) > 1
        push!(groups, AlignGroup(lens, src_line_offsets, idxs))
    end

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
    groups = AlignGroup[]
    src_line_offsets = Int[]
    idxs = Int[]
    lens = Int[]
    prev_endline = fst[assignment_idxs[1]].endline

    for i in assignment_idxs
        n = fst[i]
        if n.startline - prev_endline > 1
            if length(lens) > 1
                push!(groups, AlignGroup(lens, src_line_offsets, idxs))
            end
            idxs = Int[]
            src_line_offsets = Int[]
            lens = Int[]
        end
        push!(idxs, i)
        push!(src_line_offsets, n.line_offset)

        if n.typ === CSTParser.BinaryOpCall
            push!(lens, length(n[1]))
        else
            push!(lens, length(n[3][1]))
        end

        prev_endline = n.endline
    end
    if length(lens) > 1
        push!(groups, AlignGroup(lens, src_line_offsets, idxs))
    end

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
    cond_src_line_offsets = Int[]
    cond_idxs = Int[]
    cond_lens = Int[]
    cond_prev_endline = 0

    colon_src_line_offsets = Int[]
    colon_idxs = Int[]
    colon_lens = Int[]
    colon_prev_endline = 0

    for (i, n) in enumerate(nodes)
        if n.typ === CSTParser.OPERATOR && n.val == "?"
            if cond_prev_endline != n.endline
                push!(cond_idxs, i)
                push!(cond_src_line_offsets, n.line_offset)
                push!(cond_lens, length(nodes[i-2]))
            end
            cond_prev_endline = n.endline
        elseif n.typ === CSTParser.OPERATOR && n.val == ":"
            if colon_prev_endline != n.endline
                push!(colon_idxs, i)
                push!(colon_src_line_offsets, n.line_offset)
                push!(colon_lens, length(nodes[i-2]))
            end
            colon_prev_endline = n.endline
        end
    end
    length(cond_idxs) > 1 || return

    cond_group = AlignGroup(cond_lens, cond_src_line_offsets, cond_idxs)
    colon_group = AlignGroup(colon_lens, colon_src_line_offsets, colon_idxs)

    len = align_to(cond_group)
    if len !== nothing
        for (i, idx) in enumerate(cond_group.node_idxs)
            diff = len - cond_group.lens[i] + 1
            nodes[idx-1] = Whitespace(diff)
        end
    end

    len = align_to(colon_group)
    for (i, idx) in enumerate(colon_group.node_idxs)
        # the placeholder would be i+1 if not for a possible inline comment
        nidx = findnext(n -> n.typ === PLACEHOLDER, nodes, idx + 1)
        if nodes[nidx+1].startline != nodes[nidx].startline
            nodes[nidx] = Newline(nest_behavior = AlwaysNest)
        end

        if len !== nothing
            diff = len - colon_group.lens[i] + 1
            nodes[idx-1] = Whitespace(diff)
        end
    end

    fst.nodes = nodes
    fst.nest_behavior = NeverNest
    return
end
