# Alignment is determined by a whitespace heuristic. Since the formatter
# separates operators by either 0 and 1 whitespaces if there there are
# aligned nodes and one of the nodes has >= 2 whitespaces before the aligned
# operator then alignment will occur.
#
# Suppose the source text is as follows
#
# ```julia
# const variable1 = 1
# const var2      = 2
# const var3 = 3
# const var4 = 4
# ```
#
# If aligning `const`s is enabled the formatter will detect that `var2`
# and `variable1` are aligned AND `var2` has several whitespaces prior to
# `=`. Since `var3` and `var4` are part of the same code block (no comments
# newlines separating code they will be aligned to `=`.
#
# So the output would be
#
# ```julia
# const variable1 = 1
# const var2      = 2
# const var3      = 3
# const var4      = 4
# ```
#
# However if the source code is
#
# ```julia
# const variable1 = 1
# const variable2 = 2
# const var3 = 3
# const var4 = 4
# ```
#
# It's now ambigious whether this is meant to be aligned and no nothing will
# occur.
#

function align_fst!(fst::FST, opts::Options)
    is_leaf(fst) && return
    nconsts = 0
    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif n.typ === CSTParser.Const
            nconsts += 1
        elseif opts.align_struct_field &&
               (n.typ === CSTParser.Struct || n.typ === CSTParser.Mutable)
            align_struct!(n)
            # elseif n.typ === CSTParser.FileH
            # align_short_function!(n)
        elseif opts.align_conditional && n.typ === CSTParser.ConditionalOpCall
            align_conditionalopcall!(n)
        else
            align_fst!(n, opts)
        end
    end

    opts.align_const && nconsts > 0 && align_consts!(fst)
end

struct AlignmentGroup
    lens::Vector{Int}
    line_offsets::Vector{Int}
    node_idxs::Vector{Int}
end

function align_to(g::AlignmentGroup)::Union{Nothing,Int}
        # determine whether alignment might be warranted
        max_len, max_idx =findmax(g.lens)
        max_idxs = findall(==(g.line_offsets[max_idx]), g.line_offsets) 
        length(max_idxs) > 1 || return nothing
        # @info "" max_len max_idxs

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
    groups = AlignmentGroup[]
    src_line_offsets = Int[]
    idxs = Int[]
    lens = Int[]
    prev_endline = block_fst[1].endline

    for (i, n) in enumerate(block_fst.nodes)
        if n.typ === CSTParser.BinaryOpCall
            if n.startline - prev_endline > 1
                if length(lens) > 1
                    push!(groups, AlignmentGroup(lens, src_line_offsets, idxs))
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
        push!(groups, AlignmentGroup(lens, src_line_offsets, idxs))
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


"""
    align_const!(fst::FST) 

_Assumes `fst` has nodes of type `CSTParser.Const`._
"""
function align_consts!(fst::FST)
    fidx = findfirst(n -> n.typ === CSTParser.Const, fst.nodes)
    lidx = findlast(n -> n.typ === CSTParser.Const, fst.nodes)

    groups = AlignmentGroup[]
    src_line_offsets = Int[]
    idxs = Int[]
    lens = Int[]
    prev_endline = fst[fidx].endline

    for (i, n) in enumerate(fst.nodes[fidx:lidx])
        if n.typ === CSTParser.Const && n[3].typ === CSTParser.BinaryOpCall
            if n.startline - prev_endline > 1
                if length(lens) > 1
                    push!(groups, AlignmentGroup(lens, src_line_offsets, idxs))
                end
                idxs = Int[]
                src_line_offsets = Int[]
                lens = Int[]
            end
            push!(idxs, fidx + i - 1)
            push!(src_line_offsets, n.line_offset)
            push!(lens, length(n[3][1]))

            prev_endline = n.endline
        end
    end
    if length(lens) > 1
        push!(groups, AlignmentGroup(lens, src_line_offsets, idxs))
    end

    for g in groups
        align_len = align_to(g)
        align_len === nothing && continue

        for (i, idx) in enumerate(g.node_idxs)
            diff = align_len - g.lens[i] + 1
            align_binaryopcall!(fst[idx][3], diff)
            fst[idx].nest_behavior = NeverNest
            # if !always_nest(fst[idx])
            #     fst[idx].nest_behavior = NeverNest
            # end
        end
    end

    return
end



"""
"""
function align_conditionalopcall!(fst::FST)
    # findall ? tokens
    # findall : tokens
    #
    # align all ? tokens
    # align all : tokens
    #
    # corner case would be when the initial conditional is
    # joined to the line of `var = cond`
    #
    # ignoring the first ? and : might solve this
    cond_idxs = findall(n -> n.typ === CSTParser.OPERATOR && n.val == "?", fst.nodes)
    length(cond_idxs) == 1 && return
    colon_idxs = findall(n -> n.typ === CSTParser.OPERATOR && n.val == ":", fst.nodes)

    offset1 = fst[cond_idxs[1]].line_offset
    cond_aligned_idxs = filter(i -> fst[i].line_offset == offset1, cond_idxs)
    offset1 = fst[colon_idxs[1]].line_offset
    colon_aligned_idxs = filter(i -> fst[i].line_offset == offset1, colon_idxs)

    # @info "" cond_aligned_idxs colon_aligned_idxs colon_idxs

    # (cond_aligned && colon_aligned) || return

    max_len = 0
    for i in cond_aligned_idxs
        length(fst[i-2]) > max_len && (max_len = length(fst[i-2]))
    end
    for i in cond_aligned_idxs
        diff = max_len - length(fst[i-2]) + 1
        fst[i-1] = Whitespace(diff)
    end

    max_len = 0
    for i in colon_aligned_idxs
        length(fst[i-2]) > max_len && (max_len = length(fst[i-2]))
        if fst[i+2].startline != fst[i].startline
            fst[i+1] = Newline(nest_behavior=AlwaysNest)
        end
    end
    for i in colon_aligned_idxs
        diff = max_len - length(fst[i-2]) + 1
        fst[i-1] = Whitespace(diff)
    end

    fst.nest_behavior = NeverNest
    return
end
