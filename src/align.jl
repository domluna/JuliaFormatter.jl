function align_fst!(fst::FST, opts::Options)
    is_leaf(fst) && return
    nconsts = 0
    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif n.typ === CSTParser.ConditionalOpCall
            align_conditionalopcall!(n)
        elseif n.typ === CSTParser.Const
            nconsts += 1
        elseif opts.align_struct_fields &&
               (n.typ === CSTParser.Struct || n.typ === CSTParser.Mutable)
            align_struct!(n)
            # elseif n.typ === CSTParser.FileH
            # align_short_function!(n)
        else
            align_fst!(n, opts)
        end
    end

    nconsts > 0 && align_consts!(fst)
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

    # determine the longest field name in the struct
    bn = fst[idx]
    max_field_len = 0
    for n in bn.nodes
        if n.typ === CSTParser.BinaryOpCall
            if length(n[1]) > max_field_len
                max_field_len = length(n[1])
            end
        end
    end

    # Don't align unless there are multiple fields
    nfields > 1 || return

    for n in bn.nodes
        if n.typ === CSTParser.BinaryOpCall && !CSTParser.defines_function(n.ref[])
            align_binaryopcall!(n, max_field_len)
        end
    end
end

"""
    align_conditionalopcall!(fst::FST) 
"""
function align_conditionalopcall!(fst::FST)
    return
end

"""
    align_const!(fst::FST) 

_Assumes `fst` has nodes of type `CSTParser.Const`._
"""
function align_consts!(fst::FST)
    fidx = findfirst(n -> n.typ === CSTParser.Const, fst.nodes)
    lidx = findlast(n -> n.typ === CSTParser.Const, fst.nodes)

    groups = Tuple{Int,Vector{Int}}[]
    group = Int[]

    prev_endline = fst[fidx].endline
    max_len = 0

    for (i, n) in enumerate(fst.nodes[fidx:lidx])
        if n.typ === CSTParser.Const && n[3].typ === CSTParser.BinaryOpCall
            if n.startline - prev_endline > 1
                push!(groups, (max_len, group))
                max_len = 0
                group = Int[]
            end
            len = length(n[3][1])
            if len > max_len
                max_len = len
            end
            push!(group, fidx + i - 1)

            prev_endline = n.endline
        end
    end
    push!(groups, (max_len, group))

    for g in groups
        len = g[1]
        idxs = g[2]
        for i in idxs
            @info "group" fst[i][3]
            align_binaryopcall!(fst[i][3], len)
        end
    end

    return
end

function align_binaryopcall!(fst::FST, align_len::Int)
    diff = align_len - length(fst[1]) + 1
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
