function align_fst!(fst::FST, opts::Options)
    is_leaf(fst) && return
    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif n.typ === CSTParser.ConditionalOpCall
            align_conditionalopcall!(n)
        elseif opts.align_struct_fields &&
               (n.typ === CSTParser.Struct || n.typ === CSTParser.Mutable)
            align_struct!(n)
        else
            align_fst!(n, opts)
        end
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

    # determine the longest field name in the struct
    bn = fst[idx]
    max_field_len = 0
    nfields = 0
    for n in bn.nodes
        if n.typ === CSTParser.BinaryOpCall
            nfields += 1
            if length(n[1]) > max_field_len
                max_field_len = length(n[1])
            end
        end
    end

    # Don't align unless there are multiple fields
    nfields > 1 || return

    for n in bn.nodes
        if n.typ === CSTParser.BinaryOpCall && !CSTParser.defines_function(n.ref[])
            diff = max_field_len - length(n[1]) + 1
            # insert whitespace before and after operator
            fidx = findfirst(x -> x.typ === WHITESPACE, n.nodes)
            lidx = findlast(x -> x.typ === WHITESPACE, n.nodes)

            if fidx === nothing
                insert!(n, 2, Whitespace(diff))
            else
                n[fidx] = Whitespace(diff)
            end

            if lidx === nothing
                insert!(n, 4, Whitespace(1))
            end
        end
    end
end

"""
    align_conditionalopcall!(fst::FST) 
"""
function align_conditionalopcall!(fst::FST)
    return 0
end
