function align_fst!(fst::FST, opts::Options)
    is_leaf(fst) && return
    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif n.typ === CSTParser.ConditionalOpCall
            align_conditionalopcall!(n)
        elseif opts.align_struct_fields && (n.typ === CSTParser.Struct || n.typ === CSTParser.Mutable)
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
    max_fname_length = 0
    for n in fst[idx].nodes
        if n.typ === CSTParser.BinaryOpCall && length(n[1]) > max_fname_length
            max_fname_length = length(n[1])
        end
    end

    for n in fst[idx].nodes
        if n.typ === CSTParser.BinaryOpCall
            diff = max_fname_length - length(n[1]) + 1
            # insert whitespace before and after operator
            insert!(n, 2, Whitespace(diff))
            insert!(n, 4, Whitespace(1))
        end
    end
end

function align_conditionalopcall!(fst::FST)
end
