function rrule(
    ::typeof(partialsort), xs::AbstractVector, k::Union{Integer,OrdinalRange}; kwargs...
)
    inds = partialsortperm(xs, k; kwargs...)
    ys = xs[inds]

    function partialsort_pullback(Δys)
        function partialsort_add!(Δxs)
            Δxs[inds] += Δys
            return Δxs
        end

        Δxs = InplaceableThunk(partialsort_add!, @thunk(partialsort_add!(zero(xs))))

        return NoTangent(), Δxs, NoTangent()
    end

    return ys, partialsort_pullback
end

function rrule(::typeof(sort), xs::AbstractVector; kwargs...)
    inds = sortperm(xs; kwargs...)
    ys = xs[inds]

    function sort_pullback(ȳ)
        Δys = unthunk(ȳ)
        function sort_add!(Δxs)
            Δxs[inds] += Δys
            return Δxs
        end

        Δxs = InplaceableThunk(sort_add!, @thunk(sort_add!(zero(Δys))))

        return NoTangent(), Δxs
    end
    return ys, sort_pullback
end
