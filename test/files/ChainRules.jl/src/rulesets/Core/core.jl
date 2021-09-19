@non_differentiable Core.print(::Any...)
@non_differentiable Core.println(::Any...)

@non_differentiable Core.show(::Any)
@non_differentiable Core.show(::Any, ::Any)

@non_differentiable Core.isdefined(::Any, ::Any)
@non_differentiable Core.:(<:)(::Any, ::Any)

@non_differentiable Core.apply_type(::Any, ::Any...)
@non_differentiable Core.typeof(::Any)

if isdefined(Core, :_typevar)
    @non_differentiable Core._typevar(::Any...)
end
@non_differentiable TypeVar(::Any...)
@non_differentiable UnionAll(::Any, ::Any)

frule((_, ẋ, _), ::typeof(typeassert), x, T) = (typeassert(x, T), ẋ)
function rrule(::typeof(typeassert), x, T)
    typeassert_pullback(Δ) = (NoTangent(), Δ, NoTangent())
    return typeassert(x, T), typeassert_pullback
end

frule((_, _, ȧ, ḃ), ::typeof(ifelse), c, a, b) = (ifelse(c, a, b), ifelse(c, ȧ, ḃ))
function rrule(::typeof(ifelse), c, a, b)
    function ifelse_pullback(Δ)
        return (
            NoTangent(),
            NoTangent(),
            ifelse(c, Δ, ZeroTangent()),
            ifelse(c, ZeroTangent(), Δ),
        )
    end
    return ifelse(c, a, b), ifelse_pullback
end
# ensure type stability for numbers
function rrule(::typeof(ifelse), c, a::Number, b::Number)
    function ifelse_pullback(Δ)
        return (NoTangent(), NoTangent(), ifelse(c, Δ, zero(Δ)), ifelse(c, zero(Δ), Δ))
    end
    return ifelse(c, a, b), ifelse_pullback
end
