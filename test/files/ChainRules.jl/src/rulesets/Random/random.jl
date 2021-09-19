frule(Δargs, T::Type{<:AbstractRNG}, args...) = T(args...), ZeroTangent()

function rrule(T::Type{<:AbstractRNG}, args...)
    function AbstractRNG_pullback(ΔΩ)
        return (NoTangent(), map(Returns(ZeroTangent()), args)...)
    end
    return T(args...), AbstractRNG_pullback
end

@non_differentiable Broadcast.broadcastable(::AbstractRNG)

@non_differentiable Random.randexp(::AbstractRNG)
@non_differentiable Random.randstring(::AbstractRNG)

@non_differentiable rand(::AbstractRNG, ::Random.Sampler)
@non_differentiable rand(::AbstractRNG, ::Integer...)
@non_differentiable rand(::Type{<:Real})
@non_differentiable rand(::Type{<:Real}, ::Tuple)
@non_differentiable rand(::Type{<:Real}, ::Integer...)
@non_differentiable rand(::Integer...)

# There are many different 1-3 arg methods, but not varargs
@non_differentiable rand!(::Any)
@non_differentiable rand!(::Any, ::Any)
@non_differentiable rand!(::Any, ::Any, ::Any)

@non_differentiable randexp(::Any...)

@non_differentiable randexp!(::AbstractArray)
@non_differentiable randexp!(::AbstractRNG, ::AbstractArray)

@non_differentiable randn(::Any...)

@non_differentiable randn!(::AbstractArray)
@non_differentiable randn!(::AbstractRNG, ::AbstractArray)

@non_differentiable copy(::AbstractRNG)
@non_differentiable copy!(::AbstractRNG, ::AbstractRNG)

@static if VERSION > v"1.3"
    @non_differentiable Random.default_rng()
    @non_differentiable Random.default_rng(::Int)
end
