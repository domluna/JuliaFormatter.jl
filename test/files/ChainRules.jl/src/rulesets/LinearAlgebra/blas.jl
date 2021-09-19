#=
These implementations were ported from the wonderful DiffLinearAlgebra
package (https://github.com/invenia/DiffLinearAlgebra.jl).
=#

using LinearAlgebra: BlasFloat

_zeros(x) = fill!(similar(x), zero(eltype(x)))

#####
##### `BLAS.dot`
#####

frule((Δself, Δx, Δy), ::typeof(BLAS.dot), x, y) = frule((Δself, Δx, Δy), dot, x, y)

rrule(::typeof(BLAS.dot), x, y) = rrule(dot, x, y)

function rrule(::typeof(BLAS.dot), n, X, incx, Y, incy)
    Ω = BLAS.dot(n, X, incx, Y, incy)
    function blas_dot_pullback(Ω̄)
        ΔΩ = unthunk(Ω̄)
        if ΔΩ isa ZeroTangent
            ∂X = ZeroTangent()
            ∂Y = ZeroTangent()
        else
            ∂X = @thunk scal!(n, ΔΩ, blascopy!(n, Y, incy, _zeros(X), incx), incx)
            ∂Y = @thunk scal!(n, ΔΩ, blascopy!(n, X, incx, _zeros(Y), incy), incy)
        end
        return (NoTangent(), NoTangent(), ∂X, NoTangent(), ∂Y, NoTangent())
    end
    return Ω, blas_dot_pullback
end

#####
##### `BLAS.nrm2`
#####

function frule((_, Δx), ::typeof(BLAS.nrm2), x)
    Ω = BLAS.nrm2(x)
    s = ifelse(iszero(Ω), one(Ω), Ω)
    ∂Ω = if x isa Real
        BLAS.dot(x, Δx) / s
    else
        sum(y -> _realconjtimes(y...), zip(x, Δx)) / s
    end
    return Ω, ∂Ω
end

function rrule(::typeof(BLAS.nrm2), x)
    Ω = BLAS.nrm2(x)
    function nrm2_pullback(ΔΩ)
        return NoTangent(), x .* (real(ΔΩ) / ifelse(iszero(Ω), one(Ω), Ω))
    end
    return Ω, nrm2_pullback
end

function rrule(::typeof(BLAS.nrm2), n, X, incx)
    Ω = BLAS.nrm2(n, X, incx)
    nrm2_pullback(::ZeroTangent) = (NoTangent(), NoTangent(), ZeroTangent(), NoTangent())
    function nrm2_pullback(ΔΩ)
        # BLAS.scal! requires s has the same eltype as X
        s = eltype(X)(real(ΔΩ) / ifelse(iszero(Ω), one(Ω), Ω))
        ∂X = scal!(n, s, blascopy!(n, X, incx, _zeros(X), incx), incx)
        return (NoTangent(), NoTangent(), ∂X, NoTangent())
    end
    return Ω, nrm2_pullback
end

#####
##### `BLAS.asum`
#####

function frule((_, Δx), ::typeof(BLAS.asum), x)
    ∂Ω = sum(zip(x, Δx)) do (xi, Δxi)
        return _realconjtimes(_signcomp(xi), Δxi)
    end
    return BLAS.asum(x), ∂Ω
end

function rrule(::typeof(BLAS.asum), x)
    function asum_pullback(ΔΩ)
        return (NoTangent(), _signcomp.(x) .* real(ΔΩ))
    end
    return BLAS.asum(x), asum_pullback
end

function rrule(::typeof(BLAS.asum), n, X, incx)
    Ω = BLAS.asum(n, X, incx)
    asum_pullback(::ZeroTangent) = (NoTangent(), NoTangent(), ZeroTangent(), NoTangent())
    function asum_pullback(ΔΩ)
        # BLAS.scal! requires s has the same eltype as X
        s = eltype(X)(real(ΔΩ))
        ∂X = scal!(n, s, blascopy!(n, _signcomp.(X), incx, _zeros(X), incx), incx)
        return (NoTangent(), NoTangent(), ∂X, NoTangent())
    end
    return Ω, asum_pullback
end

# component-wise sign, e.g. sign(x) + i sign(y)
@inline _signcomp(x::Real) = sign(x)
@inline _signcomp(x::Complex) = Complex(sign(real(x)), sign(imag(x)))

#####
##### `BLAS.gemv`
#####

function rrule(
    ::typeof(gemv), tA::Char, α::T, A::AbstractMatrix{T}, x::AbstractVector{T}
) where {T<:BlasFloat}
    y = gemv(tA, α, A, x)
    function gemv_pullback(Ȳ)
        ȳ = unthunk(Ȳ)
        if uppercase(tA) === 'N'
            ∂A = InplaceableThunk(Ā -> ger!(α', ȳ, x, Ā), @thunk(α' * ȳ * x'))
            ∂x = InplaceableThunk(
                x̄ -> gemv!('C', α', A, ȳ, one(T), x̄), @thunk(gemv('C', α', A, ȳ))
            )
        elseif uppercase(tA) === 'C'
            ∂A = InplaceableThunk(Ā -> ger!(α, x, ȳ, Ā), @thunk(α * x * ȳ'))
            ∂x = InplaceableThunk(
                x̄ -> gemv!('N', α', A, ȳ, one(T), x̄), @thunk(gemv('N', α', A, ȳ))
            )
        else  # uppercase(tA) === 'T'
            ∂A = InplaceableThunk(
                Ā -> conj!(ger!(α, x, ȳ, conj!(Ā))), @thunk(conj(α * x * ȳ'))
            )
            ∂x = InplaceableThunk(
                x̄ -> gemv!('N', α', conj(A), ȳ, one(T), x̄),
                @thunk(gemv('N', α', conj(A), ȳ)),
            )
        end
        return (NoTangent(), NoTangent(), @thunk(dot(y, ȳ) / α'), ∂A, ∂x)
    end
    return y, gemv_pullback
end

function rrule(
    ::typeof(gemv), tA::Char, A::AbstractMatrix{T}, x::AbstractVector{T}
) where {T<:BlasFloat}
    y, inner_pullback = rrule(gemv, tA, one(T), A, x)
    function gemv_pullback(Ȳ)
        (_, dtA, _, dA, dx) = inner_pullback(Ȳ)
        return (NoTangent(), dtA, dA, dx)
    end
    return y, gemv_pullback
end

#####
##### `BLAS.gemm`
#####

function rrule(
    ::typeof(gemm), tA::Char, tB::Char, α::T, A::AbstractMatrix{T}, B::AbstractMatrix{T}
) where {T<:BlasFloat}
    C = gemm(tA, tB, α, A, B)
    function gemm_pullback(Cbar)
        C̄ = unthunk(Cbar)
        β = one(T)
        if uppercase(tA) === 'N'
            if uppercase(tB) === 'N'
                ∂A = InplaceableThunk(
                    Ā -> gemm!('N', 'C', α', C̄, B, β, Ā),
                    @thunk(gemm('N', 'C', α', C̄, B)),
                )
                ∂B = InplaceableThunk(
                    B̄ -> gemm!('C', 'N', α', A, C̄, β, B̄),
                    @thunk(gemm('C', 'N', α', A, C̄)),
                )
            elseif uppercase(tB) === 'C'
                ∂A = InplaceableThunk(
                    Ā -> gemm!('N', 'N', α', C̄, B, β, Ā),
                    @thunk(gemm('N', 'N', α', C̄, B)),
                )
                ∂B = InplaceableThunk(
                    B̄ -> gemm!('C', 'N', α, C̄, A, β, B̄), @thunk(gemm('C', 'N', α, C̄, A))
                )
            else  # uppercase(tB) === 'T'
                ∂A = InplaceableThunk(
                    Ā -> gemm!('N', 'N', α', C̄, conj(B), β, Ā),
                    @thunk(gemm('N', 'N', α', C̄, conj(B))),
                )
                ∂B = InplaceableThunk(
                    B̄ -> conj!(gemm!('C', 'N', α, C̄, A, β, conj!(B̄))),
                    @thunk(conj(gemm('C', 'N', α, C̄, A))),
                )
            end
        elseif uppercase(tA) === 'C'
            if uppercase(tB) === 'N'
                ∂A = InplaceableThunk(
                    Ā -> gemm!('N', 'C', α, B, C̄, β, Ā), @thunk(gemm('N', 'C', α, B, C̄))
                )
                ∂B = InplaceableThunk(
                    B̄ -> gemm!('N', 'N', α', A, C̄, β, B̄),
                    @thunk(gemm('N', 'N', α', A, C̄)),
                )
            elseif uppercase(tB) === 'C'
                ∂A = InplaceableThunk(
                    Ā -> gemm!('C', 'C', α, B, C̄, β, Ā), @thunk(gemm('C', 'C', α, B, C̄))
                )
                ∂B = InplaceableThunk(
                    B̄ -> gemm!('C', 'C', α, C̄, A, β, B̄), @thunk(gemm('C', 'C', α, C̄, A))
                )
            else  # uppercase(tB) === 'T'
                ∂A = InplaceableThunk(
                    Ā -> gemm!('T', 'C', α, B, C̄, β, Ā), @thunk(gemm('T', 'C', α, B, C̄))
                )
                ∂B = InplaceableThunk(
                    B̄ -> gemm!('T', 'T', α', C̄, A, β, B̄),
                    @thunk(gemm('T', 'T', α', C̄, A)),
                )
            end
        else  # uppercase(tA) === 'T'
            if uppercase(tB) === 'N'
                ∂A = InplaceableThunk(
                    Ā -> conj!(gemm!('N', 'C', α, B, C̄, β, conj!(Ā))),
                    @thunk(conj(gemm('N', 'C', α, B, C̄))),
                )
                ∂B = InplaceableThunk(
                    B̄ -> gemm!('N', 'N', α', conj(A), C̄, β, B̄),
                    @thunk(gemm('N', 'N', α', conj(A), C̄)),
                )
            elseif uppercase(tB) === 'C'
                ∂A = InplaceableThunk(
                    Ā -> gemm!('T', 'T', α', B, C̄, β, Ā),
                    @thunk(gemm('T', 'T', α', B, C̄)),
                )
                ∂B = InplaceableThunk(
                    B̄ -> gemm!('C', 'T', α, C̄, A, β, B̄), @thunk(gemm('C', 'T', α, C̄, A))
                )
            else  # uppercase(tB) === 'T'
                ∂A = InplaceableThunk(
                    Ā -> gemm!('C', 'T', α', B, C̄, β, Ā),
                    @thunk(gemm('C', 'T', α', B, C̄)),
                )
                ∂B = InplaceableThunk(
                    B̄ -> gemm!('T', 'C', α', C̄, A, β, B̄),
                    @thunk(gemm('T', 'C', α', C̄, A)),
                )
            end
        end
        return (NoTangent(), NoTangent(), NoTangent(), @thunk(dot(C, C̄) / α'), ∂A, ∂B)
    end
    return C, gemm_pullback
end

function rrule(
    ::typeof(gemm), tA::Char, tB::Char, A::AbstractMatrix{T}, B::AbstractMatrix{T}
) where {T<:BlasFloat}
    C, inner_pullback = rrule(gemm, tA, tB, one(T), A, B)
    function gemm_pullback(Ȳ)
        (_, dtA, dtB, _, dA, dB) = inner_pullback(Ȳ)
        return (NoTangent(), dtA, dtB, dA, dB)
    end
    return C, gemm_pullback
end
