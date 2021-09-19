#####
##### `dot`
#####

function frule((_, Δx, Δy), ::typeof(dot), x, y)
    return dot(x, y), dot(Δx, y) + dot(x, Δy)
end

function rrule(::typeof(dot), x::AbstractArray, y::AbstractArray)
    project_x = ProjectTo(x)
    project_y = ProjectTo(y)
    function dot_pullback(Ω̄)
        ΔΩ = unthunk(Ω̄)
        x̄ = @thunk(project_x(reshape(y .* ΔΩ', axes(x))))
        ȳ = @thunk(project_y(reshape(x .* ΔΩ, axes(y))))
        return (NoTangent(), x̄, ȳ)
    end
    return dot(x, y), dot_pullback
end

#####
##### 3-arg `dot`
#####

function frule(
    (_, Δx, ΔA, Δy),
    ::typeof(dot),
    x::AbstractVector{<:Number},
    A::AbstractMatrix{<:Number},
    y::AbstractVector{<:Number},
)
    return dot(x, A, y), dot(Δx, A, y) + dot(x, ΔA, y) + dot(x, A, Δy)
end

function rrule(
    ::typeof(dot),
    x::AbstractVector{<:Number},
    A::AbstractMatrix{<:Number},
    y::AbstractVector{<:Number},
)
    project_x = ProjectTo(x)
    project_A = ProjectTo(A)
    project_y = ProjectTo(y)
    Ay = A * y
    z = adjoint(x) * Ay
    function dot_pullback(Ω̄)
        ΔΩ = unthunk(Ω̄)
        dx = @thunk project_x(conj(ΔΩ) .* Ay)
        dA = @thunk project_A(ΔΩ .* x .* adjoint(y))
        dy = @thunk project_y(ΔΩ .* (adjoint(A) * x))
        return (NoTangent(), dx, dA, dy)
    end
    dot_pullback(::ZeroTangent) = (NoTangent(), ZeroTangent(), ZeroTangent(), ZeroTangent())
    return z, dot_pullback
end

function rrule(
    ::typeof(dot),
    x::AbstractVector{<:Number},
    A::Diagonal{<:Number},
    y::AbstractVector{<:Number},
)
    project_x = ProjectTo(x)
    project_A = ProjectTo(A)
    project_y = ProjectTo(y)
    z = dot(x, A, y)
    function dot_pullback(Ω̄)
        ΔΩ = unthunk(Ω̄)
        dx = @thunk project_x(conj(ΔΩ) .* A.diag .* y)  # A*y is this broadcast, can be fused
        dA = @thunk project_A(Diagonal(ΔΩ .* x .* conj(y)))  # calculate N not N^2 elements
        dy = @thunk project_y(ΔΩ .* conj.(A.diag) .* x)
        return (NoTangent(), dx, dA, dy)
    end
    dot_pullback(::ZeroTangent) = (NoTangent(), ZeroTangent(), ZeroTangent(), ZeroTangent())
    return z, dot_pullback
end

#####
##### `cross`
#####

function frule((_, Δa, Δb), ::typeof(cross), a::AbstractVector, b::AbstractVector)
    return cross(a, b), cross(Δa, b) .+ cross(a, Δb)
end

# TODO: support complex vectors
function rrule(::typeof(cross), a::AbstractVector{<:Real}, b::AbstractVector{<:Real})
    project_a = ProjectTo(a)
    project_b = ProjectTo(b)
    Ω = cross(a, b)
    function cross_pullback(Ω̄)
        ΔΩ = unthunk(Ω̄)
        da = @thunk(project_a(cross(b, ΔΩ)))
        db = @thunk(project_b(cross(ΔΩ, a)))
        return (NoTangent(), da, db)
    end
    return Ω, cross_pullback
end

#####
##### `det`
#####

function frule((_, Δx), ::typeof(det), x::AbstractMatrix)
    Ω = det(x)
    # TODO Performance optimization: probably there is an efficent
    # way to compute this trace without during the full compution within
    return Ω, Ω * tr(x \ Δx)
end
frule((_, Δx), ::typeof(det), x::Number) = (det(x), Δx)

function rrule(::typeof(det), x::Union{Number,AbstractMatrix})
    Ω = det(x)
    function det_pullback(ΔΩ)
        ∂x = x isa Number ? ΔΩ : Ω * ΔΩ * inv(x)'
        return (NoTangent(), ∂x)
    end
    return Ω, det_pullback
end

#####
##### `logdet`
#####

function frule((_, Δx), ::typeof(logdet), x::Union{Number,StridedMatrix{<:Number}})
    Ω = logdet(x)
    return Ω, tr(x \ Δx)
end

function rrule(::typeof(logdet), x::Union{Number,StridedMatrix{<:Number}})
    Ω = logdet(x)
    function logdet_pullback(ΔΩ)
        ∂x = x isa Number ? ΔΩ / x' : ΔΩ * inv(x)'
        return (NoTangent(), ∂x)
    end
    return Ω, logdet_pullback
end

#####
##### `logabsdet`
#####

function frule((_, Δx), ::typeof(logabsdet), x::AbstractMatrix)
    Ω = logabsdet(x)
    (y, signy) = Ω
    b = tr(x \ Δx)
    ∂y = real(b)
    ∂signy = eltype(x) <: Real ? ZeroTangent() : im * imag(b) * signy
    ∂Ω = Tangent{typeof(Ω)}(∂y, ∂signy)
    return Ω, ∂Ω
end

function rrule(::typeof(logabsdet), x::AbstractMatrix)
    Ω = logabsdet(x)
    function logabsdet_pullback(ΔΩ)
        (Δy, Δsigny) = ΔΩ
        (_, signy) = Ω
        f = signy' * Δsigny
        imagf = f - real(f)
        g = real(Δy) + imagf
        ∂x = g * inv(x)'
        return (NoTangent(), ∂x)
    end
    return Ω, logabsdet_pullback
end

#####
##### `trace`
#####

function frule((_, Δx), ::typeof(tr), x)
    return tr(x), tr(Δx)
end

function rrule(::typeof(tr), x)
    # This should really be a FillArray
    # see https://github.com/JuliaDiff/ChainRules.jl/issues/46
    d = size(x, 1)
    function tr_pullback(ΔΩ)
        return (NoTangent(), Diagonal(fill(ΔΩ, d)))
    end
    return tr(x), tr_pullback
end

#####
##### `pinv`
#####

@scalar_rule pinv(x) -(Ω^2)

function frule(
    (_, ẋ), ::typeof(pinv), x::AbstractVector{T}, tol::Real=0
) where {T<:Union{Real,Complex}}
    y = pinv(x, tol)
    Δx = unthunk(ẋ)
    ∂y′ = sum(abs2, parent(y)) .* Δx .- 2real(y * Δx) .* parent(y)
    ∂y = y isa Transpose ? transpose(∂y′) : adjoint(∂y′)
    return y, ∂y
end

function frule(
    (_, ẋ), ::typeof(pinv), x::LinearAlgebra.AdjOrTransAbsVec{T}, tol::Real=0
) where {T<:Union{Real,Complex}}
    y = pinv(x, tol)
    Δx = unthunk(ẋ)
    ∂y = sum(abs2, y) .* vec(Δx') .- 2real(Δx * y) .* y
    return y, ∂y
end

# Formula for derivative adapted from Eq 4.12 of
# Golub, Gene H., and Victor Pereyra. "The Differentiation of Pseudo-Inverses and Nonlinear
# Least Squares Problems Whose Variables Separate."
# SIAM Journal on Numerical Analysis 10(2). (1973). 413-432. doi: 10.1137/0710036
function frule((_, Ȧ), ::typeof(pinv), A::AbstractMatrix{T}; kwargs...) where {T}
    Y = pinv(A; kwargs...)
    m, n = size(A)
    # contract over the largest dimension
    ΔA = unthunk(Ȧ)
    if m ≤ n
        ∂Y = -Y * (ΔA * Y)
        ∂Y = add!!(∂Y, (ΔA' - Y * (A * ΔA')) * (Y' * Y))  # (I - Y A) ΔA' Y' Y
        ∂Y = add!!(∂Y, Y * (Y' * ΔA') * (I - A * Y))  # Y Y' ΔA' (I - A Y)
    else
        ∂Y = -(Y * ΔA) * Y
        ∂Y = add!!(∂Y, (I - Y * A) * (ΔA' * Y') * Y)  # (I - Y A) ΔA' Y' Y
        ∂Y = add!!(∂Y, (Y * Y') * (ΔA' - (ΔA' * A) * Y))  # Y Y' ΔA' (I - A Y)
    end
    return Y, ∂Y
end

function rrule(
    ::typeof(pinv), x::Union{AbstractVector{T},LinearAlgebra.AdjOrTransAbsVec{T}}
) where {T<:Union{Real,Complex}}
    y, full_pb = rrule(pinv, x, 0)
    pinv_pullback(Δy) = return full_pb(Δy)[1:2]
    return y, pinv_pullback
end

function rrule(
    ::typeof(pinv), x::AbstractVector{T}, tol::Real
) where {T<:Union{Real,Complex}}
    y = pinv(x, tol)
    function pinv_pullback(ȳ)
        Δy = unthunk(ȳ)
        ∂x = sum(abs2, parent(y)) .* vec(Δy') .- 2real(y * Δy') .* parent(y)
        return (NoTangent(), ∂x, ZeroTangent())
    end
    return y, pinv_pullback
end

function rrule(
    ::typeof(pinv), x::LinearAlgebra.AdjOrTransAbsVec{T}, tol::Real
) where {T<:Union{Real,Complex}}
    y = pinv(x, tol)
    function pinv_pullback(ȳ)
        Δy = ȳ
        ∂x′ = sum(abs2, y) .* Δy .- 2real(y' * Δy) .* y
        ∂x = x isa Transpose ? transpose(conj(∂x′)) : adjoint(∂x′)
        return (NoTangent(), ∂x, ZeroTangent())
    end
    return y, pinv_pullback
end

function rrule(::typeof(pinv), A::AbstractMatrix{T}; kwargs...) where {T}
    Y = pinv(A; kwargs...)
    function pinv_pullback(Ȳ)
        ΔY = unthunk(Ȳ)
        m, n = size(A)
        # contract over the largest dimension
        if m ≤ n
            ∂A = (Y' * -ΔY) * Y'
            ∂A = add!!(∂A, (Y' * Y) * (ΔY' - (ΔY' * Y) * A)) # Y' Y ΔY' (I - Y A)
            ∂A = add!!(∂A, (I - A * Y) * (ΔY' * Y) * Y') # (I - A Y) ΔY' Y Y'
        elseif m > n
            ∂A = Y' * (-ΔY * Y')
            ∂A = add!!(∂A, Y' * (Y * ΔY') * (I - Y * A)) # Y' Y ΔY' (I - Y A)
            ∂A = add!!(∂A, (ΔY' - A * (Y * ΔY')) * (Y * Y')) # (I - A Y) ΔY' Y Y'
        end
        return (NoTangent(), ∂A)
    end
    return Y, pinv_pullback
end

#####
##### `sylvester`
#####

# included because the primal uses `schur`, for which we don't have a rule

function frule(
    (_, ΔA, ΔB, ΔC),
    ::typeof(sylvester),
    A::StridedMatrix{T},
    B::StridedMatrix{T},
    C::StridedMatrix{T},
) where {T<:BlasFloat}
    RA, QA = schur(A)
    RB, QB = schur(B)
    D = QA' * (C * QB)
    Y, scale = LAPACK.trsyl!('N', 'N', RA, RB, D)
    Ω = rmul!(QA * (Y * QB'), -inv(scale))
    ∂D = QA' * (mul!(muladd(ΔA, Ω, ΔC), Ω, ΔB, true, true) * QB)
    ∂Y, scale2 = LAPACK.trsyl!('N', 'N', RA, RB, ∂D)
    ∂Ω = rmul!(QA * (∂Y * QB'), -inv(scale2))
    return Ω, ∂Ω
end

# included because the primal mutates and uses `schur` and LAPACK

function rrule(
    ::typeof(sylvester), A::StridedMatrix{T}, B::StridedMatrix{T}, C::StridedMatrix{T}
) where {T<:BlasFloat}
    RA, QA = schur(A)
    RB, QB = schur(B)
    D = QA' * (C * QB)
    Y, scale = LAPACK.trsyl!('N', 'N', RA, RB, D)
    Ω = rmul!(QA * (Y * QB'), -inv(scale))
    function sylvester_pullback(ΔΩ)
        ∂Ω = T <: Real ? real(ΔΩ) : ΔΩ
        ∂Y = QA' * (∂Ω * QB)
        trans = T <: Complex ? 'C' : 'T'
        ∂D, scale2 = LAPACK.trsyl!(trans, trans, RA, RB, ∂Y)
        ∂Z = rmul!(QA * (∂D * QB'), -inv(scale2))
        return NoTangent(), @thunk(∂Z * Ω'), @thunk(Ω' * ∂Z), @thunk(∂Z * inv(scale))
    end
    return Ω, sylvester_pullback
end

#####
##### `lyap`
#####

# included because the primal uses `schur`, for which we don't have a rule

function frule(
    (_, Ȧ, Ċ), ::typeof(lyap), A::StridedMatrix{T}, C::StridedMatrix{T}
) where {T<:BlasFloat}
    ΔA = unthunk(Ȧ)
    ΔC = unthunk(Ċ)
    R, Q = schur(A)
    D = Q' * (C * Q)
    Y, scale = LAPACK.trsyl!('N', T <: Complex ? 'C' : 'T', R, R, D)
    Ω = rmul!(Q * (Y * Q'), -inv(scale))
    ∂D = Q' * (mul!(muladd(ΔA, Ω, ΔC), Ω, ΔA', true, true) * Q)
    ∂Y, scale2 = LAPACK.trsyl!('N', T <: Complex ? 'C' : 'T', R, R, ∂D)
    ∂Ω = rmul!(Q * (∂Y * Q'), -inv(scale2))
    return Ω, ∂Ω
end

# included because the primal mutates and uses `schur` and LAPACK

function rrule(
    ::typeof(lyap), A::StridedMatrix{T}, C::StridedMatrix{T}
) where {T<:BlasFloat}
    R, Q = schur(A)
    D = Q' * (C * Q)
    Y, scale = LAPACK.trsyl!('N', T <: Complex ? 'C' : 'T', R, R, D)
    Ω = rmul!(Q * (Y * Q'), -inv(scale))
    function lyap_pullback(ΔΩ)
        ∂Ω = T <: Real ? real(ΔΩ) : ΔΩ
        ∂Y = Q' * (∂Ω * Q)
        ∂D, scale2 = LAPACK.trsyl!(T <: Complex ? 'C' : 'T', 'N', R, R, ∂Y)
        ∂Z = rmul!(Q * (∂D * Q'), -inv(scale2))
        return NoTangent(),
        @thunk(mul!(∂Z * Ω', ∂Z', Ω, true, true)),
        @thunk(∂Z * inv(scale))
    end
    return Ω, lyap_pullback
end
