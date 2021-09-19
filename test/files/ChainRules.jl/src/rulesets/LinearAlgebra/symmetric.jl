#####
##### `Symmetric`/`Hermitian`
#####

function frule((_, ΔA, _), T::Type{<:LinearAlgebra.HermOrSym}, A::AbstractMatrix, uplo)
    return T(A, uplo), T(ΔA, uplo)
end

function rrule(T::Type{<:LinearAlgebra.HermOrSym}, A::AbstractMatrix, uplo)
    Ω = T(A, uplo)
    project_A = ProjectTo(A)
    @inline function HermOrSym_pullback(ΔΩ)
        return (NoTangent(), project_A(_symherm_back(typeof(Ω), ΔΩ, uplo)), NoTangent())
    end
    return Ω, HermOrSym_pullback
end

function frule((_, ΔA), TM::Type{<:Matrix}, A::LinearAlgebra.HermOrSym)
    return TM(A), TM(_symherm_forward(A, ΔA))
end
function frule((_, ΔA), ::Type{Array}, A::LinearAlgebra.HermOrSym)
    return Array(A), Array(_symherm_forward(A, ΔA))
end

function rrule(TM::Type{<:Matrix}, A::LinearAlgebra.HermOrSym)
    function Matrix_pullback(Ω̄)
        ΔΩ = unthunk(Ω̄)
        TA = _symhermtype(A)
        T∂A = TA{eltype(ΔΩ),typeof(ΔΩ)}
        uplo = A.uplo
        ∂A = T∂A(_symherm_back(typeof(A), ΔΩ, Symbol(uplo)), uplo)
        return NoTangent(), ∂A
    end
    return TM(A), Matrix_pullback
end
rrule(::Type{Array}, A::LinearAlgebra.HermOrSym) = rrule(Matrix, A)

# for Ω = Matrix(A::HermOrSym), push forward ΔA to get ∂Ω
function _symherm_forward(A, Ȧ)
    ΔA = unthunk(Ȧ)
    TA = _symhermtype(A)
    return if typeof(ΔA) <: TA
        ΔA
    else
        TA{eltype(ΔA),typeof(ΔA)}(ΔA, A.uplo)
    end
end

# for Ω = HermOrSym(A, uplo), pull back ΔΩ to get ∂A
@inline function _symherm_back(::Type{T}, Ω̄, uplo::Symbol) where {T}
    ΔΩ = unthunk(Ω̄)
    if T <: Symmetric
        return _symmetric_back(ΔΩ, uplo)
    elseif T <: Hermitian
        if ΔΩ isa AbstractMatrix{<:Real}
            return _symmetric_back(ΔΩ, uplo)
        else
            return _hermitian_back(ΔΩ, uplo)
        end
    end
    return error()
end

@inline function _symmetric_back(Ω̄, uplo::Symbol)
    ΔΩ = unthunk(Ω̄)
    if ΔΩ isa Diagonal
        return ΔΩ
    elseif ΔΩ isa LinearAlgebra.AbstractTriangular
        if istriu(ΔΩ)
            return Matrix(uplo === :U ? ΔΩ : transpose(ΔΩ))
        else
            return Matrix(uplo === :U ? transpose(ΔΩ) : ΔΩ)
        end
    end
    L, U, D = LowerTriangular(ΔΩ), UpperTriangular(ΔΩ), Diagonal(ΔΩ)
    return Matrix(uplo === :U ? U .+ transpose(L) - D : L .+ transpose(U) - D)
end

@inline function _hermitian_back(ΔΩ, uplo::Symbol)
    if ΔΩ isa Diagonal
        return real.(ΔΩ)
    elseif ΔΩ isa LinearAlgebra.AbstractTriangular
        ∂UL = ΔΩ .- Diagonal(_extract_imag.(diag(ΔΩ)))
        if istriu(ΔΩ)
            return Matrix(uplo === :U ? ∂UL : ∂UL')
        else
            return Matrix(uplo === :U ? ∂UL' : ∂UL)
        end
    end
    L, U, rD = LowerTriangular(ΔΩ), UpperTriangular(ΔΩ), real.(Diagonal(ΔΩ))
    return Matrix(uplo === :U ? U .+ L' - rD : L .+ U' - rD)
end

#####
##### `eigen!`/`eigen`
#####

# rule is old but the usual references are
# real rules:
# Giles M. B., An extended collection of matrix derivative results for forward and reverse
# mode algorithmic differentiation.
# https://people.maths.ox.ac.uk/gilesm/files/NA-08-01.pdf.
# complex rules:
# Boeddeker C., Hanebrink P., et al, On the Computation of Complex-valued Gradients with
# Application to Statistically Optimum Beamforming. arXiv:1701.00392v2 [cs.NA]
#
# accounting for normalization convention appears in Boeddeker && Hanebrink.
# account for phase convention is unpublished.
function frule(
    (_, Ȧ),
    ::typeof(eigen!),
    A::LinearAlgebra.RealHermSymComplexHerm{<:BLAS.BlasReal,<:StridedMatrix};
    kwargs...,
)
    ΔA = unthunk(Ȧ)
    F = eigen!(A; kwargs...)
    ΔA isa AbstractZero && return F, ΔA
    λ, U = F.values, F.vectors
    tmp = U' * ΔA
    ∂K = mul!(ΔA.data, tmp, U)
    ∂Kdiag = @view ∂K[diagind(∂K)]
    ∂λ = real.(∂Kdiag)
    ∂K ./= λ' .- λ
    fill!(∂Kdiag, 0)
    ∂U = mul!(tmp, U, ∂K)
    _eigen_norm_phase_fwd!(∂U, A, U)
    ∂F = Tangent{typeof(F)}(; values=∂λ, vectors=∂U)
    return F, ∂F
end

function rrule(
    ::typeof(eigen),
    A::LinearAlgebra.RealHermSymComplexHerm{<:BLAS.BlasReal,<:StridedMatrix};
    kwargs...,
)
    F = eigen(A; kwargs...)
    function eigen_pullback(ΔF::Tangent)
        λ, U = F.values, F.vectors
        Δλ, ΔU = ΔF.values, ΔF.vectors
        ΔU = ΔU isa AbstractZero ? ΔU : copy(ΔU)
        ∂A = eigen_rev!(A, λ, U, Δλ, ΔU)
        return NoTangent(), ∂A
    end
    eigen_pullback(ΔF::AbstractZero) = (NoTangent(), ΔF)
    return F, eigen_pullback
end

# ∂U is overwritten if not an `AbstractZero`
function eigen_rev!(A::LinearAlgebra.RealHermSymComplexHerm, λ, U, ∂λ, ∂U)
    ∂λ isa AbstractZero && ∂U isa AbstractZero && return ∂λ + ∂U
    Ā = similar(parent(A), eltype(U))
    tmp = ∂U
    if ∂U isa AbstractZero
        mul!(Ā, U, real.(∂λ) .* U')
    else
        _eigen_norm_phase_rev!(∂U, A, U)
        ∂K = mul!(Ā, U', ∂U)
        ∂K ./= λ' .- λ
        ∂K[diagind(∂K)] .= real.(∂λ)
        mul!(tmp, ∂K, U')
        mul!(Ā, U, tmp)
    end
    ∂A = _hermitrizelike!(Ā, A)
    return ∂A
end

# NOTE: for small vₖ, the derivative of sign(vₖ) explodes, causing the tangents to become
# unstable even for phase-invariant programs. So for small vₖ we don't account for the phase
# in the gradient. Then derivatives are accurate for phase-invariant programs but inaccurate
# for phase-dependent programs that have low vₖ.

_eigen_norm_phase_fwd!(∂V, ::Union{Symmetric{T,S},Hermitian{T,S}}, V) where {T<:Real,S} = ∂V
function _eigen_norm_phase_fwd!(∂V, A::Hermitian{<:Complex}, V)
    k = A.uplo === 'U' ? size(A, 1) : 1
    ϵ = sqrt(eps(real(eltype(V))))
    @inbounds for i in axes(V, 2)
        v = @view V[:, i]
        vₖ = real(v[k])
        if abs(vₖ) > ϵ
            ∂v = @view ∂V[:, i]
            ∂v .-= v .* (im * (imag(∂v[k]) / vₖ))
        end
    end
    return ∂V
end

_eigen_norm_phase_rev!(∂V, ::Union{Symmetric{T,S},Hermitian{T,S}}, V) where {T<:Real,S} = ∂V
function _eigen_norm_phase_rev!(∂V, A::Hermitian{<:Complex}, V)
    k = A.uplo === 'U' ? size(A, 1) : 1
    ϵ = sqrt(eps(real(eltype(V))))
    @inbounds for i in axes(V, 2)
        v = @view V[:, i]
        vₖ = real(v[k])
        if abs(vₖ) > ϵ
            ∂v = @view ∂V[:, i]
            ∂c = dot(v, ∂v)
            ∂v[k] -= im * (imag(∂c) / vₖ)
        end
    end
    return ∂V
end

#####
##### `eigvals!`/`eigvals`
#####

function frule(
    (_, Ȧ),
    ::typeof(eigvals!),
    A::LinearAlgebra.RealHermSymComplexHerm{<:BLAS.BlasReal,<:StridedMatrix};
    kwargs...,
)
    ΔA = unthunk(Ȧ)
    ΔA isa AbstractZero && return eigvals!(A; kwargs...), ΔA
    F = eigen!(A; kwargs...)
    λ, U = F.values, F.vectors
    tmp = ΔA * U
    # diag(U' * tmp) without computing matrix product
    ∂λ = similar(λ)
    @inbounds for i in eachindex(λ)
        ∂λ[i] = @views real(dot(U[:, i], tmp[:, i]))
    end
    return λ, ∂λ
end

function rrule(
    ::typeof(eigvals),
    A::LinearAlgebra.RealHermSymComplexHerm{<:BLAS.BlasReal,<:StridedMatrix};
    kwargs...,
)
    F, eigen_back = rrule(eigen, A; kwargs...)
    λ = F.values
    function eigvals_pullback(Δλ)
        ∂F = Tangent{typeof(F)}(; values=Δλ)
        _, ∂A = eigen_back(∂F)
        return NoTangent(), ∂A
    end
    return λ, eigvals_pullback
end

#####
##### `svd`
#####

# NOTE: rrule defined because the `svd` primal mutates after calling `eigen`.
# otherwise, this rule just applies the chain rule and can be removed when mutation
# is supported by reverse-mode AD packages
function rrule(
    ::typeof(svd), A::LinearAlgebra.RealHermSymComplexHerm{<:BLAS.BlasReal,<:StridedMatrix}
)
    F = svd(A)
    function svd_pullback(ΔF::Tangent)
        U, V = F.U, F.V
        c = _svd_eigvals_sign!(similar(F.S), U, V)
        λ = F.S .* c
        ∂λ = ΔF.S isa AbstractZero ? ΔF.S : ΔF.S .* c
        if all(x -> x isa AbstractZero, (ΔF.U, ΔF.V, ΔF.Vt))
            ∂U = ΔF.U + ΔF.V + ΔF.Vt
        else
            ∂U = ΔF.U .+ (ΔF.V .+ ΔF.Vt') .* c'
        end
        ∂A = eigen_rev!(A, λ, U, ∂λ, ∂U)
        return NoTangent(), ∂A
    end
    svd_pullback(ΔF::AbstractZero) = (NoTangent(), ΔF)
    return F, svd_pullback
end

# given singular vectors, compute sign of eigenvalues corresponding to singular values
function _svd_eigvals_sign!(c, U, V)
    n = size(U, 1)
    @inbounds broadcast!(c, eachindex(c)) do i
        u = @views U[:, i]
        # find element not close to zero
        # at least one element satisfies abs2(x) ≥ 1/n > 1/(n + 1)
        k = findfirst(x -> (n + 1) * abs2(x) ≥ 1, u)
        return sign(real(u[k]) * real(V[k, i]))
    end
    return c
end

#####
##### `svdvals`
#####

# NOTE: rrule defined because `svdvals` calls mutating `svdvals!` internally.
# can be removed when mutation is supported by reverse-mode AD packages
function rrule(
    ::typeof(svdvals),
    A::LinearAlgebra.RealHermSymComplexHerm{<:BLAS.BlasReal,<:StridedMatrix},
)
    λ, back = rrule(eigvals, A)
    S = abs.(λ)
    p = sortperm(S; rev=true)
    permute!(S, p)
    function svdvals_pullback(ΔS)
        ∂λ = real.(ΔS)
        invpermute!(∂λ, p)
        ∂λ .*= sign.(λ)
        _, ∂A = back(∂λ)
        return NoTangent(), unthunk(∂A)
    end
    svdvals_pullback(ΔS::AbstractZero) = (NoTangent(), ΔS)
    return S, svdvals_pullback
end

#####
##### matrix functions
#####

# Formula for frule (Fréchet derivative) from Daleckiĭ-Kreĭn theorem given in Theorem 3.11 of
# Higham N.J. Functions of Matrices: Theory and Computation. 2008. ISBN: 978-0-898716-46-7.
# rrule is derived from frule. These rules are more stable for degenerate matrices than
# applying the chain rule to the rules for `eigen`.

for func in (
    :exp,
    :log,
    :sqrt,
    :cos,
    :sin,
    :tan,
    :cosh,
    :sinh,
    :tanh,
    :acos,
    :asin,
    :atan,
    :acosh,
    :asinh,
    :atanh,
)
    @eval begin
        function frule((_, Ȧ), ::typeof($func), A::LinearAlgebra.RealHermSymComplexHerm)
            ΔA = unthunk(Ȧ)
            Y, intermediates = _matfun($func, A)
            Ȳ = _matfun_frechet($func, ΔA, A, Y, intermediates)
            # If ΔA was hermitian, then ∂Y has the same structure as Y
            ∂Y = if ishermitian(ΔA) && (isa(Y, Symmetric) || isa(Y, Hermitian))
                _symhermlike!(Ȳ, Y)
            else
                Ȳ
            end
            return Y, ∂Y
        end

        function rrule(::typeof($func), A::LinearAlgebra.RealHermSymComplexHerm)
            Y, intermediates = _matfun($func, A)
            function $(Symbol(func, :_pullback))(ΔY)
                # for Hermitian Y, we don't need to realify the diagonal of ΔY, since the
                # effect is the same as applying _hermitrizelike! at the end
                ∂Y = eltype(Y) <: Real ? real(ΔY) : ΔY
                Ā = _matfun_frechet_adjoint($func, ∂Y, A, Y, intermediates)
                # the cotangent of Hermitian A should be Hermitian
                ∂A = _hermitrizelike!(Ā, A)
                return NoTangent(), ∂A
            end
            return Y, $(Symbol(func, :_pullback))
        end
    end
end

function frule((_, Ȧ), ::typeof(sincos), A::LinearAlgebra.RealHermSymComplexHerm)
    ΔA = unthunk(Ȧ)
    sinA, (λ, U, sinλ, cosλ) = _matfun(sin, A)
    cosA = _symhermtype(sinA)((U * Diagonal(cosλ)) * U')
    # We will overwrite tmp matrix several times to hold different values
    tmp = mul!(similar(U, Base.promote_eltype(ΔA, U)), ΔA, U)
    ∂Λ = mul!(similar(U), U', tmp)
    ∂sinΛ = _muldiffquotmat!!(similar(∂Λ), sin, λ, sinλ, cosλ, ∂Λ)
    ∂cosΛ = _muldiffquotmat!!(∂Λ, cos, λ, cosλ, -sinλ, ∂Λ)
    ∂sinA = _symhermlike!(mul!(∂sinΛ, U, mul!(tmp, ∂sinΛ, U')), sinA)
    ∂cosA = _symhermlike!(mul!(∂cosΛ, U, mul!(tmp, ∂cosΛ, U')), cosA)
    Y = (sinA, cosA)
    ∂Y = Tangent{typeof(Y)}(∂sinA, ∂cosA)
    return Y, ∂Y
end

function rrule(::typeof(sincos), A::LinearAlgebra.RealHermSymComplexHerm)
    sinA, (λ, U, sinλ, cosλ) = _matfun(sin, A)
    cosA = typeof(sinA)((U * Diagonal(cosλ)) * U', sinA.uplo)
    Y = (sinA, cosA)
    function sincos_pullback((ΔsinA, ΔcosA)::Tangent)
        ΔsinA isa AbstractZero &&
            ΔcosA isa AbstractZero &&
            return NoTangent(), ΔsinA + ΔcosA
        if eltype(A) <: Real
            ΔsinA, ΔcosA = real(ΔsinA), real(ΔcosA)
        end
        if ΔcosA isa AbstractZero
            Ā = _matfun_frechet_adjoint(sin, ΔsinA, A, sinA, (λ, U, sinλ, cosλ))
        elseif ΔsinA isa AbstractZero
            Ā = _matfun_frechet_adjoint(cos, ΔcosA, A, cosA, (λ, U, cosλ, -sinλ))
        else
            # we will overwrite tmp with various temporary values during this computation
            tmp = mul!(similar(U, Base.promote_eltype(U, ΔsinA, ΔcosA)), ΔsinA, U)
            ∂sinΛ = mul!(similar(tmp), U', tmp)
            ∂cosΛ = U' * mul!(tmp, ΔcosA, U)
            ∂Λ = _muldiffquotmat!!(∂sinΛ, sin, λ, sinλ, cosλ, ∂sinΛ)
            ∂Λ = _muldiffquotmat!!(∂Λ, cos, λ, cosλ, -sinλ, ∂cosΛ, true)
            Ā = mul!(∂Λ, U, mul!(tmp, ∂Λ, U'))
        end
        ∂A = _hermitrizelike!(Ā, A)
        return NoTangent(), ∂A
    end
    return Y, sincos_pullback
end

"""
    _matfun(f, A::LinearAlgebra.RealHermSymComplexHerm)

Compute the matrix function `f(A)` for real or complex hermitian `A`.
The function returns a tuple containing the result and a tuple of intermediates to be
reused by [`_matfun_frechet`](@ref) to compute the Fréchet derivative.

Note any function `f` used with this **must** have a `frule` defined on it.
"""
function _matfun(f, A::LinearAlgebra.RealHermSymComplexHerm)
    λ, U = eigen(A)
    if all(λi -> _isindomain(f, λi), λ)
        fλ_df_dλ = map(λi -> frule((ZeroTangent(), true), f, λi), λ)
    else  # promote to complex if necessary
        fλ_df_dλ = map(λi -> frule((ZeroTangent(), true), f, complex(λi)), λ)
    end
    fλ = first.(fλ_df_dλ)
    df_dλ = last.(unthunk.(fλ_df_dλ))
    fA = (U * Diagonal(fλ)) * U'
    Y = if eltype(A) <: Real
        Symmetric(fA)
    elseif eltype(fλ) <: Complex
        fA
    else
        Hermitian(fA)
    end
    intermediates = (λ, U, fλ, df_dλ)
    return Y, intermediates
end

# Computes ∂Y = U * (P .* (U' * ΔA * U)) * U' with fewer allocations
function _matfun_frechet(
    f, ΔA, A::LinearAlgebra.RealHermSymComplexHerm, Y, (λ, U, fλ, df_dλ)
)
    # We will overwrite tmp matrix several times to hold different values
    tmp = mul!(similar(U, Base.promote_eltype(U, ΔA)), ΔA, U)
    ∂Λ = mul!(similar(tmp), U', tmp)
    ∂fΛ = _muldiffquotmat!!(∂Λ, f, λ, fλ, df_dλ, ∂Λ)
    # reuse intermediate if possible
    if eltype(tmp) <: Real && eltype(∂fΛ) <: Complex
        tmp2 = ∂fΛ * U'
    else
        tmp2 = mul!(tmp, ∂fΛ, U')
    end
    ∂Y = mul!(∂fΛ, U, tmp2)
    return ∂Y
end

# difference quotient, i.e. Pᵢⱼ = (f(λⱼ) - f(λᵢ)) / (λⱼ - λᵢ), with f'(λᵢ) when λᵢ=λⱼ
function _diffquot(f, λi, λj, fλi, fλj, ∂fλi, ∂fλj)
    T = Base.promote_typeof(λi, λj, fλi, fλj, ∂fλi, ∂fλj)
    Δλ = λj - λi
    iszero(Δλ) && return T(∂fλi)
    # handle round-off error using Maclaurin series of (f(λᵢ + Δλ) - f(λᵢ)) / Δλ wrt Δλ
    # and approximating f''(λᵢ) with forward difference (f'(λᵢ + Δλ) - f'(λᵢ)) / Δλ
    # so (f(λᵢ + Δλ) - f(λᵢ)) / Δλ = (f'(λᵢ + Δλ) + f'(λᵢ)) / 2 + O(Δλ^2)
    # total error on the order of f(λᵢ) * eps()^(2/3)
    abs(Δλ) < cbrt(eps(real(T))) && return T((∂fλj + ∂fλi) / 2)
    Δfλ = fλj - fλi
    return T(Δfλ / Δλ)
end

# broadcast multiply Δ by the matrix of difference quotients P, storing the result in PΔ.
# If β is is nonzero, then @. PΔ = β*PΔ + P*Δ
# if type of PΔ is incompatible with result, new matrix is allocated
function _muldiffquotmat!!(PΔ, f, λ, fλ, ∂fλ, Δ, β=false)
    if eltype(PΔ) <: Real && eltype(fλ) <: Complex
        PΔ2 = similar(PΔ, complex(eltype(PΔ)))
        return _muldiffquotmat!!(PΔ2, f, λ, fλ, ∂fλ, Δ, β)
    else
        PΔ .= β .* PΔ .+ _diffquot.(f, λ, λ', fλ, transpose(fλ), ∂fλ, transpose(∂fλ)) .* Δ
        return PΔ
    end
end

_isindomain(f, x) = true
_isindomain(::Union{typeof(acos),typeof(asin)}, x::Real) = -1 ≤ x ≤ 1
_isindomain(::typeof(acosh), x::Real) = x ≥ 1
_isindomain(::Union{typeof(log),typeof(sqrt)}, x::Real) = x ≥ 0

#####
##### utilities
#####

# Get type (Symmetric or Hermitian) from type or matrix
_symhermtype(::Type{<:Symmetric}) = Symmetric
_symhermtype(::Type{<:Hermitian}) = Hermitian
_symhermtype(A) = _symhermtype(typeof(A))
_symhermtype(a::AbstractThunk) = _symhermtype(unthunk(a))

function _realifydiag!(A)
    for i in diagind(A)
        @inbounds A[i] = real(A[i])
    end
    return A
end

function _symhermlike!(A, S::Union{Symmetric,Hermitian})
    A isa Hermitian{<:Complex} && _realifydiag!(A)
    return typeof(S)(A, S.uplo)
end

# in-place hermitrize matrix
function _hermitrizelike!(A_, S::LinearAlgebra.RealHermSymComplexHerm)
    A = eltype(S) <: Real ? real(A_) : A_
    n = size(A, 1)
    for i in 1:n
        for j in (i + 1):n
            A[i, j] = (A[i, j] + conj(A[j, i])) / 2
            A[j, i] = conj(A[i, j])
        end
        A[i, i] = real(A[i, i])
    end
    return _symhermtype(S)(A, Symbol(S.uplo))
end
