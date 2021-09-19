# Structured matrices
using LinearAlgebra: AbstractTriangular

# Matrix wrapper types that we know are square and are thus potentially invertible. For
# these we can use simpler definitions for `/` and `\`.
const SquareMatrix{T} = Union{Diagonal{T},AbstractTriangular{T}}

function rrule(::typeof(/), A::AbstractMatrix{<:Real}, B::T) where {T<:SquareMatrix{<:Real}}
    Y = A / B
    project_A = ProjectTo(A)
    project_B = ProjectTo(B)
    function slash_pullback(ȳ)
        Ȳ = unthunk(ȳ)
        ∂A = @thunk project_A(Ȳ / B')
        ∂B = @thunk project_B(-Y' * (Ȳ / B'))
        return (NoTangent(), ∂A, ∂B)
    end
    return Y, slash_pullback
end

function rrule(
    ::typeof(\), A::T, B::AbstractVecOrMat{<:Real}
) where {T<:SquareMatrix{<:Real}}
    Y = A \ B
    project_A = ProjectTo(A)
    project_B = ProjectTo(B)
    function backslash_pullback(ȳ)
        Ȳ = unthunk(ȳ)
        ∂A = @thunk project_A(-(A' \ Ȳ) * Y')
        ∂B = @thunk project_B(A' \ Ȳ)
        return NoTangent(), ∂A, ∂B
    end
    return Y, backslash_pullback
end

#####
##### `Diagonal`
#####

_diagview(x::Diagonal) = x.diag
_diagview(x::AbstractMatrix) = view(x, diagind(x))
_diagview(x::Tangent{<:Diagonal}) = x.diag
function ChainRulesCore.rrule(::typeof(sqrt), d::Diagonal)
    y = sqrt(d)
    @assert y isa Diagonal
    function sqrt_pullback(Δ)
        Δ_diag = _diagview(unthunk(Δ))
        return NoTangent(), Diagonal(Δ_diag ./ (2 .* y.diag))
    end
    return y, sqrt_pullback
end

# these functions are defined outside the rrule because otherwise type inference breaks
# see https://github.com/JuliaLang/julia/issues/40990
_Diagonal_pullback(ȳ::AbstractMatrix) = return (NoTangent(), diag(ȳ)) # should we emit a warning here? this shouldn't be called if project works right
_Diagonal_pullback(ȳ::Diagonal) = return (NoTangent(), diag(ȳ))
function _Diagonal_pullback(ȳ::Tangent)
    # TODO: Assert about the primal type in the Tangent, It should be Diagonal
    # infact it should be exactly the type of `Diagonal(d)`
    # but right now Zygote loses primal type information so we can't use it.
    # See https://github.com/FluxML/Zygote.jl/issues/603
    return (NoTangent(), ȳ.diag)
end
_Diagonal_pullback(ȳ::AbstractThunk) = return _Diagonal_pullback(unthunk(ȳ))

function rrule(::Type{<:Diagonal}, d::AbstractVector)
    return Diagonal(d), _Diagonal_pullback
end

function rrule(::typeof(diag), A::AbstractMatrix)
    function diag_pullback(ȳ)
        return (NoTangent(), Diagonal(ȳ))
    end
    return diag(A), diag_pullback
end
if VERSION ≥ v"1.3"
    function rrule(::typeof(diag), A::AbstractMatrix, k::Integer)
        function diag_pullback(ȳ)
            return (NoTangent(), diagm(size(A)..., k => ȳ), NoTangent())
        end
        return diag(A, k), diag_pullback
    end

    function rrule(
        ::typeof(diagm), m::Integer, n::Integer, kv::Pair{<:Integer,<:AbstractVector}...
    )
        function diagm_pullback(ȳ)
            return (NoTangent(), NoTangent(), NoTangent(), _diagm_back.(kv, Ref(ȳ))...)
        end
        return diagm(m, n, kv...), diagm_pullback
    end
end
function rrule(::typeof(diagm), kv::Pair{<:Integer,<:AbstractVector}...)
    function diagm_pullback(ȳ)
        return (NoTangent(), _diagm_back.(kv, Ref(ȳ))...)
    end
    return diagm(kv...), diagm_pullback
end

function _diagm_back(p, ȳ)
    k, v = p
    d = diag(unthunk(ȳ), k)[1:length(v)] # handle if diagonal was smaller than matrix
    return Tangent{typeof(p)}(; second=d)
end

function rrule(::typeof(*), D::Diagonal{<:Real}, V::AbstractVector{<:Real})
    project_D = ProjectTo(D)
    project_V = ProjectTo(V)
    function times_pullback(ȳ)
        Ȳ = unthunk(ȳ)
        dD = @thunk(project_D(Diagonal(Ȳ .* V)))
        dV = @thunk(project_V(D * Ȳ))
        return (NoTangent(), dD, dV)
    end
    return D * V, times_pullback
end

#####
##### `Adjoint`
#####

# these functions are defined outside the rrule because otherwise type inference breaks
# see https://github.com/JuliaLang/julia/issues/40990
_Adjoint_mat_pullback(ȳ::Tangent, proj) = (NoTangent(), proj(ȳ.parent))
_Adjoint_mat_pullback(ȳ::AbstractVecOrMat, proj) = (NoTangent(), proj(adjoint(ȳ)))
function _Adjoint_mat_pullback(ȳ::AbstractThunk, proj)
    return _Adjoint_mat_pullback(unthunk(ȳ), proj)
end
# currently needed by Diffractor (ref https://github.com/JuliaDiff/Diffractor.jl/issues/25)
_Adjoint_mat_pullback(ȳ::AbstractZero, proj) = (NoTangent(), proj(ȳ))
function rrule(::Type{<:Adjoint}, A::AbstractMatrix{<:Number})
    project_A = ProjectTo(A)
    Adjoint_mat_pullback(ȳ) = _Adjoint_mat_pullback(ȳ, project_A)
    return Adjoint(A), Adjoint_mat_pullback
end

_Adjoint_vec_pullback(ȳ::Tangent) = (NoTangent(), vec(ȳ.parent))
_Adjoint_vec_pullback(ȳ::AbstractMatrix) = (NoTangent(), vec(adjoint(ȳ)))
_Adjoint_vec_pullback(ȳ::AbstractThunk) = return _Adjoint_vec_pullback(unthunk(ȳ))
# currently needed by Diffractor (ref https://github.com/JuliaDiff/Diffractor.jl/issues/25)
_Adjoint_vec_pullback(ȳ::AbstractZero) = (NoTangent(), ȳ)
function rrule(::Type{<:Adjoint}, A::AbstractVector{<:Number})
    return Adjoint(A), _Adjoint_vec_pullback
end

_adjoint_mat_pullback(ȳ::Tangent, proj) = (NoTangent(), proj(ȳ.parent))
_adjoint_mat_pullback(ȳ::AbstractVecOrMat, proj) = (NoTangent(), proj(adjoint(ȳ)))
function _adjoint_mat_pullback(ȳ::AbstractThunk, proj)
    return _adjoint_mat_pullback(unthunk(ȳ), proj)
end
# currently needed by Diffractor (ref https://github.com/JuliaDiff/Diffractor.jl/issues/25)
_adjoint_mat_pullback(ȳ::AbstractZero, proj) = (NoTangent(), proj(ȳ))
function rrule(::typeof(adjoint), A::AbstractMatrix{<:Number})
    project_A = ProjectTo(A)
    adjoint_mat_pullback(ȳ) = _adjoint_mat_pullback(ȳ, project_A)
    return adjoint(A), adjoint_mat_pullback
end

_adjoint_vec_pullback(ȳ::Tangent) = (NoTangent(), vec(ȳ.parent))
_adjoint_vec_pullback(ȳ::AbstractMatrix) = (NoTangent(), vec(adjoint(ȳ)))
_adjoint_vec_pullback(ȳ::AbstractThunk) = return _adjoint_vec_pullback(unthunk(ȳ))
# currently needed by Diffractor (ref https://github.com/JuliaDiff/Diffractor.jl/issues/25)
_adjoint_vec_pullback(ȳ::AbstractZero) = (NoTangent(), ȳ)
function rrule(::typeof(adjoint), A::AbstractVector{<:Number})
    return adjoint(A), _adjoint_vec_pullback
end

#####
##### `Transpose`
#####

# these functions are defined outside the rrule because otherwise type inference breaks
# see https://github.com/JuliaLang/julia/issues/40990
_Transpose_mat_pullback(ȳ::Tangent, proj) = (NoTangent(), proj(ȳ.parent))
_Transpose_mat_pullback(ȳ::AbstractVecOrMat, proj) = (NoTangent(), proj(Transpose(ȳ)))
function _Transpose_mat_pullback(ȳ::AbstractThunk, proj)
    return _Transpose_mat_pullback(unthunk(ȳ), proj)
end
# currently needed by Diffractor (ref https://github.com/JuliaDiff/Diffractor.jl/issues/25)
_Transpose_mat_pullback(ȳ::AbstractZero, proj) = (NoTangent(), proj(ȳ))
function rrule(::Type{<:Transpose}, A::AbstractMatrix{<:Number})
    project_A = ProjectTo(A)
    Transpose_mat_pullback(ȳ) = _Transpose_mat_pullback(ȳ, project_A)
    return Transpose(A), Transpose_mat_pullback
end

_Transpose_vec_pullback(ȳ::Tangent) = (NoTangent(), vec(ȳ.parent))
_Transpose_vec_pullback(ȳ::AbstractMatrix) = (NoTangent(), vec(Transpose(ȳ)))
_Transpose_vec_pullback(ȳ::AbstractThunk) = return _Transpose_vec_pullback(unthunk(ȳ))
# currently needed by Diffractor (ref https://github.com/JuliaDiff/Diffractor.jl/issues/25)
_Transpose_vec_pullback(ȳ::AbstractZero) = (NoTangent(), ȳ)
function rrule(::Type{<:Transpose}, A::AbstractVector{<:Number})
    return Transpose(A), _Transpose_vec_pullback
end

_transpose_mat_pullback(ȳ::Tangent, proj) = (NoTangent(), proj(ȳ.parent))
_transpose_mat_pullback(ȳ::AbstractVecOrMat, proj) = (NoTangent(), proj(transpose(ȳ)))
function _transpose_mat_pullback(ȳ::AbstractThunk, proj)
    return _transpose_mat_pullback(unthunk(ȳ), proj)
end
# currently needed by Diffractor (ref https://github.com/JuliaDiff/Diffractor.jl/issues/25)
_transpose_mat_pullback(ȳ::AbstractZero, proj) = (NoTangent(), proj(ȳ))
function rrule(::typeof(transpose), A::AbstractMatrix{<:Number})
    project_A = ProjectTo(A)
    transpose_mat_pullback(ȳ) = _transpose_mat_pullback(ȳ, project_A)
    return transpose(A), transpose_mat_pullback
end

_transpose_vec_pullback(ȳ::Tangent) = (NoTangent(), vec(ȳ.parent))
_transpose_vec_pullback(ȳ::AbstractMatrix) = (NoTangent(), vec(transpose(ȳ)))
_transpose_vec_pullback(ȳ::AbstractThunk) = return _transpose_vec_pullback(unthunk(ȳ))
# currently needed by Diffractor (ref https://github.com/JuliaDiff/Diffractor.jl/issues/25)
_transpose_vec_pullback(ȳ::AbstractZero) = (NoTangent(), ȳ)
function rrule(::typeof(transpose), A::AbstractVector{<:Number})
    return transpose(A), _transpose_vec_pullback
end

#####
##### Triangular matrices
#####

function rrule(::Type{<:UpperTriangular}, A::AbstractMatrix)
    function UpperTriangular_pullback(ȳ)
        return (NoTangent(), Matrix(ȳ))
    end
    return UpperTriangular(A), UpperTriangular_pullback
end

function rrule(::Type{<:LowerTriangular}, A::AbstractMatrix)
    function LowerTriangular_pullback(ȳ)
        return (NoTangent(), Matrix(ȳ))
    end
    return LowerTriangular(A), LowerTriangular_pullback
end

function rrule(::typeof(triu), A::AbstractMatrix, k::Integer)
    function triu_pullback(ȳ)
        return (NoTangent(), triu(ȳ, k), NoTangent())
    end
    return triu(A, k), triu_pullback
end
function rrule(::typeof(triu), A::AbstractMatrix)
    function triu_pullback(ȳ)
        return (NoTangent(), triu(ȳ))
    end
    return triu(A), triu_pullback
end

function rrule(::typeof(tril), A::AbstractMatrix, k::Integer)
    function tril_pullback(ȳ)
        return (NoTangent(), tril(ȳ, k), NoTangent())
    end
    return tril(A, k), tril_pullback
end
function rrule(::typeof(tril), A::AbstractMatrix)
    function tril_pullback(ȳ)
        return (NoTangent(), tril(ȳ))
    end
    return tril(A), tril_pullback
end

_diag_view(X) = view(X, diagind(X))
_diag_view(X::Diagonal) = parent(X)  #Diagonal wraps a Vector of just Diagonal elements

function rrule(::typeof(det), X::Union{Diagonal,AbstractTriangular})
    y = det(X)
    s = conj!(y ./ _diag_view(X))
    function det_pullback(ȳ)
        return (NoTangent(), Diagonal(ȳ .* s))
    end
    return y, det_pullback
end

function rrule(::typeof(logdet), X::Union{Diagonal,AbstractTriangular})
    y = logdet(X)
    s = conj!(one(eltype(X)) ./ _diag_view(X))
    function logdet_pullback(ȳ)
        return (NoTangent(), Diagonal(ȳ .* s))
    end
    return y, logdet_pullback
end
