# Some utility functions for optimizing linear algebra operations that aren't specific
# to any particular rule definition

# F .* (X - X'), overwrites X if possible
function _mulsubtrans!!(X::AbstractMatrix{<:Real}, F::AbstractMatrix{<:Real})
    T = promote_type(eltype(X), eltype(F))
    Y = (T <: eltype(X)) ? X : similar(X, T)
    k = size(X, 1)
    @inbounds for j in 1:k, i in 1:j  # Iterate the upper triangle
        if i == j
            Y[i, i] = zero(T)
        else
            Y[i, j], Y[j, i] = F[i, j] * (X[i, j] - X[j, i]), F[j, i] * (X[j, i] - X[i, j])
        end
    end
    return Y
end
_mulsubtrans!!(X::AbstractZero, F::AbstractZero) = X
_mulsubtrans!!(X::AbstractZero, F::AbstractMatrix{<:Real}) = X
_mulsubtrans!!(X::AbstractMatrix{<:Real}, F::AbstractZero) = F

# I - X, overwrites X
function _eyesubx!(X::AbstractMatrix)
    n, m = size(X)
    @inbounds for j in 1:m, i in 1:n
        X[i, j] = (i == j) - X[i, j]
    end
    return X
end

_extract_imag(x) = complex(0, imag(x))

"""
    WithSomeZeros{T}

This is a union of LinearAlgebra types, all of which are partly structral zeros,
with a simple backing array given by `parent(x)`. All have methods of `_rewrap`
to re-create.

This exists to solve a type instability, as broadcasting for instance
`λ .* Diagonal(rand(3))` gives a dense matrix when `x==Inf`.
But `withsomezeros_rewrap(x, λ .* parent(x))` is type-stable.
"""
WithSomeZeros{T} = Union{
    Diagonal{T},
    UpperTriangular{T},
    UnitUpperTriangular{T},
    # UpperHessenberg{T},  # doesn't exist in Julia 1.0
    LowerTriangular{T},
    UnitLowerTriangular{T},
}
for S in [
    :Diagonal,
    :UpperTriangular,
    :UnitUpperTriangular,
    # :UpperHessenberg,
    :LowerTriangular,
    :UnitLowerTriangular,
]
    @eval withsomezeros_rewrap(::$S, x) = $S(x)
end

# Bidiagonal, Tridiagonal have more complicated storage.
# AdjOrTransUpperOrUnitUpperTriangular would need adjoint(parent(parent()))
