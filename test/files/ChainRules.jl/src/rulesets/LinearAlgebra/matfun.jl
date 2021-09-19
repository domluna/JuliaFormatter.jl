# matrix functions of dense matrices
# https://en.wikipedia.org/wiki/Matrix_function

# NOTE: for a matrix function f, the pushforward and pullback can be computed using the
# Fréchet derivative and its adjoint, respectively.
# https://en.wikipedia.org/wiki/Fréchet_derivative

# The pushforwards and pullbacks are related by matrix adjoints. If the pushforward of f(A)
# at A is (f_*)_A(ΔA), then the pullback at A is (f^*)_A(ΔY) = ((f_*)_A(ΔY'))'.
# If f has a power series representation with real coefficients, then this simplifies to
# (f^*)_Y(ΔY) = (f_*)_{A'}(ΔY)
# So we reuse the code from the pushforward to implement the pullback.

#####
##### interface function definitions
#####

"""
    _matfun(f, A) -> (Y, intermediates)

Compute the matrix function `Y=f(A)` for matrix `A`.
The function returns a tuple containing the result and a tuple of intermediates to be
reused by [`_matfun_frechet`](@ref) to compute the Fréchet derivative.
"""
_matfun

"""
    _matfun!(f, A) -> (Y, intermediates)

Similar to [`_matfun`](@ref), but where `A` may be overwritten.
"""
_matfun!

"""
    _matfun_frechet(f, E, A, Y, intermediates)

Compute the Fréchet derivative of the matrix function ``Y = f(A)`` at ``A`` in the direction
of ``E``, where `intermediates` is the second argument returned by [`_matfun`](@ref).

The Fréchet derivative is the unique linear map ``L_f \\colon E → L_f(A, E)``, such that
```math
L_f(A, E) = f(A + E) - f(A) + o(\\lVert E \\rVert).
```

[^Higham08]:
    > Higham, Nicholas J. Chapter 3: Conditioning. Functions of Matrices. 2008, 55-70.
    > doi: 10.1137/1.9780898717778.ch3
"""
_matfun_frechet

"""
    _matfun_frechet!(f, E, A, Y, intermediates)

Similar to [`_matfun_frechet`](@ref), but where `E` may be overwritten.
"""
_matfun_frechet!

"""
    _matfun_frechet_adjoint(f, E, A, Y, intermediates)

Compute the adjoint of the Fréchet derivative of the matrix function ``Y = f(A)`` at ``A``
in the direction of ``E``, where `intermediates` is the second argument returned by
[`_matfun`](@ref).

Given the Fréchet ``L_f(A, E)`` computed by [`_matfun_frechet`](@ref), then its adjoint
``L_f^⋆(A, E)`` is defined by the identity
```math
\\langle B, L_f(A, C) \\rangle = \\langle L_f^⋆(A, B), C \\rangle.
```
This identity is satisfied by ``L_f^⋆(A, E) = L_f(A, E')'``.

[^Higham08]:
    > Higham, Nicholas J. Chapter 3: Conditioning. Functions of Matrices. 2008, 55-70.
    > doi: 10.1137/1.9780898717778.ch3
"""
function _matfun_frechet_adjoint(f, E, A, Y, intermediates)
    E′ = E'
    # avoid passing an Adjoint to _matfun_frechet in case it can't handle it
    E′ = E′ isa Adjoint ? copy(E′) : E′
    LE = adjoint(_matfun_frechet(f, E′, A, Y, intermediates))
    # avoid returning an Adjoint
    return LE isa Adjoint ? copy(LE) : LE
end

"""
    _matfun_frechet_adjoint!(f, E, A, Y, intermediates)

Similar to [`_matfun_frechet_adjoint`](@ref), but where `E` may be overwritten.
"""
function _matfun_frechet_adjoint!(f, E, A, Y, intermediates)
    E′ = E'
    # avoid passing an Adjoint to _matfun_frechet in case it can't handle it
    E′ = E′ isa Adjoint ? copy(E′) : E′
    LE = adjoint(_matfun_frechet!(f, E′, A, Y, intermediates))
    # avoid returning an Adjoint
    return LE isa Adjoint ? copy(LE) : LE
end

#####
##### `exp`/`exp!`
#####

function frule((_, ΔA), ::typeof(LinearAlgebra.exp!), A::StridedMatrix{<:BlasFloat})
    if ishermitian(A)
        hermA = Hermitian(A)
        hermX, intermediates = _matfun(exp, hermA)
        ∂hermX = _matfun_frechet(exp, ΔA, hermA, hermX, intermediates)
        X = Matrix(hermX)
        ∂X = Matrix(∂hermX)
    else
        X, intermediates = _matfun!(exp, A)
        ∂X = _matfun_frechet!(exp, ΔA, A, X, intermediates)
    end
    return X, ∂X
end

function rrule(::typeof(exp), A0::StridedMatrix{<:BlasFloat})
    # TODO: try to make this more type-stable
    if ishermitian(A0)
        # call _matfun instead of the rrule to avoid hermitrizing ∂A in the pullback
        hermA = Hermitian(A0)
        hermX, hermX_intermediates = _matfun(exp, hermA)
        function exp_pullback_hermitian(ΔX)
            ∂hermA = _matfun_frechet_adjoint(exp, ΔX, hermA, hermX, hermX_intermediates)
            return NoTangent(), Matrix(∂hermA)
        end
        return Matrix(hermX), exp_pullback_hermitian
    else
        A = copy(A0)
        X, intermediates = _matfun!(exp, A)
        function exp_pullback(X̄)
            # Ensures ∂X is mutable. The outer `adjoint` is unwrapped without copy by
            # the default _matfun_frechet_adjoint!
            ΔX = unthunk(X̄)
            ∂X = ChainRulesCore.is_inplaceable_destination(ΔX) ? ΔX : convert(Matrix, ΔX')'
            ∂A = _matfun_frechet_adjoint!(exp, ∂X, A, X, intermediates)
            return NoTangent(), ∂A
        end
        return X, exp_pullback
    end
end

## Destructive matrix exponential using algorithm from Higham, 2008,
## "Functions of Matrices: Theory and Computation", SIAM
## Adapted from LinearAlgebra.exp! with return of intermediates
## https://github.com/JuliaLang/julia/blob/f613b55/stdlib/LinearAlgebra/src/dense.jl#L583-L666
function _matfun!(::typeof(exp), A::StridedMatrix{T}) where {T<:BlasFloat}
    n = LinearAlgebra.checksquare(A)
    ilo, ihi, scale = LAPACK.gebal!('B', A)  # modifies A
    nA = opnorm(A, 1)
    Inn = Matrix{T}(I, n, n)
    ## For sufficiently small nA, use lower order Padé-Approximations
    if (nA <= 2.1)
        if nA > 0.95
            C = T[
                17643225600.0,
                8821612800.0,
                2075673600.0,
                302702400.0,
                30270240.0,
                2162160.0,
                110880.0,
                3960.0,
                90.0,
                1.0,
            ]
        elseif nA > 0.25
            C = T[17297280.0, 8648640.0, 1995840.0, 277200.0, 25200.0, 1512.0, 56.0, 1.0]
        elseif nA > 0.015
            C = T[30240.0, 15120.0, 3360.0, 420.0, 30.0, 1.0]
        else
            C = T[120.0, 60.0, 12.0, 1.0]
        end
        si = 0
    else
        C = T[
            64764752532480000.0,
            32382376266240000.0,
            7771770303897600.0,
            1187353796428800.0,
            129060195264000.0,
            10559470521600.0,
            670442572800.0,
            33522128640.0,
            1323241920.0,
            40840800.0,
            960960.0,
            16380.0,
            182.0,
            1.0,
        ]
        s = log2(nA / 5.4)  # power of 2 later reversed by squaring
        si = ceil(Int, s)
    end

    if si > 0
        A ./= convert(T, 2^si)
    end

    A2 = A * A
    P = copy(Inn)
    W = C[2] * P
    V = C[1] * P
    Apows = typeof(P)[]
    for k in 1:(div(size(C, 1), 2) - 1)
        k2 = 2 * k
        P *= A2
        push!(Apows, P)
        W += C[k2 + 2] * P
        V += C[k2 + 1] * P
    end
    U = A * W
    X = V + U
    F = lu!(V - U)  # NOTE: use lu! instead of LAPACK.gesv! so we can reuse factorization
    ldiv!(F, X)
    Xpows = typeof(X)[X]
    if si > 0  # squaring to reverse dividing by power of 2
        for t in 1:si
            X *= X
            push!(Xpows, X)
        end
    end

    _unbalance!(X, ilo, ihi, scale, n)
    return X, (ilo, ihi, scale, C, si, Apows, W, F, Xpows)
end

# Application of the chain rule to exp!, also Algorithm 6.4 from
# Al-Mohy, Awad H. and Higham, Nicholas J. (2009).
# Computing the Fréchet Derivative of the Matrix Exponential, with an application to
# Condition Number Estimation", SIAM. 30 (4). pp. 1639-1657.
# http://eprints.maths.manchester.ac.uk/id/eprint/1218
function _matfun_frechet!(
    ::typeof(exp), ΔA, A::StridedMatrix{T}, X, (ilo, ihi, scale, C, si, Apows, W, F, Xpows)
) where {T<:BlasFloat}
    n = LinearAlgebra.checksquare(A)
    _balance!(ΔA, ilo, ihi, scale, n)

    if si > 0
        ΔA ./= convert(T, 2^si)
    end

    ∂A2 = mul!(A * ΔA, ΔA, A, true, true)
    A2 = first(Apows)
    # we will repeatedly overwrite ∂temp and ∂P below
    ∂temp = Matrix{eltype(∂A2)}(undef, n, n)
    ∂P = copy(∂A2)
    ∂W = C[4] * ∂P
    ∂V = C[3] * ∂P
    for k in 2:(length(Apows) - 1)
        k2 = 2 * k
        P = Apows[k - 1]
        ∂P, ∂temp = mul!(mul!(∂temp, ∂P, A2), P, ∂A2, true, true), ∂P
        axpy!(C[k2 + 2], ∂P, ∂W)
        axpy!(C[k2 + 1], ∂P, ∂V)
    end
    ∂U, ∂temp = mul!(mul!(∂temp, A, ∂W), ΔA, W, true, true), ∂W
    ∂temp .= ∂U .- ∂V
    ∂X = add!!(∂U, ∂V)
    mul!(∂X, ∂temp, first(Xpows), true, true)
    ldiv!(F, ∂X)

    if si > 0
        for t in 1:(length(Xpows) - 1)
            X = Xpows[t]
            ∂X, ∂temp = mul!(mul!(∂temp, X, ∂X), ∂X, X, true, true), ∂X
        end
    end

    _unbalance!(∂X, ilo, ihi, scale, n)
    return ∂X
end
function _matfun_frechet!(
    f::typeof(exp), a::AbstractThunk, A::StridedMatrix{T}, X, t
) where {T<:BlasFloat}
    return _matfun_frechet!(f, unthunk(a), A, X, t)
end

#####
##### utils
#####

# Given (ilo, ihi, iscale) returned by LAPACK.gebal!('B', A), apply same balancing to X
function _balance!(X, ilo, ihi, scale, n)
    n = size(X, 1)
    if ihi < n
        for j in (ihi + 1):n
            LinearAlgebra.rcswap!(j, Int(scale[j]), X)
        end
    end
    if ilo > 1
        for j in (ilo - 1):-1:1
            LinearAlgebra.rcswap!(j, Int(scale[j]), X)
        end
    end

    for j in ilo:ihi
        scj = scale[j]
        for i in 1:n
            X[j, i] /= scj
        end
        for i in 1:n
            X[i, j] *= scj
        end
    end
    return X
end

# Reverse of _balance!
function _unbalance!(X, ilo, ihi, scale, n)
    for j in ilo:ihi
        scj = scale[j]
        for i in 1:n
            X[j, i] *= scj
        end
        for i in 1:n
            X[i, j] /= scj
        end
    end

    if ilo > 1
        for j in (ilo - 1):-1:1
            LinearAlgebra.rcswap!(j, Int(scale[j]), X)
        end
    end
    if ihi < n
        for j in (ihi + 1):n
            LinearAlgebra.rcswap!(j, Int(scale[j]), X)
        end
    end
    return X
end
