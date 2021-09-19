# See also fastmath_able.jl for where rules are defined simple base functions
# that also have FastMath versions.

@scalar_rule copysign(y, x) (
    ifelse(signbit(x) != signbit(y), -one(y), +one(y)), NoTangent()
)

@scalar_rule one(x) zero(x)
@scalar_rule zero(x) zero(x)
@scalar_rule transpose(x) true

# `adjoint`

frule((_, Δz), ::typeof(adjoint), z::Number) = (z', Δz')

function rrule(::typeof(adjoint), z::Number)
    adjoint_pullback(ΔΩ) = (NoTangent(), ΔΩ')
    return (z', adjoint_pullback)
end

# `real`

@scalar_rule real(x::Real) true

frule((_, Δz), ::typeof(real), z::Number) = (real(z), real(Δz))

function rrule(::typeof(real), z::Number)
    # add zero(z) to embed the real number in the same number type as z
    real_pullback(ΔΩ) = (NoTangent(), real(ΔΩ) + zero(z))
    return (real(z), real_pullback)
end

# Conversions to Float

@scalar_rule float(x) true
@scalar_rule Float64(x::Real) true
@scalar_rule Float32(x::Real) true
@scalar_rule AbstractFloat(x::Real) true

# `imag`

@scalar_rule imag(x::Real) ZeroTangent()

frule((_, Δz), ::typeof(imag), z::Complex) = (imag(z), imag(Δz))

function rrule(::typeof(imag), z::Complex)
    imag_pullback(ΔΩ) = (NoTangent(), real(ΔΩ) * im)
    return (imag(z), imag_pullback)
end

# `Complex`

frule((_, Δz), ::Type{T}, z::Number) where {T<:Complex} = (T(z), Complex(Δz))
function frule((_, Δx, Δy), ::Type{T}, x::Number, y::Number) where {T<:Complex}
    return (T(x, y), Complex(Δx, Δy))
end

function rrule(::Type{T}, z::Complex) where {T<:Complex}
    Complex_pullback(ΔΩ) = (NoTangent(), Complex(ΔΩ))
    return (T(z), Complex_pullback)
end
function rrule(::Type{T}, x::Real) where {T<:Complex}
    Complex_pullback(ΔΩ) = (NoTangent(), real(ΔΩ))
    return (T(x), Complex_pullback)
end
function rrule(::Type{T}, x::Number, y::Number) where {T<:Complex}
    project_x = ProjectTo(x)
    project_y = ProjectTo(y)

    function Complex_pullback(Ω̄)
        ΔΩ = unthunk(Ω̄)
        return (NoTangent(), project_x(real(ΔΩ)), project_y(imag(ΔΩ)))
    end
    return (T(x, y), Complex_pullback)
end

# `hypot`

@scalar_rule hypot(x::Real) sign(x)

function frule((_, Δz), ::typeof(hypot), z::Complex)
    Ω = hypot(z)
    ∂Ω = _realconjtimes(z, Δz) / ifelse(iszero(Ω), one(Ω), Ω)
    return Ω, ∂Ω
end

function rrule(::typeof(hypot), z::Complex)
    Ω = hypot(z)
    function hypot_pullback(ΔΩ)
        return (NoTangent(), (real(ΔΩ) / ifelse(iszero(Ω), one(Ω), Ω)) * z)
    end
    return (Ω, hypot_pullback)
end

@scalar_rule fma(x, y, z) (y, x, true)
@scalar_rule muladd(x, y, z) (y, x, true)
@scalar_rule rem2pi(x, r::RoundingMode) (true, NoTangent())
@scalar_rule(
    mod(x, y),
    @setup((u, nan) = promote(x / y, NaN16), isint = isinteger(x / y)),
    (ifelse(isint, nan, one(u)), ifelse(isint, nan, -floor(u))),
)

@scalar_rule deg2rad(x) π / oftype(x, 180)
@scalar_rule rad2deg(x) oftype(x, 180) / π

@scalar_rule(ldexp(x, y), (2^y, NoTangent()))

# Can't multiply though sqrt in acosh because of negative complex case for x
@scalar_rule acosh(x) inv(sqrt(x - 1) * sqrt(x + 1))
@scalar_rule acoth(x) inv(1 - x^2)
@scalar_rule acsch(x) -(inv(x^2 * sqrt(1 + x^-2)))
@scalar_rule acsch(x::Real) -(inv(abs(x) * sqrt(1 + x^2)))
@scalar_rule asech(x) -(inv(x * sqrt(1 - x^2)))
@scalar_rule asinh(x) inv(sqrt(x^2 + 1))
@scalar_rule atanh(x) inv(1 - x^2)

@scalar_rule acosd(x) (-(oftype(x, 180)) / π) / sqrt(1 - x^2)
@scalar_rule acotd(x) (-(oftype(x, 180)) / π) / (1 + x^2)
@scalar_rule acscd(x) ((-(oftype(x, 180)) / π) / x^2) / sqrt(1 - x^-2)
@scalar_rule acscd(x::Real) ((-(oftype(x, 180)) / π) / abs(x)) / sqrt(x^2 - 1)
@scalar_rule asecd(x) ((oftype(x, 180) / π) / x^2) / sqrt(1 - x^-2)
@scalar_rule asecd(x::Real) ((oftype(x, 180) / π) / abs(x)) / sqrt(x^2 - 1)
@scalar_rule asind(x) (oftype(x, 180) / π) / sqrt(1 - x^2)
@scalar_rule atand(x) (oftype(x, 180) / π) / (1 + x^2)

@scalar_rule cot(x) -((1 + Ω^2))
@scalar_rule coth(x) -(csch(x)^2)
@scalar_rule cotd(x) -(π / oftype(x, 180)) * (1 + Ω^2)
@scalar_rule csc(x) -Ω * cot(x)
@scalar_rule cscd(x) -(π / oftype(x, 180)) * Ω * cotd(x)
@scalar_rule csch(x) -(coth(x)) * Ω
@scalar_rule sec(x) Ω * tan(x)
@scalar_rule secd(x) (π / oftype(x, 180)) * Ω * tand(x)
@scalar_rule sech(x) -(tanh(x)) * Ω

@scalar_rule acot(x) -(inv(1 + x^2))
@scalar_rule acsc(x) -(inv(x^2 * sqrt(1 - x^-2)))
@scalar_rule acsc(x::Real) -(inv(abs(x) * sqrt(x^2 - 1)))
@scalar_rule asec(x) inv(x^2 * sqrt(1 - x^-2))
@scalar_rule asec(x::Real) inv(abs(x) * sqrt(x^2 - 1))

@scalar_rule cosd(x) -(π / oftype(x, 180)) * sind(x)
@scalar_rule cospi(x) -π * sinpi(x)
@scalar_rule sind(x) (π / oftype(x, 180)) * cosd(x)
@scalar_rule sinpi(x) π * cospi(x)
@scalar_rule tand(x) (π / oftype(x, 180)) * (1 + Ω^2)

@scalar_rule sinc(x) cosc(x)

# the position of the minus sign below warrants the correct type for π  
if VERSION ≥ v"1.6"
    @scalar_rule sincospi(x) @setup((sinpix, cospix) = Ω) (π * cospix) (π * (-sinpix))
end

@scalar_rule(
    clamp(x, low, high),
    @setup(islow = x < low, ishigh = high < x,),
    (!(islow | ishigh), islow, ishigh),
)
@scalar_rule x \ y (-(Ω / x), one(y) / x)

function frule((_, ẏ), ::typeof(identity), x)
    return (x, ẏ)
end

function rrule(::typeof(identity), x)
    function identity_pullback(ȳ)
        return (NoTangent(), ȳ)
    end
    return (x, identity_pullback)
end

# rouding related,
# we use `zero` rather than `ZeroTangent()` for scalar, and avoids issues with map etc
@scalar_rule round(x) zero(x)
@scalar_rule floor(x) zero(x)
@scalar_rule ceil(x) zero(x)

# `literal_pow`
# This is mostly handled by AD; it's a micro-optimisation to provide a gradient for x*x*x
# Note that rules for `^` are defined in the fastmath_able.jl

function frule((_, _, Δx, _), ::typeof(Base.literal_pow), ::typeof(^), x::Real, ::Val{2})
    return x * x, 2 * x * Δx
end
function frule((_, _, Δx, _), ::typeof(Base.literal_pow), ::typeof(^), x::Real, ::Val{3})
    x2 = x * x
    return x2 * x, 3 * x2 * Δx
end

function rrule(::typeof(Base.literal_pow), ::typeof(^), x::Real, ::Val{2})
    square_pullback(dy) = (NoTangent(), NoTangent(), ProjectTo(x)(2 * x * dy), NoTangent())
    return x * x, square_pullback
end
function rrule(::typeof(Base.literal_pow), ::typeof(^), x::Real, ::Val{3})
    x2 = x * x
    cube_pullback(dy) = (NoTangent(), NoTangent(), ProjectTo(x)(3 * x2 * dy), NoTangent())
    return x2 * x, cube_pullback
end
