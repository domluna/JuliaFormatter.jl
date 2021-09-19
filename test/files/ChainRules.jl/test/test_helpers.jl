# A functor for testing
struct Multiplier{T}
    x::T
end
(m::Multiplier)(y) = m.x * y
function ChainRulesCore.rrule(m::Multiplier, y)
    Multiplier_pullback(z̄) = Tangent{typeof(m)}(; x=y * z̄), m.x * z̄
    return m(y), Multiplier_pullback
end

# NoRules - has no rules defined
struct NoRules end

@testset "test_helpers.jl" begin
    @testset "Multiplier functor test-helper" begin
        test_rrule(Multiplier(4.0), 3.0)
    end
end
