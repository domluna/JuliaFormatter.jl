# Simple Distributions like object for testing purposes
struct NormalDistribution
    μ::Any
    σ::Any
end
Random.rand(d::NormalDistribution) = d.μ + d.σ * randn()

@testset "Random" begin
    rng_types = [MersenneTwister]
    isdefined(Random, :Xoshiro) && push!(rng_types, getfield(Random, :Xoshiro))

    @testset "$Rng" for Rng in rng_types
        @testset "no args" begin
            rng, dΩ = frule((5.0,), Rng)
            @test rng isa Rng
            @test dΩ isa ZeroTangent

            rng, pb = rrule(Rng)
            @test rng isa Rng
            @test first(pb(10)) isa typeof(NoTangent())
        end
        @testset "unary" begin
            rng, dΩ = frule((5.0, 4.0), Rng, 123)
            @test rng isa Rng
            @test dΩ isa ZeroTangent

            rng, pb = rrule(Rng, 123)
            @test rng isa Rng
            @test all(map(x -> x isa AbstractZero, pb(10)))
        end
    end

    @testset "rand" begin
        non_differentiables = [
            ((), Float64),
            ((MersenneTwister(123),), Float64),
            ((MersenneTwister(123), 2, 2), Matrix{<:Float64}),
            ((Float32,), Float32),
            ((Float32, 2, 2), Matrix{<:Float32}),
            ((Float32, (2, 2)), Matrix{<:Float32}),
            ((2, 2), Matrix{<:Float64}),
        ]
        if isdefined(Random, :Xoshiro)
            Xoshiro = getfield(Random, :Xoshiro)
            push!(non_differentiables, ((Xoshiro(123),), Float64))
            push!(non_differentiables, ((Xoshiro(123), 2, 2), Matrix{<:Float64}))
        end

        for (args, xType) in non_differentiables
            x, dΩ = frule((ZeroTangent(), randn(args...)), rand, args...)
            @test x isa xType
            @test dΩ isa NoTangent

            x, pb = rrule(rand, args...)
            @test x isa xType
            dself, dargs = Iterators.peel(pb(10.0))
            @test iszero(dself)
            @test all(darg isa NoTangent for darg in dargs)
        end

        # Make sure that we do *not* have these set as non_differentiable. as they are differentiable
        @test nothing === frule(
            (ZeroTangent(), Tangent{NormalDistribution}(; μ=0.5, σ=2.0)),
            rand,
            NormalDistribution(0.1, 1.5),
        )
        @test rrule(rand, NormalDistribution(0.1, 1.5)) === nothing
    end
end
