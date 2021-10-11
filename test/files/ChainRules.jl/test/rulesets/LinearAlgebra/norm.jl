@testset "norm functions" begin

    # First test the un-exported functions which norm(A,p) calls
    # ==========================================================

    @testset "$fnorm(x::Array{$T,$(length(sz))})" for fnorm in (
            LinearAlgebra.norm1,
            LinearAlgebra.norm2,
            LinearAlgebra.normInf,
            LinearAlgebra.normMinusInf,
        ),
        T in (Float64, ComplexF64),
        sz in [(3,), (3, 3), (3, 2, 1)]

        x = randn(T, sz)
        # finite differences is unstable if maxabs (minabs) values are not well
        # separated from other values
        if fnorm === LinearAlgebra.normInf
            x[end] = 1000rand(T)
            kwargs = (atol=1e-5, rtol=1e-5)
        elseif fnorm == LinearAlgebra.normMinusInf
            x .*= 1000
            x[end] = rand(T)
            kwargs = (atol=1e-5, rtol=1e-5)
        else
            kwargs = NamedTuple()
        end

        if fnorm === LinearAlgebra.norm2
            @testset "frule" begin
                test_frule(fnorm, x)
            end
        end
        @testset "rrule" begin
            test_rrule(fnorm, x; kwargs...)
            x isa Matrix && @testset "$MT" for MT in
                                               (Diagonal, UpperTriangular, LowerTriangular)
                test_rrule(fnorm, MT(x); kwargs...)
            end

            ȳ = rand_tangent(fnorm(x))
            @test unthunk(rrule(fnorm, zero(x))[2](ȳ)[2]) ≈ zero(x)
            @test rrule(fnorm, x)[2](ZeroTangent())[2] isa ZeroTangent
        end
        ndims(x) > 1 && @testset "non-strided" begin
            xp = if x isa Matrix
                view(x, [1, 2, 3], 1:3)
            elseif x isa Array{T,3}
                PermutedDimsArray(x, (1, 2, 3))
            end
            @test !(xp isa StridedArray)
            test_rrule(fnorm, xp)
        end
        T == Float64 &&
            ndims(x) == 1 &&
            @testset "Integer input" begin
                x = [1, 2, 3]
                int_fwd, int_back = rrule(fnorm, x)
                float_fwd, float_back = rrule(fnorm, float(x))
                @test int_fwd ≈ float_fwd
                @test unthunk(int_back(1.0)[2]) ≈ unthunk(float_back(1.0)[2])
            end
    end

    # Next test norm(A, p=2) -- two methods
    # =====================================

    @testset "norm(x::Array{$T,$(length(sz))})" for T in (Float64, ComplexF64),
        sz in [(0,), (3,), (3, 3), (3, 2, 1)]

        x = randn(T, sz)

        @testset "frule" begin
            test_frule(norm, x)
            @test frule((ZeroTangent(), ZeroTangent()), norm, x)[2] isa ZeroTangent

            ẋ = rand_tangent(x)
            @test iszero(frule((ZeroTangent(), ẋ), norm, zero(x))[2])
        end
        @testset "rrule" begin
            test_rrule(norm, x)
            x isa Matrix && @testset "$MT" for MT in
                                               (Diagonal, UpperTriangular, LowerTriangular)
                test_rrule(norm, MT(x); check_inferred=VERSION >= v"1.5")
            end

            ȳ = rand_tangent(norm(x))
            @test unthunk(rrule(norm, zero(x))[2](ȳ)[2]) ≈ zero(x)
            @test rrule(norm, x)[2](ZeroTangent())[2] isa ZeroTangent
        end
        ndims(x) > 1 && @testset "non-strided" begin
            xp = if x isa Matrix
                view(x, [1, 2, 3], 1:3)
            elseif x isa Array{T,3}
                PermutedDimsArray(x, (1, 2, 3))
            end
            @assert !(xp isa StridedArray)
            test_frule(norm, xp)
            test_rrule(norm, xp)
        end
    end
    @testset "$fnorm(x::Array{$T,$(length(sz))}, $p) with size $sz" for fnorm in (
            norm, LinearAlgebra.normp
        ),
        p in (1.0, 2.0, Inf, -Inf, 2.5),
        T in (Float64, ComplexF64),
        sz in (fnorm === norm ? [(0,), (3,), (3, 3), (3, 2, 1)] : [(3,), (3, 3), (3, 2, 1)])

        x = randn(T, sz)
        # finite differences is unstable if maxabs (minabs) values are not well
        # separated from other values (same as above)
        if p == Inf
            if !isempty(x)
                x[end] = 1000rand(T)
            end
            kwargs = (atol=1e-5, rtol=1e-5)
        elseif p == -Inf
            if !isempty(x)
                x .*= 1000
                x[end] = rand(T)
            end
            kwargs = (atol=1e-5, rtol=1e-5)
        else
            kwargs = NamedTuple()
        end

        test_rrule(fnorm, x, p; kwargs...)
        x isa Matrix &&
            @testset "$MT" for MT in (Diagonal, UpperTriangular, LowerTriangular)
                test_rrule(fnorm, MT(x), p; kwargs..., check_inferred=VERSION >= v"1.5")
            end

        ȳ = rand_tangent(fnorm(x, p))
        @test unthunk(rrule(fnorm, zero(x), p)[2](ȳ)[2]) ≈ zero(x)
        @test rrule(fnorm, x, p)[2](ZeroTangent())[2] isa ZeroTangent
        T == Float64 &&
            sz == (3,) &&
            @testset "Integer input, p=$p" begin
                x = [1, 2, 3]
                int_fwd, int_back = rrule(fnorm, x, p)
                float_fwd, float_back = rrule(fnorm, float(x), p)
                @test int_fwd ≈ float_fwd
                @test unthunk(int_back(1.0)[2]) ≈ unthunk(float_back(1.0)[2])
            end
    end
    # Extra test for norm(adjoint vector, p)
    @testset "norm($fdual(::Vector{$T}), 2.5)" for T in (Float64, ComplexF64),
        fdual in (adjoint, transpose)

        x = fdual(randn(T, 3))
        p = 2.5

        test_rrule(norm, x, p)
        ȳ = rand_tangent(norm(x, p))
        @test unthunk(rrule(norm, x, p)[2](ȳ)[2]) isa typeof(x)
    end

    # Scalar norm(x, p)
    # =================

    @testset "norm(x::$T, p)" for T in (Float64, ComplexF64)
        @testset "p = $p" for p in (-1.0, 2.0, 2.5)
            test_frule(norm, randn(T), p)
            test_rrule(norm, randn(T), p)

            _, back = rrule(norm, randn(T), p)
            @test back(ZeroTangent()) == (NoTangent(), ZeroTangent(), ZeroTangent())
        end
        @testset "p = 0" begin
            p = 0.0
            x = randn(T)
            y = norm(x, p)
            ẋ, ṗ = rand_tangent.((x, p))
            x̄, p̄, ȳ = rand_tangent.((x, p, y))
            y_fwd, ẏ = frule((ZeroTangent(), ẋ, ṗ), norm, x, p)
            @test y_fwd == y
            @test iszero(ẏ)
            y_rev, back = rrule(norm, x, p)
            @test y_rev == y
            @test back(ȳ) == (NoTangent(), zero(x), ZeroTangent())
            @test back(ZeroTangent()) == (NoTangent(), ZeroTangent(), ZeroTangent())
        end
    end
end

# normalise(x, p) and normalise(A, p)
# ===================================

@testset "normalize" begin
    @testset "x::Vector{$T}" for T in (Float64, ComplexF64)
        x = randn(T, 3)
        test_rrule(normalize, x)
        @test rrule(normalize, x)[2](ZeroTangent()) === (NoTangent(), ZeroTangent())
    end
    @testset "x::Vector{$T}, p=$p" for T in (Float64, ComplexF64),
        p in (1.0, 2.0, -Inf, Inf, 2.5)
        # skip p=0, since FD is unstable
        x = randn(T, 3)
        test_rrule(normalize, x, p)
        @test rrule(normalize, x, p)[2](ZeroTangent()) ===
            (NoTangent(), ZeroTangent(), ZeroTangent())
    end
end
