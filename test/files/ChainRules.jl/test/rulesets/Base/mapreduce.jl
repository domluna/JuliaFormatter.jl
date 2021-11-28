# for sum(xs, weights) (#522)
Base.sum(xs::AbstractArray, weights::AbstractArray) = dot(xs, weights)
struct SumRuleConfig <: RuleConfig{Union{HasReverseMode}} end

@testset "Maps and Reductions" begin
    @testset "sum(x; dims=$dims)" for dims in (:, 2, (1, 3))
        # Forward
        test_frule(sum, rand(5); fkwargs=(; dims=dims))
        test_frule(sum, rand(ComplexF64, 2, 3, 4); fkwargs=(; dims=dims))

        # Reverse
        test_rrule(sum, rand(5); fkwargs=(; dims=dims))
        test_rrule(sum, rand(ComplexF64, 2, 3, 4); fkwargs=(; dims=dims))

        # Structured matrices
        test_rrule(sum, rand(5)'; fkwargs=(; dims=dims))
        y, back = rrule(sum, UpperTriangular(rand(5, 5)); dims=dims)
        unthunk(back(y * (1 + im))[2]) isa UpperTriangular{Float64}
        @test_skip test_rrule(
            sum,
            UpperTriangular(rand(5, 5)) ⊢ randn(5, 5);
            fkwargs=(; dims=dims),
            check_inferred=false,
        ) # Problem: in add!!  Evaluated: isapprox

        # Boolean -- via @non_differentiable
        test_rrule(sum, randn(5) .> 0; fkwargs=(; dims=dims))

        # Function allowing for 2nd derivatives
        for x in (rand(5), rand(2, 3, 4))
            dy = maximum(x; dims=dims)
            test_frule(ChainRules._unsum, x, dy, dims)
            test_rrule(ChainRules._unsum, x, dy, dims)
        end

        # Arrays of arrays
        for x in
            ([rand(ComplexF64, 3) for _ in 1:4], [rand(3) for _ in 1:2, _ in 1:3, _ in 1:4])
            test_rrule(sum, x; fkwargs=(; dims=dims), check_inferred=false)

            dy = sum(x; dims=dims)
            ddy = rrule(ChainRules._unsum, x, dy, dims)[2](x)[3]
            @test size(ddy) == size(dy)
        end
    end

    @testset "sum abs2" begin
        sizes = (3, 4, 7)
        @testset "dims = $dims" for dims in (:, 1)
            @testset "Array{$N, $T}" for N in eachindex(sizes), T in (Float64, ComplexF64)
                x = randn(T, sizes[1:N]...)
                test_frule(sum, abs2, x; fkwargs=(; dims=dims))
                test_rrule(sum, abs2, x; fkwargs=(; dims=dims))
            end

            # Boolean -- via @non_differentiable, test that this isn't ambiguous
            test_rrule(sum, abs2, randn(5) .> 0; fkwargs=(; dims=dims))
        end
    end  # sum abs2

    @testset "sum(f, xs)" begin
        # This calls back into AD
        test_rrule(sum, abs, [-4.0, 2.0, 2.0])
        test_rrule(sum, cbrt, randn(5))
        test_rrule(sum, Multiplier(2.0), [2.0, 4.0, 8.0])

        # Complex numbers
        test_rrule(sum, sqrt, rand(ComplexF64, 5))
        test_rrule(sum, abs, rand(ComplexF64, 3, 4))  # complex -> real

        # inference fails for array of arrays
        test_rrule(sum, sum, [[2.0, 4.0], [4.0, 1.9]]; check_inferred=false)

        # dims kwarg
        test_rrule(sum, abs, [-2.0 4.0; 5.0 1.9]; fkwargs=(; dims=1))
        test_rrule(sum, abs, [-2.0 4.0; 5.0 1.9]; fkwargs=(; dims=2))
        test_rrule(sum, sqrt, rand(ComplexF64, 3, 4); fkwargs=(; dims=(1,)))

        test_rrule(sum, abs, @SVector [1.0, -3.0])

        # covectors
        x = [-4.0 2.0; 2.0 -1.0]
        test_rrule(sum, inv, x[1, :]')
        test_rrule(sum, inv, x[1:1, :]')
        test_rrule(sum, inv, transpose(view(x, 1, :)))

        # Make sure we preserve type for StaticArrays
        ADviaRuleConfig = ChainRulesTestUtils.ADviaRuleConfig
        _, pb = rrule(ADviaRuleConfig(), sum, abs, @SVector [1.0, -3.0])
        @test pb(1.0) isa Tuple{NoTangent,NoTangent,SVector{2,Float64}}

        # make sure we preserve type for Diagonal
        _, pb = rrule(ADviaRuleConfig(), sum, abs, Diagonal([1.0, -3.0]))
        @test pb(1.0)[3] isa Diagonal

        # Boolean -- via @non_differentiable, test that this isn't ambiguous
        test_rrule(sum, sqrt, randn(5) .> 0)
        test_rrule(sum, sqrt, randn(5, 5) .> 0; fkwargs=(; dims=1))
        # ... and Bool produced by function
        @test_skip test_rrule(sum, iszero, randn(5))  # DimensionMismatch("second dimension of A, 1, does not match length of x, 0")
    end

    # https://github.com/JuliaDiff/ChainRules.jl/issues/522
    @testset "sum(xs, weights) (#522)" begin
        xs = rand(5)
        weights = rand(5)

        @test rrule(SumRuleConfig(), Base.sum, xs, weights) isa Nothing
    end

    @testset "prod" begin
        @testset "Array{$T}" for T in [Float64, ComplexF64]
            @testset "size = $sz, dims = $dims" for (sz, dims) in [
                ((12,), :),
                ((12,), 1),
                ((3, 4), 1),
                ((3, 4), 2),
                ((3, 4), :),
                ((3, 4), [1, 2]),
                ((3, 4, 1), 1),
                ((3, 2, 2), 3),
                ((3, 2, 2), 2:3),
            ]
                x = randn(T, sz)
                test_rrule(prod, x; fkwargs=(dims=dims,), check_inferred=true)
                x[1] = 0
                test_rrule(prod, x; fkwargs=(dims=dims,), check_inferred=true)
                x[5] = 0
                test_rrule(prod, x; fkwargs=(dims=dims,), check_inferred=true)
                x[3] = x[7] = 0  # two zeros along some slice, for any dims
                test_rrule(prod, x; fkwargs=(dims=dims,), check_inferred=true)

                if ndims(x) == 3
                    xp = PermutedDimsArray(x, (3, 2, 1))  # not a StridedArray
                    test_rrule(prod, xp; fkwargs=(dims=dims,), check_inferred=true)
                end
            end

            @testset "structured wrappers" begin
                # Adjoint -- like PermutedDimsArray this may actually be used
                xa = adjoint(rand(T, 4, 4))
                test_rrule(prod, xa)
                test_rrule(prod, xa; fkwargs=(dims=2,))
                # use Adjoint for tangent
                test_rrule(prod, xa ⊢ rand(T, 4, 4)')
                test_rrule(prod, xa ⊢ rand(T, 4, 4)'; fkwargs=(dims=2,))
                @test unthunk(rrule(prod, adjoint(rand(T, 3, 3)))[2](1.0)[2]) isa Matrix
                @test unthunk(
                    rrule(prod, adjoint(rand(T, 3, 3)); dims=1)[2](ones(1, 3))[2]
                ) isa Matrix

                # Diagonal -- a stupid thing to do, product of zeros! Shouldn't be an error though:
                @test iszero(unthunk(rrule(prod, Diagonal(rand(T, 3)))[2](1.0)[2]))
                @test iszero(
                    unthunk(rrule(prod, Diagonal(rand(T, 3)); dims=1)[2](ones(1, 3))[2])
                )
                # does a division for the complex case, so is not necessarily exact
                @test isapprox(
                    unthunk(rrule(prod, Diagonal(rand(T, 1)))[2](1.0)[2]), # 1x1 sparse matrix
                    hcat(1);
                    rtol=T <: Complex ? 2eps() : 0.0,
                )
                @test unthunk(
                    rrule(prod, Diagonal(ones(T, 2)); dims=1)[2](ones(1, 2))[2]
                ) == Diagonal([0.0 1; 1 0])

                # Triangular -- almost equally stupud
                @test iszero(
                    unthunk(rrule(prod, UpperTriangular(rand(T, 3, 3)))[2](1.0)[2])
                )
                @test unthunk(rrule(prod, UpperTriangular(ones(T, 2, 2)))[2](1.0)[2]) ==
                    UpperTriangular([0.0 0; 1 0])

                # Symmetric -- at least this doesn't have zeros, still an unlikely combination
                xs = Symmetric(rand(T, 4, 4))
                @test unthunk(rrule(prod, Symmetric(T[1 2; -333 4]))[2](1.0)[2]) ==
                    [16 8; 8 4]
                # TODO debug why these fail  https://github.com/JuliaDiff/ChainRules.jl/issues/475
                @test_skip test_rrule(prod, xs)
                @test_skip test_rrule(prod, xs; fkwargs=(dims=2,))
            end
        end
        @testset "Array{Float32}, no zero entries" begin
            v = [1.0f-5, 1.0f-10, 1.0f-15, 1.0f-20]
            @test prod(v) == 0
            @test unthunk(rrule(prod, v)[2](1.0f0)[2]) == zeros(4)
            test_rrule(prod, v)
        end
    end # prod
end

@testset "Accumulations" begin
    @testset "cumprod" begin
        v = round.(10 .* randn(9), sigdigits=3)
        test_rrule(cumprod, v)
        v[3] = 0
        test_rrule(cumprod, v)
        v[6] = 0
        test_rrule(cumprod, v)

        @testset "higher dimensions, dims=$dims" for dims in (1, 2, 3)
            m = round.(10 .* randn(4, 5), sigdigits=3)
            test_rrule(cumprod, m; fkwargs=(; dims=dims), atol=0.1)
            m[2, 2] = 0
            m[2, 4] = 0
            test_rrule(cumprod, m; fkwargs=(; dims=dims))

            t = round.(10 .* randn(3, 3, 3), sigdigits=3)
            test_rrule(cumprod, t; fkwargs=(; dims=dims))
            t[2, 2, 2] = 0
            t[2, 3, 3] = 0
            test_rrule(cumprod, t; fkwargs=(; dims=dims))
        end

        @testset "types" begin
            back = rrule(cumprod, [1, 2, 3])[2]  # rule allows integer input, but test_rrule does not
            @test unthunk(back(fill(0.5, 3))[2]) == [9 / 2, 2, 1]

            back = rrule(cumprod, PermutedDimsArray([1 2; 3 4], (2, 1)); dims=1)[2]
            @test unthunk(back(ones(Float32, 2, 2))[2]) == [3 5; 1 3]

            @test_throws Exception cumprod(Symmetric([1 2; 3 4]); dims=1) # forward pass fails, so can't test gradient

            back = rrule(cumprod, Diagonal([1, 2]); dims=1)[2]
            @test unthunk(back(fill(0.5, 2, 2))[2]) ≈ [1/2 0; 0 0]  # ProjectTo'd to Diagonal now
        end
    end
end
