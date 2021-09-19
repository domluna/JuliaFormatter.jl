@testset "matrix functions" begin
    @testset "LinearAlgebra.exp!(A::Matrix) frule" begin
        n = 10
        # each normalization hits a specific branch
        @testset "A::Matrix{$T}, opnorm(A,1)=$nrm" for T in (Float64, ComplexF64),
            nrm in (0.01, 0.1, 0.5, 1.5, 3.0, 6.0, 12.0)

            A = randn(T, n, n)
            A *= nrm / opnorm(A, 1)
            tols = nrm == 0.1 ? (atol=1e-8, rtol=1e-8) : NamedTuple()
            test_frule(LinearAlgebra.exp!, A; tols...)
        end
        @testset "imbalanced A" begin
            A = Float64[0 10 0 0; -1 0 0 0; 0 0 0 0; -2 0 0 0]
            test_frule(LinearAlgebra.exp!, A)
        end
        @testset "hermitian A, T=$T" for T in (Float64, ComplexF64)
            A = Matrix(Hermitian(randn(T, n, n)))
            test_frule(LinearAlgebra.exp!, A)
            test_frule(LinearAlgebra.exp!, A ⊢ Matrix(Hermitian(randn(T, n, n))))
        end
    end

    @testset "exp(A::Matrix) rrule" begin
        n = 10
        # each normalization hits a specific branch
        @testset "A::Matrix{$T}, opnorm(A,1)=$nrm" for T in (Float64, ComplexF64),
            nrm in (0.01, 0.1, 0.5, 1.5, 3.0, 6.0, 12.0)

            A = randn(T, n, n)
            A *= nrm / opnorm(A, 1)
            # rrule is not inferrable, but pullback should be
            tols = nrm == 0.1 ? (atol=1e-8, rtol=1e-8) : NamedTuple()
            test_rrule(exp, A; check_inferred=false, tols...)
            Y, back = rrule(exp, A)
            @maybe_inferred back(rand_tangent(Y))
        end
        @testset "imbalanced A" begin
            A = Float64[0 10 0 0; -1 0 0 0; 0 0 0 0; -2 0 0 0]
            test_rrule(exp, A; check_inferred=false)
        end
        @testset "hermitian A, T=$T" for T in (Float64, ComplexF64)
            A = Matrix(Hermitian(randn(T, n, n)))
            test_rrule(exp, A; check_inferred=false)
            test_rrule(
                exp,
                A;
                check_inferred=false,
                output_tangent=Matrix(Hermitian(randn(T, n, n))),
            )
        end
    end
end
