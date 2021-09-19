@testset "BLAS" begin
    @testset "dot" begin
        @testset "all entries" begin
            n = 10
            test_frule(BLAS.dot, randn(n), randn(n))
            test_rrule(BLAS.dot, randn(n), randn(n))
        end

        @testset "over strides" begin
            n = 10
            incx = 2
            incy = 3
            test_rrule(BLAS.dot, n, randn(n * incx), incx, randn(n * incy), incy)
        end
    end

    @testset "nrm2" begin
        @testset "all entries" begin
            @testset "$T" for T in (Float64, ComplexF64)
                n = 10
                test_frule(BLAS.nrm2, randn(T, n))
                test_rrule(BLAS.nrm2, randn(T, n))
            end
        end

        @testset "over strides" begin
            dims = (3, 2, 1)
            incx = 2
            @testset "Array{$T,$N}" for N in eachindex(dims), T in (Float64, ComplexF64)
                s = (dims[1] * incx, dims[2:N]...)
                n = div(prod(s), incx)
                test_rrule(BLAS.nrm2, n, randn(T, s), incx; atol=0, rtol=1e-5)
            end
        end
    end

    @testset "asum" begin
        @testset "all entries" begin
            @testset "$T" for T in (Float64, ComplexF64)
                n = 6
                test_frule(BLAS.asum, randn(T, n))
                test_rrule(BLAS.asum, randn(T, n))
            end
        end

        @testset "over strides" begin
            dims = (2, 2, 1)
            incx = 2
            @testset "Array{$T,$N}" for N in eachindex(dims), T in (Float64, ComplexF64)
                s = (dims[1] * incx, dims[2:N]...)
                n = div(prod(s), incx)
                test_rrule(BLAS.asum, n, randn(T, s), incx)
            end
        end
    end

    @testset "gemm" begin
        for m in 3:5,
            n in 3:5,
            p in 3:5,
            tA in ('N', 'C', 'T'),
            tB in ('N', 'C', 'T'),
            T in (Float64, ComplexF64)

            A = randn(T, tA === 'N' ? (m, n) : (n, m))
            B = randn(T, tB === 'N' ? (n, p) : (p, n))
            test_rrule(gemm, tA, tB, A, B; check_inferred=false)
            test_rrule(  # 5 arg version with scaling scalar
                gemm,
                tA,
                tB,
                randn(T),
                A,
                B;
                check_inferred=false,
            )
        end
    end

    @testset "gemv" begin
        for n in 3:5, m in 3:5, t in ('N', 'C', 'T'), T in (Float64, ComplexF64)
            x = randn(T, t === 'N' ? n : m)
            test_rrule(gemv, t, randn(T), randn(T, m, n), x; check_inferred=false)
        end
    end
end
