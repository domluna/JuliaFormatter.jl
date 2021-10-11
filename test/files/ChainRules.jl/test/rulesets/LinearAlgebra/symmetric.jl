@testset "Symmetric/Hermitian rules" begin
    @testset "$(SymHerm)(::AbstractMatrix{$T}, :$(uplo))" for SymHerm in
                                                              (Symmetric, Hermitian),
        T in (Float64, ComplexF64),
        uplo in (:U, :L)

        @testset "frule" begin
            test_frule(SymHerm, rand(T, 3, 3), uplo)
        end
        @testset "rrule" begin
            # on old versions of julia this combination doesn't infer but we don't care as
            # it infers fine on modern versions.
            check_inferred = !(VERSION < v"1.5" && T <: ComplexF64 && SymHerm <: Hermitian)

            @testset "back(::$MT)" for MT in (Matrix, LowerTriangular, UpperTriangular)
                x = randn(T, 3, 3)
                ΔΩ = MT(randn(T, 3, 3))
                test_rrule(
                    SymHerm,
                    x,
                    uplo;
                    output_tangent=ΔΩ,
                    # type stability here critically relies on uplo being constant propagated,
                    # so we need to test this more carefully below
                    check_inferred=false,
                )
                if check_inferred && false # ChainRulesCore #407
                    @maybe_inferred (function (SymHerm, x, ΔΩ, ::Val)
                        return rrule(SymHerm, x, uplo)[2](ΔΩ)
                    end)(
                        SymHerm, x, ΔΩ, Val(uplo)
                    )
                end
            end
            @testset "back(::Diagonal)" begin
                x = randn(T, 3, 3)
                ΔΩ = Diagonal(randn(T, 3, 3))
                test_rrule(
                    SymHerm,
                    x ⊢ Diagonal(randn(T, 3)),
                    uplo;
                    check_inferred=false,
                    output_tangent=ΔΩ,
                )
                if check_inferred && false # ChainRulesCore #407
                    @maybe_inferred (function (SymHerm, x, ΔΩ, ::Val)
                        return rrule(SymHerm, x, uplo)[2](ΔΩ)
                    end)(
                        SymHerm, x, ΔΩ, Val(uplo)
                    )
                end
            end
        end
    end
    # constructing a `Matrix`/`Array` from `SymHerm`
    @testset "$(f)(::$(SymHerm){$T}) with uplo=:$uplo" for f in (Matrix, Array),
        SymHerm in (Symmetric, Hermitian),
        T in (Float64, ComplexF64),
        uplo in (:U, :L)

        x = SymHerm(randn(T, 3, 3), uplo)
        test_rrule(f, x)

        # intentionally specifying tangents here to test both SymHerm (default) and Matrix
        test_frule(f, x)
        test_frule(f, x ⊢ randn(T, 3, 3))
    end

    # symmetric/hermitian eigendecomposition follows the sign convention
    # v = v * sign(real(vₖ)) * sign(vₖ)', where vₖ is the first or last coordinate
    # in the eigenvector. This is unstable for finite differences, but using the convention
    # v = v * sign(vₖ)' seems to be more stable, the (co)tangents are related as
    # ∂v_ad = sign(real(vₖ)) * ∂v_fd
    function _eigvecs_stabilize_mat(vectors, uplo)
        Ui = Symbol(uplo) === :U ? @view(vectors[end, :]) : @view(vectors[1, :])
        return Diagonal(conj.(sign.(Ui)))
    end

    @testset "eigendecomposition" begin
        @testset "eigen/eigen!" begin
            # avoid implementing to_vec(::Eigen)
            asnt(E::Eigen) = (values=E.values, vectors=E.vectors)

            function _eigen_stable(A)
                F = eigen(A)
                rmul!(F.vectors, _eigvecs_stabilize_mat(F.vectors, A.uplo))
                return F
            end

            n = 10
            @testset "frule for eigen!(::$SymHerm{$T}) uplo=$uplo" for SymHerm in (
                    Symmetric, Hermitian
                ),
                T in (SymHerm === Symmetric ? (Float64,) : (Float64, ComplexF64)),
                uplo in (:L, :U)

                A, ΔA = randn(T, n, n), randn(T, n, n)
                symA = SymHerm(A, uplo)
                ΔsymA = frule((ZeroTangent(), ΔA, ZeroTangent()), SymHerm, A, uplo)[2]

                F = eigen!(copy(symA))
                @test @maybe_inferred(
                    frule((ZeroTangent(), ZeroTangent()), eigen!, copy(symA))
                ) == (F, ZeroTangent())
                F_ad, ∂F_ad = @maybe_inferred frule(
                    (ZeroTangent(), copy(ΔsymA)), eigen!, copy(symA)
                )
                @test F_ad == F
                @test ∂F_ad isa Tangent{typeof(F)}
                @test ∂F_ad.values isa typeof(F.values)
                @test ∂F_ad.vectors isa typeof(F.vectors)

                f = x -> asnt(eigen(SymHerm(x, uplo)))
                ∂F_fd = jvp(_fdm, f, (A, ΔA))
                @test ∂F_ad.values ≈ ∂F_fd.values

                f_stable = x -> asnt(_eigen_stable(SymHerm(x, uplo)))
                F_stable = f_stable(A)
                ∂F_stable_fd = jvp(_fdm, f_stable, (A, ΔA))
                C = _eigvecs_stabilize_mat(F.vectors, uplo)
                @test ∂F_ad.vectors * C ≈ ∂F_stable_fd.vectors
            end

            @testset "rrule for eigen(::$SymHerm{$T}) uplo=$uplo" for SymHerm in (
                    Symmetric, Hermitian
                ),
                T in (SymHerm === Symmetric ? (Float64,) : (Float64, ComplexF64)),
                uplo in (:L, :U)

                A, ΔU, Δλ = randn(T, n, n), randn(T, n, n), randn(n)
                symA = SymHerm(A, uplo)

                F = eigen(symA)
                ΔF = Tangent{typeof(F)}(; values=Δλ, vectors=ΔU)
                F_ad, back = @maybe_inferred rrule(eigen, symA)
                @test F_ad == F

                C = _eigvecs_stabilize_mat(F.vectors, uplo)
                CT = Tangent{typeof(F)}

                @testset for nzprops in ([:values], [:vectors], [:values, :vectors])
                    ∂F = CT(; [s => getproperty(ΔF, s) for s in nzprops]...)
                    ∂F_stable = (; [s => copy(getproperty(ΔF, s)) for s in nzprops]...)
                    :vectors in nzprops && rmul!(∂F_stable.vectors, C)

                    f_stable = function (x)
                        F_ = _eigen_stable(SymHerm(x, uplo))
                        return (; (s => getproperty(F_, s) for s in nzprops)...)
                    end

                    ∂self, ∂symA = @maybe_inferred back(∂F)
                    @test ∂self === NoTangent()
                    @test ∂symA isa typeof(symA)
                    @test ∂symA.uplo == symA.uplo
                    ∂A = rrule(SymHerm, A, uplo)[2](∂symA)[2]
                    ∂A_fd = j′vp(_fdm, f_stable, ∂F_stable, A)[1]
                    @test ∂A ≈ ∂A_fd
                end

                @test @maybe_inferred(back(ZeroTangent())) == (NoTangent(), ZeroTangent())
                @test @maybe_inferred(back(CT())) == (NoTangent(), ZeroTangent())
            end

            # when value used to determine phase convention is low, the usual derivatives
            # become unstable, causing the rules to compose poorly in a program.
            # this test set checks that the rules compose correctly for the function
            # f(A) = I, using eigenvectors, where all sensitivities should cancel
            @testset "phase convention from low value" begin
                @testset for min_val in [0, eps(), sqrt(eps()), cbrt(eps()), eps()^(1//4)],
                    uplo in (:U, :L)

                    U = randn(ComplexF64, n, n)
                    U[uplo === :U ? n : 1] = min_val
                    U = Matrix(qr(U).Q)
                    λ = sort(randn(n))
                    A = Hermitian(U * Diagonal(λ) * U')
                    function f(A)
                        V = eigen(A).vectors
                        return V * V'
                    end

                    Ȧ = Hermitian(randn(eltype(A), size(A)))
                    F, Ḟ_ad = frule((ZeroTangent(), copy(Ȧ)), eigen!, copy(A))
                    V, V̇_ad = F.vectors, Ḟ_ad.vectors
                    Ω̇_ad = V̇_ad' * V + V' * V̇_ad
                    @test maximum(abs, Ω̇_ad) < sqrt(eps())

                    Ω̄ = randn(eltype(A), (n, n))
                    V̄ = V * (Ω̄ + Ω̄')
                    F̄ = Tangent{typeof(F)}(; vectors=V̄)
                    _, back = rrule(eigen, A)
                    Ā = back(F̄)[2]
                    @test maximum(abs, Ā) < sqrt(eps())
                end
            end
        end

        @testset "eigvals!/eigvals" begin
            n = 10
            @testset "frule for eigvals!(::$SymHerm{$T}) uplo=$uplo" for SymHerm in (
                    Symmetric, Hermitian
                ),
                T in (SymHerm === Symmetric ? (Float64,) : (Float64, ComplexF64)),
                uplo in (:L, :U)

                A, ΔA = randn(T, n, n), randn(T, n, n)
                symA = SymHerm(A, uplo)
                ΔsymA = @maybe_inferred frule(
                    (ZeroTangent(), ΔA, ZeroTangent()), SymHerm, A, uplo
                )[2]

                λ = eigvals!(copy(symA))
                λ_ad, ∂λ_ad = @maybe_inferred frule(
                    (ZeroTangent(), copy(ΔsymA)), eigvals!, copy(symA)
                )
                @test λ_ad ≈ λ # inexact because frule uses eigen not eigvals
                @test ∂λ_ad isa typeof(λ)
                @test ∂λ_ad ≈ jvp(_fdm, A -> eigvals(SymHerm(A, uplo)), (A, ΔA))
            end

            @testset "rrule for eigvals(::$SymHerm{$T}) uplo=$uplo" for SymHerm in (
                    Symmetric, Hermitian
                ),
                T in (SymHerm === Symmetric ? (Float64,) : (Float64, ComplexF64)),
                uplo in (:L, :U)

                A, Δλ = randn(T, n, n), randn(n)
                symA = SymHerm(A, uplo)

                λ = eigvals(symA)
                λ_ad, back = @maybe_inferred rrule(eigvals, symA)
                @test λ_ad ≈ λ # inexact because rrule uses eigen not eigvals
                ∂self, ∂symA = @maybe_inferred back(Δλ)
                @test ∂self === NoTangent()
                @test ∂symA isa typeof(symA)
                @test ∂symA.uplo == symA.uplo

                # pull the cotangent back to A to test against finite differences
                ∂A = rrule(SymHerm, A, uplo)[2](∂symA)[2]
                @test ∂A ≈ j′vp(_fdm, A -> eigvals(SymHerm(A, uplo)), Δλ, A)[1]

                @test @maybe_inferred(back(ZeroTangent())) == (NoTangent(), ZeroTangent())
            end
        end
    end

    @testset "singular value decomposition" begin
        # avoid implementing to_vec(::SVD)
        asnt(F::SVD) = (U=F.U, S=F.S, V=F.V, Vt=F.Vt)

        function _svd_stable(A)
            F = svd(A)
            C = _eigvecs_stabilize_mat(F.U, A.uplo)
            rmul!(F.U, C)
            lmul!(C, F.Vt)
            return F
        end

        n = 10
        VERSION ≥ v"1.3.0" &&
            @testset "rrule for svd(::$SymHerm{$T}) uplo=$uplo" for SymHerm in
                                                                    (Symmetric, Hermitian),
                T in (SymHerm === Symmetric ? (Float64,) : (Float64, ComplexF64)),
                uplo in (:L, :U)

                A, ΔU, ΔV, ΔVt = ntuple(_ -> randn(T, n, n), 4)
                ΔS = randn(n)
                symA = SymHerm(A, uplo)

                F = svd(symA)
                CT = Tangent{typeof(F)}
                ΔF = CT(; U=ΔU, V=ΔV, Vt=ΔVt, S=ΔS)
                F_ad, back = @maybe_inferred rrule(svd, symA)
                @test F_ad == F

                C = _eigvecs_stabilize_mat(F.U, uplo)

                # pull back different combination of non-ZeroTangent cotangents
                @testset for nzprops in ([:U], [:S], [:V], [:Vt], [:U, :S, :V, :Vt])
                    ∂F = CT(; [s => getproperty(ΔF, s) for s in nzprops]...)
                    ∂F_stable = (; [s => copy(getproperty(ΔF, s)) for s in nzprops]...)
                    :U in nzprops && rmul!(∂F_stable.U, C)
                    :Vt in nzprops && lmul!(C, ∂F_stable.Vt)
                    :V in nzprops && rmul!(∂F_stable.V, C)

                    f_stable = function (x)
                        F_ = _svd_stable(SymHerm(x, uplo))
                        return (; (s => getproperty(F_, s) for s in nzprops)...)
                    end

                    VERSION ≥ v"1.6.0-DEV.1686" && @maybe_inferred back(∂F)
                    ∂self, ∂symA = back(∂F)
                    @test ∂self === NoTangent()
                    @test ∂symA isa typeof(symA)
                    @test ∂symA.uplo == symA.uplo
                    ∂A = rrule(SymHerm, A, uplo)[2](∂symA)[2]
                    ∂A_fd = j′vp(_fdm, f_stable, ∂F_stable, A)[1]
                    @test ∂A ≈ ∂A_fd
                end

                @test @maybe_inferred(back(ZeroTangent())) == (NoTangent(), ZeroTangent())
                @test @maybe_inferred(back(CT())) == (NoTangent(), ZeroTangent())
            end

        @testset "rrule for svdvals(::$SymHerm{$T}) uplo=$uplo" for SymHerm in
                                                                    (Symmetric, Hermitian),
            T in (SymHerm === Symmetric ? (Float64,) : (Float64, ComplexF64)),
            uplo in (:L, :U)

            A, ΔS = randn(T, n, n), randn(n)
            symA = SymHerm(A, uplo)

            S = svdvals(symA)
            S_ad, back = @maybe_inferred rrule(svdvals, symA)
            @test S_ad ≈ S # inexact because rrule uses svd not svdvals
            ∂self, ∂symA = @maybe_inferred back(ΔS)
            @test ∂self === NoTangent()
            @test ∂symA isa typeof(symA)
            @test ∂symA.uplo == symA.uplo

            # pull the cotangent back to A to test against finite differences
            ∂A = rrule(SymHerm, A, uplo)[2](∂symA)[2]
            @test ∂A ≈ j′vp(_fdm, A -> svdvals(SymHerm(A, uplo)), ΔS, A)[1]

            @test @maybe_inferred(back(ZeroTangent())) == (NoTangent(), ZeroTangent())
        end
    end

    @testset "Symmetric/Hermitian matrix functions" begin
        # generate random matrices of type TA in the domain of f
        function rand_matfun_input(f, TA, T, uplo, n, hermout)
            U = Matrix(qr(randn(T, n, n)).Q)
            if hermout # f(A) will also be a TA
                λ = if f in (acos, asin, atanh)
                    2 .* rand(real(T), n) .- 1
                elseif f in (log, sqrt)
                    abs.(randn(real(T), n))
                elseif f === acosh
                    1 .+ abs.(randn(real(T), n))
                else
                    randn(real(T), n)
                end
            else
                λ = randn(real(T), n)
                λ = if f === atanh
                    2 .* rand(real(T), n) .- 1
                else
                    randn(real(T), n)
                end
            end
            return TA(U * Diagonal(λ) * U', uplo)
        end

        # Adapted From ChainRulesTestUtils._is_inferrable
        function is_inferrable(f, A)
            try
                @maybe_inferred f(A)
                return true
            catch ErrorException
                return false
            end
        end

        @testset "$(f)(::$TA{<:$T})" for f in (
                exp,
                log,
                sqrt,
                cos,
                sin,
                tan,
                cosh,
                sinh,
                tanh,
                acos,
                asin,
                atan,
                acosh,
                asinh,
                atanh,
            ),
            TA in (Symmetric, Hermitian),
            T in (TA <: Symmetric ? (Float64,) : (Float64, ComplexF64))

            TC = Complex{real(T)}

            n = 10
            @testset "frule" begin
                @testset for uplo in (:L, :U), hermout in (true, false)
                    A, ΔA = rand_matfun_input(f, TA, T, uplo, n, hermout),
                    TA(randn(T, n, n), uplo)
                    Y = f(A)
                    if is_inferrable(f, A)
                        Y_ad, ∂Y_ad = @maybe_inferred frule((ZeroTangent(), ΔA), f, A)
                    else
                        TY = T∂Y = if T <: Real
                            Union{Symmetric{Complex{T}},Symmetric{T}}
                        else
                            Union{Matrix{T},Hermitian{T}}
                        end
                        Y_ad, ∂Y_ad = @maybe_inferred Tuple{TY,T∂Y} frule(
                            (ZeroTangent(), ΔA), f, A
                        )
                    end
                    @test Y_ad == Y
                    @test typeof(Y_ad) === typeof(Y)
                    hasproperty(Y, :uplo) && @test Y_ad.uplo == Y.uplo
                    @test ∂Y_ad isa typeof(Y)
                    hasproperty(∂Y_ad, :uplo) && @test ∂Y_ad.uplo == Y.uplo
                    @test parent(∂Y_ad) ≈ jvp(
                        _fdm, x -> Matrix{TC}(parent(f(TA(x, uplo)))), (A.data, ΔA.data)
                    )
                end

                @testset "stable for (almost-)singular input" begin
                    λ, U = eigen(rand_matfun_input(f, TA, T, :U, n, true))
                    m = div(n, 2)
                    λ[1:m] .= λ[(m + 1):(2m)] .+ cbrt(eps(eltype(λ))) / 100
                    A = TA(U * Diagonal(λ) * U')
                    ΔA = TA(randn(T, n, n))
                    _, ∂Y = frule((ZeroTangent(), ΔA), f, A)
                    @test parent(∂Y) ≈
                        jvp(_fdm, x -> Matrix{TC}(parent(f(TA(x)))), (A.data, ΔA.data))

                    λ[1:m] .= λ[(m + 1):(2m)]
                    A2 = TA(U * Diagonal(λ) * U')
                    ΔA2 = TA(randn(T, n, n))
                    _, ∂Y2 = frule((ZeroTangent(), ΔA2), f, A2)
                    @test parent(∂Y2) ≈
                        jvp(_fdm, x -> Matrix{TC}(parent(f(TA(x)))), (A2.data, ΔA2.data))
                end

                f ∉ (log, sqrt, acosh) && @testset "low-rank matrix" begin
                    λ, U = eigen(rand_matfun_input(f, TA, T, :U, n, true))
                    λ[2:n] .= 0
                    A = TA(U * Diagonal(λ) * U')
                    ΔA = TA(randn(T, n, n))
                    _, ∂Y = frule((ZeroTangent(), ΔA), f, A)
                    @test parent(∂Y) ≈ jvp(
                        _fdm, x -> Matrix{TC}(parent(f(TA(x)))), (A.data, ΔA.data)
                    )
                end
            end

            @testset "rrule" begin
                @testset for uplo in (:L, :U), hermout in (true, false)
                    A = rand_matfun_input(f, TA, T, uplo, n, hermout)
                    Y = f(A)
                    ΔY = if Y isa Matrix
                        randn(eltype(Y), n, n)
                    else
                        typeof(Y)(randn(eltype(Y), n, n), Y.uplo)
                    end
                    if is_inferrable(f, A)
                        Y_ad, back = @maybe_inferred rrule(f, A)
                    else
                        TY = if T <: Real
                            Union{Symmetric{Complex{T}},Symmetric{T}}
                        else
                            Union{Matrix{T},Hermitian{T}}
                        end
                        Y_ad, back = @maybe_inferred Tuple{TY,Any} rrule(f, A)
                    end
                    @test Y_ad == Y
                    @test typeof(Y_ad) === typeof(Y)
                    hasproperty(Y, :uplo) && @test Y_ad.uplo == Y.uplo
                    ∂self, ∂A = @maybe_inferred back(ΔY)
                    @test ∂self === NoTangent()
                    @test ∂A isa typeof(A)
                    @test ∂A.uplo == A.uplo
                    # check pullback composes correctly
                    ∂data = rrule(Hermitian, A.data, uplo)[2](∂A)[2]
                    @test ∂data ≈ j′vp(_fdm, x -> parent(f(TA(x, uplo))), ΔY, A.data)[1]

                    # check works correctly even when cotangent is different type than Y
                    ΔY2 = randn(Complex{real(T)}, n, n)
                    _, ∂A2 = back(ΔY2)
                    ∂data2 = rrule(Hermitian, A.data, uplo)[2](∂A2)[2]
                    @test ∂data2 ≈ j′vp(
                        _fdm, x -> Matrix{Complex{real(T)}}(f(TA(x, uplo))), ΔY2, A.data
                    )[1]
                end

                @testset "stable for (almost-)singular input" begin
                    λ, U = eigen(rand_matfun_input(f, TA, T, :U, n, true))
                    m = div(n, 2)
                    λ[1:m] .= λ[(m + 1):(2m)] .+ cbrt(eps(eltype(λ))) / 100
                    A = TA(U * Diagonal(λ) * U')
                    ΔY = TA(randn(T, n, n))
                    ∂A = rrule(f, A)[2](ΔY)[2]
                    ∂data = rrule(Hermitian, A.data, :U)[2](∂A)[2]
                    @test ∂data ≈ j′vp(_fdm, x -> parent(f(TA(x))), ΔY, A.data)[1]

                    λ[1:m] .= λ[(m + 1):(2m)]
                    A2 = TA(U * Diagonal(λ) * U')
                    ΔY2 = TA(randn(T, n, n))
                    ∂A2 = rrule(f, A2)[2](ΔY2)[2]
                    ∂data2 = rrule(Hermitian, A2.data, :U)[2](∂A2)[2]
                    @test ∂data2 ≈ j′vp(_fdm, x -> parent(f(TA(x))), ΔY2, A2.data)[1]
                end

                f ∉ (log, sqrt, acosh) && @testset "low-rank matrix" begin
                    λ, U = eigen(rand_matfun_input(f, TA, T, :U, n, true))
                    λ[2:n] .= 0
                    A = TA(U * Diagonal(λ) * U')
                    ΔY = TA(randn(T, n, n))
                    ∂A = rrule(f, A)[2](ΔY)[2]
                    ∂data = rrule(Hermitian, A.data, :U)[2](∂A)[2]
                    @test ∂data ≈ j′vp(_fdm, x -> parent(f(TA(x))), ΔY, A.data)[1]
                end
            end
        end

        @testset "sincos(::$TA{<:$T})" for TA in (Symmetric, Hermitian),
            T in (TA <: Symmetric ? (Float64,) : (Float64, ComplexF64))

            n = 10
            @testset "frule" begin
                @testset for uplo in (:L, :U)
                    A, ΔA = TA(randn(T, n, n), uplo), TA(randn(T, n, n), uplo)
                    Y = sincos(A)
                    sinA, cosA = Y
                    Y_ad, ∂Y_ad = @maybe_inferred frule((ZeroTangent(), ΔA), sincos, A)
                    @test Y_ad == Y
                    @test typeof(Y_ad) === typeof(Y)
                    @test Y_ad[1].uplo === Y[1].uplo
                    @test Y_ad[2].uplo === Y[2].uplo

                    @test ∂Y_ad isa Tangent{typeof(Y)}
                    ∂Y_ad2 = Tangent{typeof(Y)}(
                        frule((ZeroTangent(), ΔA), sin, A)[2],
                        frule((ZeroTangent(), ΔA), cos, A)[2],
                    )
                    # not exact because evaluated in a more efficient way
                    ChainRulesTestUtils.test_approx(∂Y_ad, ∂Y_ad2)
                end
            end

            @testset "rrule" begin
                @testset for uplo in (:L, :U)
                    A = TA(randn(T, n, n), uplo)
                    Y = sincos(A)
                    sinA, cosA = Y
                    ΔsinA = typeof(sinA)(randn(T, n, n), sinA.uplo)
                    ΔcosA = typeof(cosA)(randn(T, n, n), cosA.uplo)
                    Y_ad, back = @maybe_inferred rrule(sincos, A)
                    @test Y_ad == Y
                    @test typeof(Y_ad) === typeof(Y)
                    @test Y_ad[1].uplo === Y[1].uplo
                    @test Y_ad[2].uplo === Y[2].uplo

                    ΔY = Tangent{typeof(Y)}(ΔsinA, ΔcosA)
                    ∂self, ∂A = @maybe_inferred back(ΔY)
                    @test ∂self === NoTangent()
                    @test ∂A ≈ rrule(sin, A)[2](ΔsinA)[2] + rrule(cos, A)[2](ΔcosA)[2]

                    ΔY2 = Tangent{typeof(Y)}(ZeroTangent(), ZeroTangent())
                    @test @maybe_inferred(back(ΔY2)) === (NoTangent(), ZeroTangent())

                    ΔY3 = Tangent{typeof(Y)}(ΔsinA, ZeroTangent())
                    @test @maybe_inferred(back(ΔY3)) == rrule(sin, A)[2](ΔsinA)

                    ΔY4 = Tangent{typeof(Y)}(ZeroTangent(), ΔcosA)
                    @test @maybe_inferred(back(ΔY4)) == rrule(cos, A)[2](ΔcosA)
                end
            end
        end
    end
end
