@testset "Github Issues" begin
    @testset "Splitpath issue" begin
        # TODO(odow): seet the TODO in src/JuliaFormatter.jl. Remove once
        # JuliaFormatter.jl drops support for Julia 1.0.
        dirs = JuliaFormatter.splitpath(@__DIR__)
        @test length(dirs) > 2
        @test dirs[end] == "test"
        @test occursin("JuliaFormatter", dirs[end-1])
    end

    @testset "issue #137" begin
        str = """
        (
            let x = f() do
                    body
                end
                x
            end for x in xs
        )"""
        str_ = """
        (
               let x = f() do
                       body
                   end
                   x
               end for x in xs
         )"""
        @test fmt(str_) == str

        str = """
        (
            let
                x = f() do
                    body
                end
                x
            end for x in xs
        )"""
        str_ = """
        (
          let
              x = f() do
                  body
              end
              x
          end for x in xs)"""
        @test fmt(str_) == str

        str = """
        let n = try
                ..
            catch
                ..
            end
            ..
        end"""
        @test fmt(str) == str

        str = """
        let n = let
                ..
            end
            ..
        end"""
        @test fmt(str) == str

        str = """
        let n = begin
                ..
            end
            ..
        end"""
        @test fmt(str) == str
    end

    @testset "multiline / issue 139" begin
        str_ = """
        m = match(r\"""
                  (
                      pattern1 |
                      pattern2 |
                      pattern3
                  )
                  \"""x, aaa, str)"""
        str = """
        m = match(
            r\"""
            (
                pattern1 |
                pattern2 |
                pattern3
            )
            \"""x,
            aaa,
            str,
        )"""
        @test fmt(str_) == str

        str_ = """
        m = match(r```
                  (
                      pattern1 |
                      pattern2 |
                      pattern3
                  )
                  ```x, aaa, str)"""
        str = """
        m = match(
            r```
            (
                pattern1 |
                pattern2 |
                pattern3
            )
            ```x,
            aaa,
            str,
        )"""
        @test fmt(str_) == str

        str_ = """
        y = similar([
            1
            2
            3
        ], (4, 5))"""
        str = """
        y = similar(
            [
                1
                2
                3
            ],
            (4, 5),
        )"""
        @test fmt(str_) == str

        str_ = """
        y = similar(T[
            1
            2
            3
        ], (4, 5))"""
        str = """
        y = similar(
            T[
                1
                2
                3
            ],
            (4, 5),
        )"""
        @test fmt(str_) == str
    end

    @testset "issue #150" begin
        str_ = "const SymReg{B,MT} = ArrayReg{B,Basic,MT} where {MT <:AbstractMatrix{Basic}}"
        str = "const SymReg{B,MT} = ArrayReg{B,Basic,MT} where {MT<:AbstractMatrix{Basic}}"
        @test fmt(str_, whitespace_typedefs = false) == str

        str = "const SymReg{B, MT} = ArrayReg{B, Basic, MT} where {MT <: AbstractMatrix{Basic}}"
        @test fmt(str_, whitespace_typedefs = true) == str
    end

    @testset "issue #170 - block within comprehension" begin
        str_ = """
        ys = ( if p1(x)
                 f1(x)
        elseif p2(x)
            f2(x)
        else
            f3(x)
        end for    x in xs)
        """
        str = """
        ys = (
            if p1(x)
                f1(x)
            elseif p2(x)
                f2(x)
            else
                f3(x)
            end for x in xs
        )
        """
        @test fmt(str_) == str

        str = """
        ys = map(xs) do x
            if p1(x)
                f1(x)
            elseif p2(x)
                f2(x)
            else
                f3(x)
            end
        end
        """
        @test fmt(str) == str

        str_ = """
        y1 = Any[if true
            very_very_very_very_very_very_very_very_very_very_very_very_very_very_very_very_long_expr
        end for i in 1:1]"""
        str = """
        y1 = Any[
            if true
                very_very_very_very_very_very_very_very_very_very_very_very_very_very_very_very_long_expr
            end for i = 1:1
        ]"""
        @test fmt(str_) == str
        _, s = run_nest(str_, 100)
        @test s.line_offset == 1

        str_ = """
        y1 = [if true
            very_very_very_very_very_very_very_very_very_very_very_very_very_very_very_very_long_expr
        end for i in 1:1]"""
        str = """
        y1 = [
            if true
                very_very_very_very_very_very_very_very_very_very_very_very_very_very_very_very_long_expr
            end for i = 1:1
        ]"""
        @test fmt(str_) == str
        _, s = run_nest(str_, 100)
        @test s.line_offset == 1

        str_ = """
        y1 = [if true
            short_expr
        end for i in 1:1]"""
        str = """
        y1 = [
            if true
                short_expr
            end for i = 1:1
        ]"""
        @test fmt(str_) == str
        _, s = run_nest(str_, 100)
        @test s.line_offset == 1
    end

    @testset "issue #183" begin
        str_ = """
        function f(args...)

            next!(s.progress;
            # comment
            )
            nothing
        end"""
        str = """
        function f(args...)

            next!(
                s.progress,
                # comment
            )
            nothing
        end"""
        @test fmt(str_) == str
    end

    @testset "issue #189" begin
        str_ = """
    D2 = [
            (b_hat * y - delta_hat[i] * y) * gamma[i] + (b * y_hat - delta[i] * y_hat) *
                                                            gamma_hat[i] + (b_hat - y_hat) *
                                                                           delta[i] + (b - y) *
                                                                                      delta_hat[i] - delta[i] * delta_hat[i]
            for i = 1:8
        ]"""
        str = """
        D2 = [
            (b_hat * y - delta_hat[i] * y) * gamma[i] +
            (b * y_hat - delta[i] * y_hat) * gamma_hat[i] +
            (b_hat - y_hat) * delta[i] +
            (b - y) * delta_hat[i] - delta[i] * delta_hat[i] for i = 1:8
        ]"""
        @test fmt(str_) == str
    end

    @testset "issue #193" begin
        str = """
        module Module
        # comment
        end"""
        @test fmt(str) == str

        str = """
        module Module
        # comment
        @test
        # comment
        end"""
        @test fmt(str) == str
    end

    @testset "issue #194" begin
        str_ = """
        function mystr( str::String )
        return SubString( str, 1:
        3 )
        end"""
        str = """
        function mystr(str::String)
            return SubString(str, 1:3)
        end"""
        @test fmt(str_) == str
    end

    @testset "issue #200" begin
        str_ = """
        begin
            f() do
                @info @sprintf \"\"\"
                Δmass   = %.16e\"\"\" abs(weightedsum(Q) - weightedsum(Qe)) / weightedsum(Qe)
            end
        end"""

        # NOTE: this looks slightly off because we're compensating for escaping quotes
        str = """
        begin
            f() do
                @info @sprintf \"\"\"
                Δmass   = %.16e\"\"\" abs(weightedsum(Q) - weightedsum(Qe)) /
                                   weightedsum(Qe)
            end
        end"""
        @test fmt(str_, m = 81) == str
        @test fmt(str, m = 82) == str_
    end

    @testset "issue #202" begin
        str_ = """
        @inline function _make_zop_getvalues(iterators)
            types = map(iterators) do itr
                t =     constructorof(typeof(itr))::Union{Iterators.ProductIterator,CartesianIndices}
                Val(t)
            end
            return function (xs) end
        end"""
        str = """
        @inline function _make_zop_getvalues(iterators)
            types = map(iterators) do itr
                t = constructorof(typeof(itr))::Union{Iterators.ProductIterator,CartesianIndices}
                Val(t)
            end
            return function (xs) end
        end"""
        @test fmt(str_, m = 92) == str

        str_ = """
        @vlplot(
            data = dataset("cars"),
            facet = {row = {field = :Origin, type = :nominal}},
            spec = {
                layer = [
                    {
                        mark = :point,
                        encoding =     {x = {field = :Horsepower}, y = {field = :Miles_per_Gallon}},
                    },
                    {
                        mark = {type = :rule, color = :red},
                        data = {values = [{ref = 10}]},
                        encoding = {y = {field = :ref, type = :quantitative}},
                    },
                ],
            }
        )"""
        str = """
        @vlplot(
            data = dataset("cars"),
            facet = {row = {field = :Origin, type = :nominal}},
            spec = {
                layer = [
                    {
                        mark = :point,
                        encoding = {x = {field = :Horsepower}, y = {field = :Miles_per_Gallon}},
                    },
                    {
                        mark = {type = :rule, color = :red},
                        data = {values = [{ref = 10}]},
                        encoding = {y = {field = :ref, type = :quantitative}},
                    },
                ],
            }
        )"""
        @test fmt(str_, m = 92) == str
    end

    @testset "issue #207" begin
        str_ = """
        @traitfn function predict_ar(m::TGP, p::Int = 3, n::Int = 1; y_past = get_y(m)) where {T,TGP<:AbstractGP{T};IsMultiOutput{TGP}}
        end"""

        str = """
        @traitfn function predict_ar(
            m::TGP,
            p::Int = 3,
            n::Int = 1;
            y_past = get_y(m),
        ) where {T,TGP<:AbstractGP{T};IsMultiOutput{TGP}} end"""
        @test fmt(str_, m = 92) == str

        str = """
        @traitfn function predict_ar(
            m::TGP,
            p::Int = 3,
            n::Int = 1;
            y_past = get_y(m),
        ) where {T, TGP <: AbstractGP{T}; IsMultiOutput{TGP}} end"""
        @test fmt(str_, m = 92, whitespace_typedefs = true) == str

        str_ = """
        @traitfn function predict_ar(m::TGP, p::Int = 3, n::Int = 1; y_past = get_y(m)) where C <: Union{T,TGP<:AbstractGP{T};IsMultiOutput{TGP}}
        end"""

        str = """
        @traitfn function predict_ar(
            m::TGP,
            p::Int = 3,
            n::Int = 1;
            y_past = get_y(m),
        ) where {C<:Union{T,TGP<:AbstractGP{T};IsMultiOutput{TGP}}} end"""
        @test fmt(str_, m = 92) == str

        str = """
        @traitfn function predict_ar(
            m::TGP,
            p::Int = 3,
            n::Int = 1;
            y_past = get_y(m),
        ) where {C <: Union{T, TGP <: AbstractGP{T}; IsMultiOutput{TGP}}} end"""
        @test fmt(str_, m = 92, whitespace_typedefs = true) == str
    end

    @testset "issue #218" begin
        str_ = raw"""
        for MT in GROUP_MANIFOLD_BASIS_DISAMBIGUATION
            eval(quote
                @invoke_maker 1 Manifold get_vector(M::$MT, e::Identity, X, B::VeeOrthogonalBasis)
            end)
        end"""
        str = raw"""
        for MT in GROUP_MANIFOLD_BASIS_DISAMBIGUATION
            eval(
                quote
                    @invoke_maker 1 Manifold get_vector(
                        M::$MT,
                        e::Identity,
                        X,
                        B::VeeOrthogonalBasis,
                    )
                end,
            )
        end"""
        @test fmt1(str_) == str
        @test fmt(str_) == str
    end

    @testset "issue 248" begin
        str_ = """
        var = call(a, @macrocall b)"""
        str = """
        var =
            call(
                a,
                @macrocall b
            )"""
        @test fmt(str_, 4, 1) == str
    end

    @testset "issue 260 - BracesCat" begin
        str = "{1; 2; 3}"
        @test fmt(str, 4, length(str)) == str

        str_ = "{1; 2; 3}"
        str = """
        {
          1;
          2;
          3;
        }"""
        @test fmt(str_, 2, length(str_) - 1) == str
        @test fmt(str, 2, length(str_)) == str_
    end

    @testset "issue 262 - removal of @ in nested macrocall" begin
        str = raw":($(@__MODULE__).@macro)"
        @test fmt(str) == str

        str = raw":($(@__MODULE__).property)"
        @test fmt(str) == str

        str = raw":($(@__MODULE__))"
        @test fmt(str) == str

        str = raw":($(@__MODULE__).@field.macro)"
        @test fmt(str) == str

        str_ = raw":($(@__MODULE__.macro).@field.macro)"
        str = raw":($(__MODULE__.@macro).@field.macro)"
        @test fmt(str_) == str
        @test fmt(str) == str

        str_ = raw"@a.b.c"
        str = raw"a.b.@c"
        @test fmt(str_) == str
        @test fmt(str) == str

        str_ = raw"@a.b.c"
        str = raw"a.b.@c"
        @test fmt(str_) == str
        @test fmt(str) == str

        str = raw"a.@b.c"
        @test fmt(str) == str

        str = raw"a.@b.c.d"
        @test fmt(str) == str

        str = raw"a.b.@c.d"
        @test fmt(str) == str

        str_ = raw"@a.b.c.d"
        str = raw"a.b.c.@d"
        @test fmt(str_) == str
        @test fmt(str) == str
    end

    @testset "issue 264 - `let` empty block body" begin
        str_ = "let; end"
        str = """
        let
        end"""
        @test fmt(str_) == str
    end

    @testset "issue 268 - whitespace around dot op if LHS is number literal" begin
        str = "xs[-5 .<= xs .& xs .<= 5]"
        @test fmt(str) == str
        str_ = "xs[(-5 .<= xs) .& (xs .<= 5)]"
        str = "xs[(-5 .<= xs).&(xs.<=5)]"
        @test fmt(str_) == str
    end

    @testset "issue 277 - flatten ops when no whitespace is allowed" begin
        # previously this would remove |>
        str_ = "get_actions(env)[env |> π.learner |> π.explorer]"
        str = "get_actions(env)[env|>π.learner|>π.explorer]"
        @test fmt(str_) == str
        @test fmt(str, whitespace_ops_in_indices = true) == str_
    end

    @testset "issue 286 - Float32 leading/trailing zeros" begin
        str_ = """
        a = 3.f0
        b = 3f0
        c = 30f0
        d = 30.0f0
        e = 30.123f0
        f = .123f0
        """
        str = """
        a = 3.0f0
        b = 3.0f0
        c = 30.0f0
        d = 30.0f0
        e = 30.123f0
        f = 0.123f0
        """
        @test fmt(str_) == str
    end

    @testset "issue 289 - no spaces/nesting for matrix elements" begin
        str_ = """
        A =  [0. 1 0 0
           -k/Jm -c/Jm k/Jm c/Jm
            0 0 0 1
            f(1,2) c/Ja -k/Ja -c/Ja]
        """

        str = """
        A =
            [
                0.0 1 0 0
                -k/Jm -c/Jm k/Jm c/Jm
                0 0 0 1
                f(1, 2) c/Ja -k/Ja -c/Ja
            ]
        """
        @test fmt(str_, 4, 1) == str
    end

    @testset "issue 317 - infinite recursion" begin
        str = raw"""
        SUITE["manifolds"][name]["tv = 2 * tv1 + 3 * tv2"] = @benchmarkable $tv =
            2 * $tv1 + 3 * $tv2
        """
        @test format_text(str, BlueStyle()) == str
    end

    @testset "issue 324 - bounds error when aligning binary op calls" begin
        # caused by the star operator
        str = """
        θ = eigvals(Matrix([0I(n^2) -I(n^2); P0 P1]), -Matrix([I(n^2) 0I(n^2); 0I(n^2) P2]))
        c = maximum(abs.(θ[(imag.(θ).==0).*(real.(θ).>0)]))
        """
        @test format_text(str, align_assignment = true) == str
    end

    @testset "issue 332" begin
        # this string has a nbsp after 'c'
        # so it should have an additional byte because
        # it's unicode
        str_ = """a = b || c ;
               f("A")"""
        str = """a = b || c;
               f("A")"""
        @test format_text(str_) == str
    end

    @testset "issue 336" begin
        str_ = """
        nzthis = _hessian_slice(d, ex, x, H, obj_factor, nzcount, recovery_tmp_storage, Val{1})::Int
        """
        str = """
        nzthis = _hessian_slice(
            d,
            ex,
            x,
            H,
            obj_factor,
            nzcount,
            recovery_tmp_storage,
            Val{1},
        )::Int
        """
        @test fmt(str_, 4, 80) == str
    end

    @testset "issue 375" begin
        s = raw"conflictstatus = @jimport ilog.cp.IloCP$ConflictStatus"
        @test fmt(s) == s

        s = raw"conflictstatus = @jimport ilog.cp.IloCP$ConflictStatus"
        @test bluefmt(s) == s
    end

    @testset "issue 352" begin
        str_ = """
                      @inbounds for f in 1:n_freqs, m in 1:n_channels, l in 1:n_channels, k in 1:length(weighted_evals)
                         a = f + m + l + k
                      end"""
        str = """
        @inbounds for f in 1:n_freqs,
                      m in 1:n_channels,
                      l in 1:n_channels,
                      k in 1:length(weighted_evals)

            a = f + m + l + k
        end"""
        @test yasfmt(str_, always_for_in = true) == str

        str_ = """
        using Test

        @testset "A long testset name that is rather long" for variable in 100:200, other_var in 1:100
            @test true
        end
        """
        str = """
        using Test

        @testset "A long testset name that is rather long" for variable in 100:200,
            other_var in 1:100

            @test true
        end
        """
        @test bluefmt(str_, always_for_in = true) == str
    end

    @testset "issue 387" begin
        str_ = """new{T1,T2}(arg1,arg2)"""
        str = """
        new{T1,
            T2}(arg1,
                arg2)"""
        @test yasfmt(str_, 4, 1) == str
    end
end
