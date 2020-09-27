@testset "Blue Style" begin
    @testset "ops" begin
        # `//` and `^` are binary ops without whitespace around them
        @test bluefmt("1 // 2 + 3 ^ 4") == "1//2 + 3^4"
        @test bluefmt("a.//10") == "a .// 10"
    end

    @testset "nest to one line" begin
        str_ = """
        var = [arg1, #com
        arg2]
        """
        str = """
        var = [
            arg1, #com
            arg2,
        ]
        """
        @test bluefmt(str_) == str

        str_ = """
        var = (arg1,
        arg2)
        """
        str = """
        var = (arg1, arg2)
        """
        @test bluefmt(str_, 4, 18) == str

        str_ = """
        var = {arg1,
        arg2}
        """
        str = """
        var = {
            arg1, arg2
        }
        """
        @test bluefmt(str_, 4, 17) == str
        @test bluefmt(str_, 4, 14) == str

        str_ = """
        var = call(arg1,
        arg2)
        """
        str = """
        var = call(
            arg1, arg2
        )
        """
        @test bluefmt(str_, 4, 14) == str

        str = """
        var = call(
            arg1,
            arg2,
        )
        """
        @test bluefmt(str_, 4, 13) == str

        str_ = """
        var = ref[arg1,
        arg2]
        """
        str = """
        var = ref[
            arg1, arg2
        ]
        """
        @test bluefmt(str_, 4, 14) == str

        str = """
        var = ref[
            arg1,
            arg2,
        ]
        """
        @test bluefmt(str_, 4, 13) == str

        str_ = """
        var = ABC{arg1,
        arg2}
        """
        str = """
        var = ABC{
            arg1,arg2
        }
        """
        @test bluefmt(str_, 4, 13) == str

        str = """
        var = ABC{
            arg1,
            arg2,
        }
        """
        @test bluefmt(str_, 4, 12) == str

        str_ = """
        var = @call(arg1,
        arg2)
        """
        str = """
        var = @call(
            arg1, arg2
        )
        """
        @test bluefmt(str_, 4, 14) == str

        str = """
        var = @call(
            arg1,
            arg2
        )
        """
        @test bluefmt(str_, 4, 13) == str

        str_ = """
        function long_name_of_function_because_i_am_writing_an_example(
            arg1, arg2, arg3, arg4, arg5, arg6
        )
            # code
        end
        """
        @test bluefmt(str_, 4, 38) == str_

        str = """
        function long_name_of_function_because_i_am_writing_an_example(
            arg1,
            arg2,
            arg3,
            arg4,
            arg5,
            arg6,
        )
            # code
        end
        """
        @test bluefmt(str_, 4, 37) == str

        str = """
        Dict(
            "options" => join(
                imap(Iterators.filter(keep_option, connection_options)) do (k, v)
                    "-c k=(show_option(v))"
                end,
                " ",
            ),
        )
        """
        @test bluefmt(str, 4, 92) == str

        str_ = """
        var = foo(
            map(arr) do x
                x * 10
            end, "")
        """
        str = """
        var = foo(
            map(arr) do x
                x * 10
            end,
            "",
        )
        """
        @test bluefmt(str_, 4, 92) == str
    end

    @testset "do not prepend return in `do` blocks" begin
        str = """
        map(arg1, arg2) do x, y
            expr1
            expr2
        end"""
        @test bluefmt(str, always_use_return = true) == str
    end

    @testset "separate kw args with semicolon" begin
        str_ = "xy = f(x, y=3)"
        str = "xy = f(x; y = 3)"
        @test bluefmt(str_) == str

        str_ = "xy = f(x=1, y=2)"
        str = "xy = f(; x = 1, y = 2)"
        @test bluefmt1(str_) == str
        @test bluefmt(str_) == str
        @test bluefmt(str) == str

        str_ = "xy = f(x=1; y=2)"
        @test bluefmt1(str_) == str
        @test bluefmt(str_) == str

        str = """
        function g(x, y = 1)
            return x + y
        end
        macro h(x, y = 1)
            nothing
        end
        shortdef1(MatrixT, VectorT = nothing) = nothing
        shortdef2(MatrixT, VectorT = nothing) where {T} = nothing
        """
        @test bluefmt1(str) == str
        @test bluefmt(str) == str
    end
end
