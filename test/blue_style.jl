@testset "Blue Style" begin
    @testset "ops" begin
        # `//` and `^` are binary ops without whitespace around them
        @test fmt("1 // 2 + 3 ^ 4", style=BlueStyle()) == "1//2 + 3^4"
        @test fmt("a.//10", style=BlueStyle()) == "a .// 10"
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
        @test fmt(str_, style = BlueStyle()) == str

        str_ = """
        var = (arg1,
        arg2)
        """
        str = """
        var = (arg1, arg2)
        """
        @test fmt(str_, 4, 18, style = BlueStyle()) == str

        str_ = """
        var = {arg1,
        arg2}
        """
        str = """
        var = {
            arg1, arg2
        }
        """
        @test fmt(str_, 4, 17, style = BlueStyle()) == str
        @test fmt(str_, 4, 14, style = BlueStyle()) == str

        str_ = """
        var = call(arg1,
        arg2)
        """
        str = """
        var = call(
            arg1, arg2
        )
        """
        @test fmt(str_, 4, 14, style = BlueStyle()) == str

        str = """
        var = call(
            arg1,
            arg2,
        )
        """
        @test fmt(str_, 4, 13, style = BlueStyle()) == str

        str_ = """
        var = ref[arg1,
        arg2]
        """
        str = """
        var = ref[
            arg1, arg2
        ]
        """
        @test fmt(str_, 4, 14, style = BlueStyle()) == str

        str = """
        var = ref[
            arg1,
            arg2,
        ]
        """
        @test fmt(str_, 4, 13, style = BlueStyle()) == str

        str_ = """
        var = ABC{arg1,
        arg2}
        """
        str = """
        var = ABC{
            arg1,arg2
        }
        """
        @test fmt(str_, 4, 13, style = BlueStyle()) == str

        str = """
        var = ABC{
            arg1,
            arg2,
        }
        """
        @test fmt(str_, 4, 12, style = BlueStyle()) == str

        str_ = """
        var = @call(arg1,
        arg2)
        """
        str = """
        var = @call(
            arg1, arg2
        )
        """
        @test fmt(str_, 4, 14, style = BlueStyle()) == str

        str = """
        var = @call(
            arg1,
            arg2
        )
        """
        @test fmt(str_, 4, 13, style = BlueStyle()) == str

        str_ = """
        function long_name_of_function_because_i_am_writing_an_example(
            arg1, arg2, arg3, arg4, arg5, arg6
        )
            # code
        end
        """
        @test fmt(str_, 4, 38, style = BlueStyle()) == str_

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
        @test fmt(str_, 4, 37, style = BlueStyle()) == str

    end

    @testset "do not prepend return in `do` blocks" begin
        str = """
        map(arg1, arg2) do x, y
            expr1
            expr2
        end"""
        @test fmt(str, style = BlueStyle(), always_use_return = true) == str
    end

    @testset "separate kw args with semicolon" begin
        str_ = "xy = f(x, y=3)"
        str = "xy = f(x; y = 3)"
        @test fmt(str_, style = BlueStyle()) == str

        str_ = "xy = f(x=1, y=2)"
        str = "xy = f(; x = 1, y = 2)"
        @test fmt1(str_, style = BlueStyle()) == str
        @test fmt(str_, style = BlueStyle()) == str
        @test fmt(str, style = BlueStyle()) == str

        str_ = "xy = f(x=1; y=2)"
        @test fmt1(str_, style = BlueStyle()) == str
        @test fmt(str_, style = BlueStyle()) == str

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
        @test fmt1(str, style = BlueStyle()) == str
        @test fmt(str, style = BlueStyle()) == str
    end
end
