@testset "Blue Style" begin
    # @testset "ops" begin
    #     # `//` and `^` are binary ops without whitespace around them
    #     @test bluefmt("1 // 2 + 3 ^ 4") == "1 // 2 + 3 ^ 4"
    #     @test bluefmt("1 // 2 + 3 ^ 4") == "1//2 + 3^4"
    #     @test bluefmt("a .// 10") == "a .// 10"
    #     @test bluefmt("a.//10") == "a .// 10"
    # end

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
            arg1,arg2
        }
        """
        @test bluefmt(str_, 4, 16) == str
        @test bluefmt(str_, 4, 13) == str

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



        str_ = """
        df[:, :some_column] = [some_big_function_name(blahhh) for (fooooo, blahhh) in my_long_list_of_vars]
        """
        str = """
        df[:, :some_column] = [
            some_big_function_name(blahhh) for (fooooo, blahhh) in my_long_list_of_vars
        ]
        """
        @test bluefmt(str_, 4, 92) == str

        str_ = """
        function f()
            for i = 1:n
         @inbounds mul!(
             reshape(view(C, :, i), eye_n, k),
             reshape(view(B, :, i), eye_n, l),
             transpose(A, B),
         )
            end
        end
        """
        str = """
        function f()
            for i = 1:n
                @inbounds mul!(
                    reshape(view(C, :, i), eye_n, k), reshape(view(B, :, i), eye_n, l), transpose(A, B)
                )
            end
        end
        """
        @test bluefmt(str_, 4, 95) == str

        str = """
        function f()
            for i = 1:n
                @inbounds mul!(
                    reshape(view(C, :, i), eye_n, k),
                    reshape(view(B, :, i), eye_n, l),
                    transpose(A, B),
                )
            end
        end
        """
        @test bluefmt(str_, 4, 94) == str

        str = """
        function f()
            for i = 1:n
                @inbounds mul!(
                    reshape(
                        view(
                            C, :, i
                        ),
                        eye_n,
                        k,
                    ),
                    reshape(
                        view(
                            B, :, i
                        ),
                        eye_n,
                        l,
                    ),
                    transpose(A, B),
                )
            end
        end
        """
        @test bluefmt(str_, 4, 28) == str

        str = """
        function f()
            for i = 1:n
                @inbounds mul!(
                    reshape(
                        view(
                            C, :, i
                        ),
                        eye_n,
                        k,
                    ),
                    reshape(
                        view(
                            B, :, i
                        ),
                        eye_n,
                        l,
                    ),
                    transpose(
                        A, B
                    ),
                )
            end
        end
        """
        @test bluefmt(str_, 4, 27) == str
    end


    @testset "do not nest assignments if the RHS is iterable" begin
        str_ = """
        var = foo(
            map(arr) do x
                x * 10
            end, "")
        """
        str = """
        var = foo(
            map(
                arr,
            ) do x
                x *
                10
            end,
            "",
        )
        """
        @test bluefmt(str_, 4, 1) == str
    end

    @testset "no chained ternary allowed !!!" begin
        str = """
        E1 ? A : B
        """
        @test bluefmt(str) == str

        str_ = """
        E1 ? A : E2 ? B : C
        """
        str = """
        if E1
            A
        elseif E2
            B
        else
            C
        end
        """
        @test bluefmt(str_) == str
    end

    @testset "use `return nothing` instead of `return`" begin
        str_ = """
        function foo()
            return
        end
        """
        str = """
        function foo()
            return nothing
        end
        """
        @test bluefmt(str_) == str

        str_ = "a || return"
        str = "a || return nothing"
        @test bluefmt(str_) == str
    end
end
