@testset "Format Options" begin
    @testset "whitespace in typedefs" begin
        str_ = "Foo{A,B,C}"
        str = "Foo{A, B, C}"
        @test fmt(str_, whitespace_typedefs = true) == str

        str_ = """
        struct Foo{A<:Bar,Union{B<:Fizz,C<:Buzz},<:Any}
            a::A
        end"""
        str = """
        struct Foo{A <: Bar, Union{B <: Fizz, C <: Buzz}, <:Any}
            a::A
        end"""
        @test fmt(str_, whitespace_typedefs = true) == str

        str_ = """
        function foo() where {A,B,C{D,E,F{G,H,I},J,K},L,M<:N,Y>:Z}
            body
        end
        """
        str = """
        function foo() where {A, B, C{D, E, F{G, H, I}, J, K}, L, M <: N, Y >: Z}
            body
        end
        """
        @test fmt(str_, whitespace_typedefs = true) == str

        str_ = "foo() where {A,B,C{D,E,F{G,H,I},J,K},L,M<:N,Y>:Z} = body"
        str = "foo() where {A, B, C{D, E, F{G, H, I}, J, K}, L, M <: N, Y >: Z} = body"
        @test fmt(str_, whitespace_typedefs = true) == str
    end

    @testset "whitespace ops in indices" begin
        str = "arr[1 + 2]"
        @test fmt("arr[1+2]", m = 1, whitespace_ops_in_indices = true) == str

        str = "arr[(1 + 2)]"
        @test fmt("arr[(1+2)]", m = 1, whitespace_ops_in_indices = true) == str

        str_ = "arr[1:2*num_source*num_dump-1]"
        str = "arr[1:(2 * num_source * num_dump - 1)]"
        @test fmt(str_, m = 1, whitespace_ops_in_indices = true) == str

        str_ = "arr[2*num_source*num_dump-1:1]"
        str = "arr[(2 * num_source * num_dump - 1):1]"
        @test fmt(str_, m = 1, whitespace_ops_in_indices = true) == str

        str = "arr[(a + b):c]"
        @test fmt("arr[(a+b):c]", m = 1, whitespace_ops_in_indices = true) == str

        str = "arr[a in b]"
        @test fmt(str, m = 1, whitespace_ops_in_indices = true) == str

        str_ = "a:b+c:d-e"
        str = "a:(b + c):(d - e)"
        @test fmt(str_, m = 1, whitespace_ops_in_indices = true) == str

        # issue 180
        str_ = "s[m+i+1]"
        str = "s[m+i+1]"
        @test fmt(str, m = 1) == str

        str = "s[m + i + 1]"
        @test fmt(str_, m = 1, whitespace_ops_in_indices = true) == str
    end

    @testset "rewrite import to using" begin
        str_ = "import A"
        str = "using A: A"
        @test fmt(str_, import_to_using = true) == str

        str_ = """
        import A,

        B, C"""
        str = """
        using A: A
        using B: B
        using C: C"""
        @test_broken fmt(str_, import_to_using = true) == str

        str_ = """
        import A,
               # comment
        B, C"""
        str = """
        using A: A
        # comment
        using B: B
        using C: C"""
        @test fmt(str_, import_to_using = true) == str

        str_ = """
        import A, # inline
               # comment
        B, C # inline"""
        str = """
        using A: A # inline
        # comment
        using B: B
        using C: C # inline"""
        @test fmt(str_, import_to_using = true) == str

        str_ = """
        import ..A, .B, ...C"""
        str = """
        using ..A: A
        using .B: B
        using ...C: C"""
        @test fmt(str_, import_to_using = true) == str
        t = run_pretty(str_, opts = Options(max_margin = 80, import_to_using = true))
        @test t.len == 13

        # #232
        str = """import A.b"""
        @test fmt(str, import_to_using = true) == str
    end

    @testset "always convert `=` to `in` (for loops)" begin
        str_ = """
        for i = 1:n
            println(i)
        end"""
        str = """
        for i in 1:n
            println(i)
        end"""
        @test fmt(str_, always_for_in = true) == str
        @test fmt(str, always_for_in = true) == str

        str_ = """
        for i = I1, j in I2
            println(i, j)
        end"""
        str = """
        for i in I1, j in I2
            println(i, j)
        end"""
        @test fmt(str_, always_for_in = true) == str
        @test fmt(str, always_for_in = true) == str

        str_ = """
        for i = 1:30, j = 100:-2:1
            println(i, j)
        end"""
        str = """
        for i in 1:30, j in 100:-2:1
            println(i, j)
        end"""
        @test fmt(str_, always_for_in = true) == str
        @test fmt(str, always_for_in = true) == str

        str_ = "[(i,j) for i=I1,j=I2]"
        str = "[(i, j) for i in I1, j in I2]"
        @test fmt(str_, always_for_in = true) == str
        @test fmt(str, always_for_in = true) == str

        str_ = "((i,j) for i=I1,j=I2)"
        str = "((i, j) for i in I1, j in I2)"
        @test fmt(str_, always_for_in = true) == str
        @test fmt(str, always_for_in = true) == str

        str_ = "[(i, j) for i = 1:2:10, j = 100:-1:10]"
        str = "[(i, j) for i in 1:2:10, j in 100:-1:10]"
        @test fmt(str_, always_for_in = true) == str
        @test fmt(str, always_for_in = true) == str
    end

    @testset "rewrite x |> f to f(x)" begin
        @test fmt("x |> f", pipe_to_function_call = true) == "f(x)"

        str_ = "var = func1(arg1) |> func2 |> func3 |> func4 |> func5"
        str = "var = func5(func4(func3(func2(func1(arg1)))))"
        @test fmt(str_, pipe_to_function_call = true) == str
        @test fmt(str_, pipe_to_function_call = true, margin = 1) == fmt(str)
    end

    @testset "function shortdef to longdef" begin
        str_ = "foo(a) = bodybodybody"
        str = """
        function foo(a)
            bodybodybody
        end"""
        @test fmt(str_, 4, length(str_), short_to_long_function_def = true) == str_
        @test fmt(str_, 4, length(str_) - 1, short_to_long_function_def = true) == str

        str_ = "foo(a::T) where {T} = bodybodybodybodybodybodyb"
        str = """
        function foo(a::T) where {T}
            bodybodybodybodybodybodyb
        end"""
        @test fmt(str_, 4, length(str_), short_to_long_function_def = true) == str_
        @test fmt(str_, 4, length(str_) - 1, short_to_long_function_def = true) == str

        str_ = "foo(a::T)::R where {T} = bodybodybodybodybodybodybody"
        str = """
        function foo(a::T)::R where {T}
            bodybodybodybodybodybodybody
        end"""
        @test fmt(str_, 4, length(str_), short_to_long_function_def = true) == str_
        @test fmt(str_, 4, length(str_) - 1, short_to_long_function_def = true) == str
    end

    @testset "always use return" begin
        str_ = "foo(a) = bodybodybody"
        str = """
        function foo(a)
            return bodybodybody
        end"""
        @test fmt(
            str_,
            4,
            length(str_) - 1,
            short_to_long_function_def = true,
            always_use_return = true,
        ) == str

        str_ = """
        function foo()
            expr1
            expr2
        end"""
        str = """
        function foo()
            expr1
            return expr2
        end"""
        @test fmt(str_, 4, length(str_) - 1, always_use_return = true) == str

        str_ = """
        macro foo()
            expr1
            expr2
        end"""
        str = """
        macro foo()
            expr1
            return expr2
        end"""
        @test fmt(str_, 4, length(str_) - 1, always_use_return = true) == str

        str_ = """
        map(arg1, arg2) do x, y
            expr1
            expr2
        end"""
        str = """
        map(arg1, arg2) do x, y
            expr1
            return expr2
        end"""
        @test fmt(str_, 4, length(str_) - 1, always_use_return = true) == str
    end

    @testset "whitespace in keyword arguments" begin
        str_ = "f(; a = b)"
        str = "f(; a=b)"
        @test fmt(str_, 4, 92, whitespace_in_kwargs = false) == str

        str = "f(; a!) = a!"
        @test fmt(str, 4, 92, whitespace_in_kwargs = false) == str

        # issue 242
        str_ = "f(a, b! = 1; c! = 2, d = 3, e! = 4)"
        str = "f(a, b! = 1; c! = 2, d=3, e! = 4)"
        @test fmt(str_, 4, 92, whitespace_in_kwargs = false) == str

        str_ = "( k1 =v1,  k2! = v2)"
        str = "(k1=v1, k2! = v2)"
        @test fmt(str_, 4, 80, style = YASStyle(), whitespace_in_kwargs = false) == str
        @test fmt(str_, 4, 80, style = DefaultStyle(), whitespace_in_kwargs = false) == str

        str_ = "( k1 =v1,  k2! = v2)"
        str = "(k1 = v1, k2! = v2)"
        @test fmt(str_, 4, 80, style = YASStyle(), whitespace_in_kwargs = true) == str
        @test fmt(str_, 4, 80, style = DefaultStyle(), whitespace_in_kwargs = true) == str
    end

    @testset "annotate untyped fields with `Any`" begin
        str = """
        struct name
            arg::Any
        end"""

        str_ = """
        struct name
            arg
        end"""
        @test fmt(str_) == str

        str_ = """
        struct name
        arg
        end"""
        @test fmt(str_) == str

        str_ = """
        struct name
                arg
            end"""
        @test fmt(str_) == str

        t = run_pretty(str_, 80)
        @test length(t) == 12

        str = """
        mutable struct name
            reallylongfieldname::Any
        end"""

        str_ = """
        mutable struct name
            reallylongfieldname
        end"""
        @test fmt(str_) == str

        str_ = """
        mutable struct name
        reallylongfieldname
        end"""
        @test fmt(str_) == str

        str_ = """
        mutable struct name
                reallylongfieldname
            end"""
        @test fmt(str_) == str

        t = run_pretty(str_, 80)
        @test length(t) == 28

        str = """
        struct name
            arg
        end"""

        str_ = """
        struct name
            arg
        end"""
        @test fmt(str_, annotate_untyped_fields_with_any = false) == str

        str_ = """
        struct name
        arg
        end"""
        @test fmt(str_, annotate_untyped_fields_with_any = false) == str

        str_ = """
        struct name
                arg
            end"""
        @test fmt(str_, annotate_untyped_fields_with_any = false) == str

        t = run_pretty(
            str_,
            opts = Options(max_margin = 80, annotate_untyped_fields_with_any = false),
        )
        @test length(t) == 11

        str = """
        mutable struct name
            reallylongfieldname
        end"""

        str_ = """
        mutable struct name
            reallylongfieldname
        end"""
        @test fmt(str_, annotate_untyped_fields_with_any = false) == str

        str_ = """
        mutable struct name
        reallylongfieldname
        end"""
        @test fmt(str_, annotate_untyped_fields_with_any = false) == str

        str_ = """
        mutable struct name
                reallylongfieldname
            end"""
        @test fmt(str_, annotate_untyped_fields_with_any = false) == str

        t = run_pretty(
            str_,
            opts = Options(max_margin = 80, annotate_untyped_fields_with_any = false),
        )
        @test length(t) == 23
    end

    @testset "format docstrings - basic" begin
        str = """
        \"""
        doc
        \"""
        function f()
            20
        end"""
        t = run_pretty(str, 80)
        @test length(t) == 12

        normalized = """
        \"""
        doc
        \"""
        function f()
            20
        end"""

        str = """
        \"""doc
        \"""
        function f()
            20
        end"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == normalized

        str = """
        \"""
        doc\"""
        function f()
            20
        end"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == normalized

        str = """
        \"""doc\"""
        function f()
            20
        end"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == normalized

        str = """
        "doc
        "
        function f()
            20
        end"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == normalized

        str = """
        "
        doc"
        function f()
            20
        end"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == normalized

        str = """
        "doc"
        function f()
            20
        end"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == normalized

        # test aligning to function identation
        str_ = """
            "doc"
        function f()
            20
        end"""
        str = """
        "doc"
        function f()
            20
        end"""
        @test fmt(str_) == str
        @test fmt(str_, format_docstrings = true) == normalized

        str = """\"""
                 doc for Foo
                 \"""
                 Foo"""
        @test fmt(str) == str
        t = run_pretty(str, 80)
        @test length(t) == 11

        str = """
        \"""
        doc
        \"""
        function f()    #  comment
            20
        end"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == str

        # Issue 157
        str = raw"""
        @doc \"""
           foo()
        \"""
        foo() = bar()"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == str

        str = raw"""
        @doc doc\"""
           foo()
        \"""
        foo() = bar()"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == str

        str = raw"""@doc "doc for foo" foo"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == str

        str = raw"""@doc \"""doc for foo\""" foo"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == str

        str = raw"""@doc doc\"""doc for foo\""" foo()"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == str

        str = raw"""@doc foo"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == str

        # issue 160
        str = """
        module MyModule

        import Markdown: @doc_str

        @doc doc\"""
            foo()
        \"""
        foo() = bar()

        end # module"""
        @test fmt(str) == str
        @test fmt(str, format_docstrings = true) == str
    end

    @testset "format docstrings - with code" begin
        unformatted = """
        \"""
        This is a docstring

        ```@example
        a =  1
         b  = 2
         a + b
        ```

        ```jldoctest
        a =  1
        b  = 2
        a + b

        # output

        3
        ```

        ```jldoctest
        julia> a =  1
        1

        julia> b  = 2;

        julia>  a + b
        3

        julia> function test(x)
               x + 1
               x + 2
               end;
        ```
        \"""
        function test(x) x end"""

        formatted = """
        \"""
        This is a docstring

        ```@example
        a = 1
        b = 2
        a + b
        ```

        ```jldoctest
        a = 1
        b = 2
        a + b

        # output

        3
        ```

        ```jldoctest
        julia> a = 1
        1

        julia> b = 2;


        julia> a + b
        3

        julia> function test(x)
                   x + 1
                   x + 2
               end;

        ```
        \"""
        function test(x)
            x
        end"""
        @test fmt(unformatted, format_docstrings = true) == formatted
    end

    @testset "format docstrings - Multi-line indented code-blocks" begin
        unformatted = """
        \"""
            fmt(
            )
        \"""
        function fmt() end"""

        formatted = """
        \"""
            fmt(
            )
        \"""
        function fmt() end"""
        @test fmt(unformatted, format_docstrings = true) == formatted
    end
    @testset "format docstrings - Empty line in docstring" begin
        unformatted = """
        \"""

        \"""
        function test() end"""

        formatted = """
        \"""
        \"""
        function test() end"""
        @test fmt(unformatted, format_docstrings = true) == formatted
    end

    @testset "format docstrings - Indented docstring" begin
        unformatted = """
        begin
            \"""
            Indented docstring

            with multiple paragraphs
            \"""
            indented_item
        end"""
        formatted = """
        begin
            \"""
            Indented docstring

            with multiple paragraphs
            \"""
            indented_item
        end"""
        @test fmt(unformatted, format_docstrings = true) == formatted

        short = """
        begin
            "Short docstring"
            item
        end
        """
        short_formatted = """
        begin
            \"""
            Short docstring
            \"""
            item
        end
        """
        @test fmt(short, format_docstrings = true) == short_formatted
    end
end
