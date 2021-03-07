@testset "Formatting Options" begin
    @testset "remove extra newlines" begin
        str_ = """
        a = 10

        # foo1
        # ooo



        # aooo


        # aaaa
        b = 20



        # hello
        """
        str = """
        a = 10

        # foo1
        # ooo

        # aooo

        # aaaa
        b = 20

        # hello
        """
        @test fmt(str_, remove_extra_newlines = true) == str
        @test fmt(str_, remove_extra_newlines = false) == str_

        str_ = """
        module M


        function foo(bar::Int64, baz::Int64)


            return bar + baz
        end

        function foo(bar::In64, baz::Int64)
            return bar + baz


        end


        end
        """
        str = """
        module M

        function foo(bar::Int64, baz::Int64)
            return bar + baz
        end

        function foo(bar::In64, baz::Int64)
            return bar + baz
        end

        end
        """
        @test fmt(str_, remove_extra_newlines = true) == str
        @test fmt(str_, remove_extra_newlines = false) == str_
    end

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
        t = run_pretty(str_, opts = Options(margin = 80, import_to_using = true))
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

        str_ = "[i for i = 1:10 if i == 2]"
        str = "[i for i in 1:10 if i == 2]"
        @test fmt(str_, always_for_in = true) == str
        @test fmt(str, always_for_in = true) == str

        str_ = "[(i, j) for i = 1:2:10, j = 100:-1:10]"
        str = "[(i, j) for i in 1:2:10, j in 100:-1:10]"
        @test yasfmt(str_, always_for_in = true) == str
        @test yasfmt(str, always_for_in = true) == str

        str_ = "[i for i = 1:10 if i == 2]"
        str = "[i for i in 1:10 if i == 2]"
        @test yasfmt(str_, always_for_in = true) == str
        @test yasfmt(str, always_for_in = true) == str
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

        str = """
        function foo()
            @macrocall(expr2)
        end"""
        @test fmt(str, 4, 92, always_use_return = true) == str

        str = """
        function foo()
            @macroblock expr2
        end"""
        @test fmt(str, 4, 92, always_use_return = true) == str

        str = """
        function foo()
            for i = 1:10
                println(i)
            end
        end"""
        @test fmt(str, 4, 92, always_use_return = true) == str

        str = """
        function f(a)
            if a > 0
                return -1
            else
                return 1
            end
        end"""
        @test fmt(str, 4, 92, always_use_return = true) == str
    end

    @testset "whitespace in keyword arguments" begin
        str_ = "f(; a = b)"
        str = "f(; a=b)"
        @test fmt(str_, 4, 92, whitespace_in_kwargs = false) == str

        str = "f(; a!) = a!"
        @test fmt(str, 4, 92, whitespace_in_kwargs = false) == str

        # issue 242
        str_ = "f(a, b! = 1; c! = 2, d = 3, e! = 4)"
        str = "f(a, (b!)=1; (c!)=2, d=3, (e!)=4)"
        @test fmt(str_, 4, 92, whitespace_in_kwargs = false) == str

        str_ = "( k1 =v1,  k2! = v2)"
        str = "(k1=v1, (k2!)=v2)"
        @test fmt(str_, 4, 80, style = YASStyle(), whitespace_in_kwargs = false) == str
        @test fmt(str_, 4, 80, style = DefaultStyle(), whitespace_in_kwargs = false) == str

        str_ = "( k1 =v1,  k2! = v2)"
        str = "(k1 = v1, k2! = v2)"
        @test fmt(str_, 4, 80, style = YASStyle(), whitespace_in_kwargs = true) == str
        @test fmt(str_, 4, 80, style = DefaultStyle(), whitespace_in_kwargs = true) == str

        str_ = "(; g = >=(1))"
        str = "(; g=(>=(1)))"
        @test fmt(str_, 4, 92, whitespace_in_kwargs = false) == str
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
            opts = Options(margin = 80, annotate_untyped_fields_with_any = false),
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
            opts = Options(margin = 80, annotate_untyped_fields_with_any = false),
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

    @testset "align struct fields" begin
        str_ = """
        struct Foo
            a::T
        end"""
        str = """
        struct Foo
            a::T
        end"""
        @test fmt(str_, align_struct_field = true) == str

        str = """
        struct Foo
            a             :: T
            longfieldname :: B
        end"""
        str_ = """
        struct Foo
            a::T
            longfieldname::B
        end"""
        @test fmt(str, align_struct_field = true) == str
        @test fmt(str, align_struct_field = false) == str_

        str_ = """
        Base.@kwdef struct Options
            indent_size::Int                       = 4
            margin::Int                            = 92
            always_for_in::Bool                 = false
            whitespace_typedefs::Bool          = false
            whitespace_ops_in_indices::Bool        = false
            remove_extra_newlines::Bool            = false
            import_to_using::Bool                  = false
            pipe_to_function_call::Bool            = false
            short_to_long_function_def::Bool      = false
            always_use_return::Bool           = false
            whitespace_in_kwargs::Bool          = true
            annotate_untyped_fields_with_any::Bool = true
            format_docstrings::Bool             = false
            align_struct_fields::Bool           = false

            another_field1::BlahBlahBlah = 10
            field2::Foo                          = 10

            Options() = new()
        end"""
        str = """
        Base.@kwdef struct Options
            indent_size::Int                       = 4
            margin::Int                            = 92
            always_for_in::Bool                    = false
            whitespace_typedefs::Bool              = false
            whitespace_ops_in_indices::Bool        = false
            remove_extra_newlines::Bool            = false
            import_to_using::Bool                  = false
            pipe_to_function_call::Bool            = false
            short_to_long_function_def::Bool       = false
            always_use_return::Bool                = false
            whitespace_in_kwargs::Bool             = true
            annotate_untyped_fields_with_any::Bool = true
            format_docstrings::Bool                = false
            align_struct_fields::Bool              = false

            another_field1::BlahBlahBlah = 10
            field2::Foo = 10

            Options() = new()
        end"""
        @test fmt(str_, align_struct_field = true) == str

        str_ = """
        Base.@kwdef struct Options
            indent_size::Int = 4
            margin::Int = 92
            always_for_in::Bool = false
            whitespace_typedefs::Bool = false
            whitespace_ops_in_indices::Bool = false
            remove_extra_newlines::Bool = false
            import_to_using::Bool = false
            pipe_to_function_call::Bool = false
            short_to_long_function_def::Bool = false
            always_use_return::Bool = false
            whitespace_in_kwargs::Bool = true
            annotate_untyped_fields_with_any::Bool = true
            format_docstrings::Bool = false
            align_struct_fields::Bool = false

            another_field1::BlahBlahBlah = 10
            field2::Foo = 10

            Options() = new()
        end"""
        @test fmt(str, align_struct_field = true) == str
        @test fmt(str, align_struct_field = false) == str_

        str = """
        Base.@kwdef struct Options
            indent_size::Int                       = 4
            margin::Int                            = 92
            always_for_in::Bool                    = false
            whitespace_typedefs::Bool              = false
            whitespace_ops_in_indices::Bool        = false
            remove_extra_newlines::Bool            = false
            import_to_using::Bool                  = false
            pipe_to_function_call::Bool            = false
            short_to_long_function_def::Bool       = false
            always_use_return::Bool                = false
            whitespace_in_kwargs::Bool             = true
            annotate_untyped_fields_with_any::Bool = true
            format_docstrings::Bool                = false
            align_struct_fields::Bool              = false

            another_field1::BlahBlahBlah =
                10
            field2::Foo =
                10

            Options() =
                new()
        end"""
        @test fmt(str, 4, 1, align_struct_field = true) == str
    end

    @testset "align assignment" begin
        str_ = """
        const variable1 = 1
        const var2      = 2
        const var3 = 3
        const var4 = 4"""
        str = """
        const variable1 = 1
        const var2      = 2
        const var3      = 3
        const var4      = 4"""
        @test fmt(str_, align_assignment = true) == str

        str = """
        const variable1 = 1
        const variable2 = 2
        const var3 = 3
        const var4 = 4"""
        @test fmt(str, align_assignment = true) == str

        str_ = """
        module Foo

        const UTF8PROC_STABLE    = (1<<1)
        const UTF8PROC_COMPAT    = (1<<2)
        const UTF8PROC_COMPOSE   = (1<<3)
        const UTF8PROC_DECOMPOSE = (1<<4)
        const UTF8PROC_IGNORE    = (1<<5)
        const UTF8PROC_REJECTNA  = (1<<6)
        const UTF8PROC_NLF2LS    = (1<<7)
        const UTF8PROC_NLF2PS    = (1<<8)
        const UTF8PROC_NLF2LF    = (UTF8PROC_NLF2LS | UTF8PROC_NLF2PS)
        const UTF8PROC_STRIPCC   = (1<<9)
        const UTF8PROC_CASEFOLD  = (1<<10)
        const UTF8PROC_CHARBOUND = (1<<11)
        const UTF8PROC_LUMP=(1<<12)
        const UTF8PROC_STRIP         = (1<<13) # align this

        const FOOBAR = 0
        const FOO = 1

        end"""

        str = """
        module Foo

        const UTF8PROC_STABLE    = (1 << 1)
        const UTF8PROC_COMPAT    = (1 << 2)
        const UTF8PROC_COMPOSE   = (1 << 3)
        const UTF8PROC_DECOMPOSE = (1 << 4)
        const UTF8PROC_IGNORE    = (1 << 5)
        const UTF8PROC_REJECTNA  = (1 << 6)
        const UTF8PROC_NLF2LS    = (1 << 7)
        const UTF8PROC_NLF2PS    = (1 << 8)
        const UTF8PROC_NLF2LF    = (UTF8PROC_NLF2LS | UTF8PROC_NLF2PS)
        const UTF8PROC_STRIPCC   = (1 << 9)
        const UTF8PROC_CASEFOLD  = (1 << 10)
        const UTF8PROC_CHARBOUND = (1 << 11)
        const UTF8PROC_LUMP      = (1 << 12)
        const UTF8PROC_STRIP     = (1 << 13) # align this

        const FOOBAR = 0
        const FOO = 1

        end"""
        # @test fmt(str_, align_assignment = false) == str
        @test fmt(str_, align_assignment = true) == str

        # the aligned consts will NOT be nestable
        str = """
        module Foo

        const UTF8PROC_STABLE    = (1 << 1)
        const UTF8PROC_COMPAT    = (1 << 2)
        const UTF8PROC_COMPOSE   = (1 << 3)
        const UTF8PROC_DECOMPOSE = (1 << 4)
        const UTF8PROC_IGNORE    = (1 << 5)
        const UTF8PROC_REJECTNA  = (1 << 6)
        const UTF8PROC_NLF2LS    = (1 << 7)
        const UTF8PROC_NLF2PS    = (1 << 8)
        const UTF8PROC_NLF2LF    = (UTF8PROC_NLF2LS | UTF8PROC_NLF2PS)
        const UTF8PROC_STRIPCC   = (1 << 9)
        const UTF8PROC_CASEFOLD  = (1 << 10)
        const UTF8PROC_CHARBOUND = (1 << 11)
        const UTF8PROC_LUMP      = (1 << 12)
        const UTF8PROC_STRIP     = (1 << 13) # align this

        const FOOBAR =
            0
        const FOO =
            1

        end"""
        @test fmt(str_, 4, 1, align_assignment = true) == str

        str = """
        a  = 1
        bc = 2

        long_variable = 1
        other_var     = 2
        """
        @test fmt(str, 4, 1, align_assignment = true) == str

        str = """
        vcat(X::T...) where {T}         = T[X[i] for i = 1:length(X)]
        vcat(X::T...) where {T<:Number} = T[X[i] for i = 1:length(X)]
        hcat(X::T...) where {T}         = T[X[j] for i = 1:1, j = 1:length(X)]
        hcat(X::T...) where {T<:Number} = T[X[j] for i = 1:1, j = 1:length(X)]
        """
        @test fmt(str, 4, 1, align_assignment = true) == str

        str = """
        μs, ns = divrem(ns, 1000)
        ms, μs = divrem(μs, 1000)
        s, ms = divrem(ms, 1000)
        """
        @test fmt(str, 4, 100, align_assignment = true) == str

        str = """
        run = wandb.init(
            name      = name,
            project   = project,
            config    = config,
            notes     = notes,
            tags      = tags,
            dir       = dir,
            job_type  = job_type,
            entity    = entity,
            group     = group,
            id        = id,
            reinit    = reinit,
            resume    = resume,
            anonymous = anonymous ? "allow" : "never",
        )
        """
        @test fmt(str, 4, 100, align_assignment = true, whitespace_in_kwargs = false) == str

        str_ = """
        s           = model.sys
        @unpack A,K = s
        C           = s.C
        poles       = eigvals(A - K * C)
        """
        str = """
        s            = model.sys
        @unpack A, K = s
        C            = s.C
        poles        = eigvals(A - K * C)
        """
        @test fmt(str_, 4, 100, align_assignment = true) == str

        str_ = """
        s             = model.sys
        @unpack A,K   = s
        C             = s.C
        const polesss = eigvals(A - K * C)
        """
        str = """
        s             = model.sys
        @unpack A, K  = s
        C             = s.C
        const polesss = eigvals(A - K * C)
        """
        @test fmt(str_, 4, 100, align_assignment = true) == str

        str = """
        function stabilize(model)
            s            = model.sys
            @unpack A, K = s
            C            = s.C
            poles        = eigvals(A - K * C)
            newpoles     = map(poles) do p
                ap = abs(p)
                ap <= 1 && (return p)
                p / (ap + sqrt(eps()))
            end
            K2           = ControlSystems.acker(A', C', newpoles)' .|> real
            all(abs(p) <= 1 for p in eigvals(A - K * C)) || @warn("Failed to stabilize predictor")
            s.K .= K2
            model
        end
        """
        @test fmt(str, 4, 100, align_assignment = true) == str
    end

    @testset "align conditionals" begin
        str_ = """
        index = zeros(n <= typemax(Int8)  ? Int8  :
                      n <= typemax(Int16) ? Int16 :
                      n <= typemax(Int32) ? Int32 : Int64, n)
        """

        str = """
        index = zeros(
            n <= typemax(Int8)  ? Int8  :
            n <= typemax(Int16) ? Int16 :
            n <= typemax(Int32) ? Int32 : Int64,
            n,
        )
        """
        @test fmt(str_, align_conditional = true) == str

        str = """
        index =
            zeros(
                n <= typemax(Int8)  ? Int8  :
                n <= typemax(Int16) ? Int16 :
                n <= typemax(Int32) ? Int32 : Int64,
                n,
            )
        """
        @test fmt(str_, 4, 1, align_conditional = true) == str

        str_ = """
        index = zeros(n <= typemax(Int8)  ? Int8 :   # inline
                        #comment 1
                      n <= typemax(Int16) ? Int16 :   # inline 2
                              # comment 2
                      n <= typemax(Int32) ? Int32 : # inline 3
                      Int64, n)
        """
        str = """
        index =
            zeros(
                n <= typemax(Int8)  ? Int8 :   # inline
                #comment 1
                n <= typemax(Int16) ? Int16 :   # inline 2
                # comment 2
                n <= typemax(Int32) ? Int32 : # inline 3
                Int64,
                n,
            )
        """
        @test fmt(str_, 4, 1, align_conditional = true) == str

        str_ = """
        index = zeros(n <= typemax(Int8)  ? Int8  :    # inline
                      n <= typemax(Int16) ? Int16 : n <= typemax(Int32) ? Int32 : Int64, n)
        """

        str = """
        index =
            zeros(
                n <= typemax(Int8)  ? Int8  :    # inline
                n <= typemax(Int16) ? Int16 : n <= typemax(Int32) ? Int32 : Int64,
                n,
            )
        """
        @test fmt(str_, 4, 1, align_conditional = true) == str

        str_ = """
        index =
            zeros(
                n <= typemax(Int8)     ? Int8  :
                n <= typemax(Int16A) ? Int16  :
                n <= typemax(Int32)  ? Int322 : Int64,
                n,
            )
        """
        str = """
        index =
            zeros(
                n <= typemax(Int8)   ? Int8   :
                n <= typemax(Int16A) ? Int16  :
                n <= typemax(Int32)  ? Int322 : Int64,
                n,
            )
        """
        @test fmt(str_, 4, 1, align_conditional = true) == str

        str_ = """
        val = cst.kind === Tokens.ABSTRACT ? "abstract" :
            cst.kind === Tokens.BAREMODULE ? "baremodule" : ""
        """
        str = """
        val = cst.kind === Tokens.ABSTRACT ? "abstract" : cst.kind === Tokens.BAREMODULE ? "baremodule" : ""
        """
        @test fmt(str_, 4, 100, align_conditional = true) == str

        str_ = """
        val = cst.kind === Tokens.ABSTRACT ? "abstract" :
            cst.kind === Tokens.BAREMODUL  ? "baremodule" : ""
        """
        str = """
        val = cst.kind === Tokens.ABSTRACT  ? "abstract" :
              cst.kind === Tokens.BAREMODUL ? "baremodule" : ""
        """
        @test fmt(str_, 4, 100, align_conditional = true) == str

        str = """
        val =
            cst.kind === Tokens.ABSTRACT  ? "abstract" :
            cst.kind === Tokens.BAREMODUL ? "baremodule" : ""
        """
        @test fmt(str_, 4, 1, align_conditional = true) == str
    end

    @testset "align pair arrow `=>`" begin
        str_ = """
        pages = [
            "Introduction" => "index.md",
            "How It Works" => "how_it_works.md",
            "Code Style"          => "style.md",
            "Skipping Formatting" => "skipping_formatting.md",
            "Syntax Transforms" => "transforms.md",
            "Custom Alignment" => "custom_alignment.md",
            "Custom Styles" => "custom_styles.md",
            "YAS Style" => "yas_style.md",
            "Configuration File" => "config.md",
            "API Reference" => "api.md",
        ]
        """
        str = """
        pages = [
            "Introduction"        => "index.md",
            "How It Works"        => "how_it_works.md",
            "Code Style"          => "style.md",
            "Skipping Formatting" => "skipping_formatting.md",
            "Syntax Transforms"   => "transforms.md",
            "Custom Alignment"    => "custom_alignment.md",
            "Custom Styles"       => "custom_styles.md",
            "YAS Style"           => "yas_style.md",
            "Configuration File"  => "config.md",
            "API Reference"       => "api.md",
        ]
        """
        @test fmt(str_, 4, 100, align_pair_arrow = true) == str

        str = """
        pages =
            [
                "Introduction"        => "index.md",
                "How It Works"        => "how_it_works.md",
                "Code Style"          => "style.md",
                "Skipping Formatting" => "skipping_formatting.md",
                "Syntax Transforms"   => "transforms.md",
                "Custom Alignment"    => "custom_alignment.md",
                "Custom Styles"       => "custom_styles.md",
                "YAS Style"           => "yas_style.md",
                "Configuration File"  => "config.md",
                "API Reference"       => "api.md",
            ]
        """
        @test fmt(str_, 4, 1, align_pair_arrow = true) == str
    end

    @testset "conditional to `if` block" begin
        str_ = """
        E ? A : B
        """
        @test fmt(str_, 2, 9, conditional_to_if = true) == str_

        str = """
        if E
          A
        else
          B
        end
        """
        @test fmt(str_, 2, 8, conditional_to_if = true) == str

        str_ = """
        begin
            E1 ? A : E2 ? B : foo(E333, E444) ? D : E
        end
        """
        @test fmt(str_, 4, 45, conditional_to_if = true) == str_

        str = """
        begin
            if E1
                A
            elseif E2
                B
            elseif foo(E333, E444)
                D
            else
                E
            end
        end
        """
        @test fmt(str_, 4, 44, conditional_to_if = true) == str
        @test fmt(str_, 4, 26, conditional_to_if = true) == str

        str = """
        begin
            if E1
                A
            elseif E2
                B
            elseif foo(
                E333,
                E444,
            )
                D
            else
                E
            end
        end
        """
        @test fmt(str_, 4, 25, conditional_to_if = true) == str

        str_ = """
        foobar = some_big_long_thing * 10_000 == 2 ?
            #comment
            bar :
            #comment
            another_big_long_thing * 10^300 / this_things_here
        """

        str = """
        foobar = if some_big_long_thing * 10_000 == 2
            #comment
            bar
        else
            #comment
            another_big_long_thing * 10^300 / this_things_here
        end
        """
        @test fmt(str_, conditional_to_if = true) == str
    end

    @testset "normalize_line_endings" begin
        windows_str = "a\r\nb\r\nc\r\nd"
        unix_str = "a\nb\nc\nd"
        mixed_windows_str = "a\r\nb\r\nc\nd"
        mixed_unix_str = "a\r\nb\nc\nd"

        @test fmt(windows_str, normalize_line_endings = "auto") == windows_str
        @test fmt(unix_str, normalize_line_endings = "auto") == unix_str
        @test fmt(mixed_windows_str, normalize_line_endings = "auto") == windows_str
        @test fmt(mixed_unix_str, normalize_line_endings = "auto") == unix_str

        @test fmt(windows_str, normalize_line_endings = "unix") == unix_str
        @test fmt(unix_str, normalize_line_endings = "unix") == unix_str
        @test fmt(mixed_windows_str, normalize_line_endings = "unix") == unix_str
        @test fmt(mixed_unix_str, normalize_line_endings = "unix") == unix_str

        @test fmt(windows_str, normalize_line_endings = "windows") == windows_str
        @test fmt(unix_str, normalize_line_endings = "windows") == windows_str
        @test fmt(mixed_windows_str, normalize_line_endings = "windows") == windows_str
        @test fmt(mixed_unix_str, normalize_line_endings = "windows") == windows_str
    end

    @testset "for_in_replacement" begin
        str_ = """
        for a = b
        end
        """
        str = """
        for a ∈ b
        end
        """
        @test fmt(str_, always_for_in = true, for_in_replacement = "∈") == str

        # generator
        str_ = "[(i, j) for i = 1:2:10, j = 100:-1:10]"
        str = "[(i, j) for i ∈ 1:2:10, j ∈ 100:-1:10]"
        @test fmt(str_, always_for_in = true, for_in_replacement = "∈") == str

        str_ = "[i for i = 1:10 if i == 2]"
        str = "[i for i ∈ 1:10 if i == 2]"
        @test fmt(str_, always_for_in = true, for_in_replacement = "∈") == str

        str_ = "[(i, j) for i = 1:2:10, j = 100:-1:10]"
        str = "[(i, j) for i ∈ 1:2:10, j ∈ 100:-1:10]"
        @test yasfmt(str_, always_for_in = true, for_in_replacement = "∈") == str

        str_ = "[i for i = 1:10 if i == 2]"
        str = "[i for i ∈ 1:10 if i == 2]"
        @test yasfmt(str_, always_for_in = true, for_in_replacement = "∈") == str
    end
end
