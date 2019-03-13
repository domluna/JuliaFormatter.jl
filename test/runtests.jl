using JLFmt: format
using Test

@testset "All" begin
@testset "basic" begin
    @test format("a") == "a"
end
@testset "tuples" begin
    @test format("a,b") == "a, b"
    @test format("a ,b") == "a, b"
    @test format("a ,b,") == "a, b,"
    @test format("a ,b ,") == "a, b,"
    @test format("a , b ,") == "a, b,"
    @test format("(a,b)") == "(a, b)"
    @test format("(a ,b)") == "(a, b)"
    @test format("( a, b)") == "(a, b)"
    @test format("(a, b )") == "(a, b)"
    @test format("(a, b ,)") == "(a, b,)"
    #= @test format("""(a,    b , =#
    #=                     c)""") == "(a, b, c)" =#
end
@testset "curly" begin
    @test format("X{a,b}") == "X{a,b}"
    @test format("X{ a,b}") == "X{a,b}"
    @test format("X{a ,b}") == "X{a,b}"
    @test format("X{a, b}") == "X{a,b}"
    @test format("X{a,b }") == "X{a,b}"
    @test format("X{a,b }") == "X{a,b}"
end
# @testset "unary ops" begin
#     @test format("! x") == "!x"
# end
@testset "binary ops" begin
    @test format("a+b*c") == "a + b * c"
    @test format("a +b*c") == "a + b * c"
    @test format("a+ b*c") == "a + b * c"
    @test format("a+b *c") == "a + b * c"
    @test format("a+b* c") == "a + b * c"
    @test format("a+b*c ") == "a + b * c"
    @test format("a:b") == "a:b"
    @test format("a : b") == "a:b"
    @test format("a: b") == "a:b"
    @test format("a :b") == "a:b"
    @test format("a:b:c") == "a:b:c"
    @test format("a :b:c") == "a:b:c"
    @test format("a: b:c") == "a:b:c"
    @test format("a:b :c") == "a:b:c"
    @test format("a:b: c") == "a:b:c"
    @test format("a:b:c ") == "a:b:c"
    @test format("a::b:: c") == "a::b::c"
    @test format("a :: b::c") == "a::b::c"
end

@testset "op chain" begin
    @test format("a+b+c+d") == "a + b + c + d"
end

@testset "comparison chain" begin
    @test format("a<b==c≥d") == "a < b == c ≥ d"
end

@testset "colon op" begin
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
end

# @testset "func call" begin
#     @test format("func(a, b, c)") == "func(a, b, c)"
#     @test format("func(a,b,c)") == "func(a, b, c)"
#     @test format("func(a,b,c,)") == "func(a, b, c,)"
#     @test format("func(a,b,c, )") == "func(a, b, c,)"
#     @test format("func( a,b,c    )") == "func(a, b, c)"
#     @test format("func(a, b, c) ") == "func(a, b, c) "
#     @test format("func(a, b; c)") == "func(a, b; c)"
#     @test format("func(  a, b; c)") == "func(a, b; c)"
#     @test format("func(a  ,b; c)") == "func(a, b; c)"
#     @test format("func(a=1,b; c=1)") == "func(a = 1, b; c = 1)"
# end

@testset "indents" begin
@testset "begin" begin
str = """
begin
    arg
end"""
    @test format("""
                begin
                arg
                end""") == str
    @test format("""
                begin
                    arg
                end""") == str
    @test format("""
                begin
                    arg
                end""") == str
    @test format("""
                begin
                        arg
                end""") == str
    str = """
    begin
        begin
            arg
        end
    end"""
    @test format("""
                begin
                begin
                arg
                end
                end""") == str
    @test format("""
                begin
                            begin
                arg
                end
                end""") == str
    @test format("""
                begin
                            begin
                arg
                        end
                end""") == str
end
end

@testset "quote" begin
    str = """
    quote
        arg
    end"""
    @test format("""
    quote
        arg
    end""") == str
    @test format("""
    quote
    arg
    end""") == str
    @test format("""
    quote
            arg
        end""") == str
end

@testset "do" begin
    str = """
    map(args) do x
        y = 20
        return x * y
    end"""

    @test format("""
    map(args) do x
      y = 20
                        return x * y
        end""") == str

end

@testset "for" begin
    str = """
    for iter in I
        arg
    end"""
    @test format("""
    for iter in I
        arg
    end""") == str
    @test format("""
    for iter in I
    arg
    end""") == str
    @test format("""
    for iter in I
      arg
    end""") == str

    str = """
    for iter in I, iter2 in I2
        arg
    end"""
    @test format("""
    for iter in I, iter2 in I2
        arg
    end""") == str
    @test format("""
    for iter in I, iter2 in I2
    arg
    end""") == str
    @test format("""
    for iter in I, iter2 in I2
            arg
        end""") == str

    #= str = """ =#
    #= for iter in I, iter2 in I2 =#
    #=     arg =#
    #= end""" =#
    #= @test format(""" =#
    #= for iter=I, iter2 in I2 =#
    #=     arg =#
    #= end""", convert_iterator_ops=true) == str =#
    #= @test format(""" =#
    #= for iter =I, iter2 in I2 =#
    #=     arg =#
    #= end""", convert_iterator_ops=true) == str =#
    #= @test format(""" =#
    #= for iter =I, iter2 in I2 =#
    #=     arg =#
    #= end""", convert_iterator_ops=true) == str =#
    #= @test format(""" =#
    #= for iter = I, iter2 = I2 =#
    #=     arg =#
    #= end""", convert_iterator_ops=true) == str =#
end

@testset "while" begin
    str = """
    while cond
        arg
    end"""
    @test format("""
    while cond
        arg
    end""") == str
    @test format("""
    while cond
    arg
    end""") == str
    @test format("""
    while cond
            arg
        end""") == str
end

@testset "let" begin
    str = """
    let x = X
        arg
    end"""
    @test format("""
    let x=X
        arg
    end""") == str
    @test format("""
    let x=X
    arg
    end""") == str
    @test format("""
    let x=X
        arg
    end""") == str

    str = """
    let x = X, y = Y
        arg
    end"""
    @test format("""
    let x = X, y = Y
        arg
    end""") == str
    @test format("""
    let x = X, y = Y
    arg
    end""") == str

    str = """
    y, back = let
        body
    end"""
    @test format("""
    y,back = let
      body
    end""") == str
end

@testset "struct" begin
    str = """
    struct name
        arg
    end"""
    @test format("""
    struct name
        arg
    end""") == str
    @test format("""
    struct name
    arg
    end""") == str
    @test format("""
    struct name
            arg
        end""") == str
end

@testset "mutable struct" begin
    str = """
    mutable struct name
        arg
    end"""
    @test format("""
    mutable struct name
        arg
    end""") == str
    @test format("""
    mutable struct name
    arg
    end""") == str
    @test format("""
    mutable struct name
            arg
        end""") == str
end

@testset "try-catch" begin
    str = """
    try
        arg
    catch
        arg
    end"""
    @test format("""
    try
        arg
    catch
        arg
    end""") == str
    @test format("""
    try
    arg
    catch
    arg
    end""") == str
    @test format("""
    try
            arg
        catch
            arg
        end""") == str
    str = """
    try
        arg
    catch
        arg
    end"""
    @test format("""
    try
        arg
    catch
        arg
    end""") == str
    @test format("""
    try
    arg
    catch
    arg
    end""") == str
    @test format("""
    try
            arg
        catch
            arg
        end""") == str

    str = """
    try
        arg
    catch err
        arg
    end"""
    @test format("""
    try
        arg
    catch err
        arg
    end""") == str
    @test format("""
    try
    arg
    catch err
    arg
    end""") == str
    @test format("""
    try
            arg
        catch err
            arg
        end""") == str
end

@testset "docs" begin
    str = """
    \"""
    doc
    \"""
    function f()
        20
    end"""

    @test format("""
    \"""doc
    \"""
    function f()
        20
    end""") == str

    @test format("""
    \"""
    doc\"""
    function f()
        20
    end""") == str

    @test format("""
    \"""doc\"""
    function f()
        20
    end""") == str

    @test format("""
    "doc
    "
    function f()
        20
    end""") == str

    @test format("""
    "
    doc"
    function f()
        20
    end""") == str

    @test format("""
    "doc"
    function f()
        20
    end""") == str

    # test aligning to function identation
    #
    @test format("""
        "doc"
    function f()
        20
    end""") == str

    # tests indentation and correctly formatting a docstring with escapes
    #
    #= str = """ =#
    #=    begin =#
    #=        \""" =#
    #=            f =#
    #=  =#
    #=        docstring for f =#
    #=        :(function \$(dict[:name]){\$(all_params...)}(\$(dict[:args]...); =#
    #=                                             \$(dict[:kwargs]...))::\$rtype =#
    #=        \$(dict[:body]) =#
    #=        \""" =#
    #=        function f() =#
    #=            100 =#
    #=        end =#
    #=    end""" =#
    #= @test format(""" =#
    #=    begin =#
    #=    \""" =#
    #=        f =#
    #=  =#
    #=    docstring for f =#
    #=    :(function \$(dict[:name]){\$(all_params...)}(\$(dict[:args]...); =#
    #=                                         \$(dict[:kwargs]...))::\$rtype =#
    #=    \$(dict[:body]) =#
    #=    \""" =#
    #=    function f() =#
    #=    100 =#
    #=    end =#
    #=    end""") == str =#
end

#= @testset "comments" begin =#
#=     str = """ =#
#=     module Foo =#
#=     # comment 1 =#
#=     begin =#
#=         # comment 2 =#
#=         begin =#
#=             # comment 3 =#
#=             a = 10 =#
#=         end =#
#=     end =#
#=     end""" =#
#=     @test format(""" =#
#=     module Foo =#
#=     # comment 1 =#
#=     begin =#
#=     # comment 2 =#
#=     begin =#
#=     # comment 3 =#
#=     a = 10 =#
#=     end =#
#=     end =#
#=     end""") == str =#
#= end =#

@testset "reformat" begin

    str = """function foo end"""
    @test format("""
        function  foo
        end""") == str

    str = """function foo
                 10
                 20
             end"""
    @test format("""function foo 10;  20 end""") == str

    str = """abstract type AbstractFoo end"""
    @test format("""abstract
            type
                 AbstractFoo
            end""") == str

    str = """for cond
                 1
                 2
                 3
             end"""
    @test format("""for cond 1; 2; 3 end""") == str

    str = """while cond
                 1
                 2
                 3
             end"""
    @test format("""while cond 1; 2; 3 end""") == str

    str = """try
                 a
             catch e
                 b
             end"""
    @test format("""try a catch e b end""") == str

    str = """try
                 a1
                 a2
             catch e
                 b1
                 b2
             finally
                 c1
                 c2
             end"""
    @test format("""try a1;a2 catch e b1;b2 finally c1;c2 end""") == str

    str = """map(a) do b, c
                 e
             end"""
    @test format("""map(a) do b,c
                 e end""") == str

    str = """let a = b, c = d
                 e1
                 e2
                 e3
             end"""
    @test format("""let a=b,c=d e1; e2; e3 end""") == str

    str = """let a, b
                 e
             end"""
    @test format("""let a,b
                 e end""") == str

    str = """module A
             end"""
    @test format("""module A  end""") == str

    str = """return a, b, c"""
    @test format("""return a,b,
                 c""") == str

    str = """begin
                 a
                 b
                 c
             end"""
    @test format("""begin a; b; c end""") == str

    str = """quote
                 a
                 b
                 c
             end"""
    @test format("""quote a; b; c end""") == str

    str = """if cond1
                 e1
                 e2
             end"""
    @test format("if cond1 e1;e2 end") == str

    str = """if cond1
                 e1
                 e2
             else
                 e3
                 e4
             end"""
    @test format("if cond1 e1;e2 else e3;e4 end") == str

    str = """begin
                 if cond1
                     e1
                     e2
                 elseif cond2
                     e3
                     e4
                 elseif cond3
                     e5
                     e6
                 else
                     e7
                     e8
                 end
             end"""
             @test format("begin if cond1 e1; e2 elseif cond2 e3; e4 elseif cond3 e5;e6 else e7;e8  end end") == str

    str = """if cond1
                 e1
                 e2
             elseif cond2
                 e3
                 e4
             end"""
    @test format("if cond1 e1;e2 elseif cond2 e3; e4 end") == str

    str = """\"""
             doc for Foo
             \"""
             Foo"""
    @test format("\"\"\"doc for Foo\"\"\"\nFoo") == str

    #= str = """function f() where {A} =#
    #=          end""" =#
    #= @test format("function f() where A end") == str =#

    #= str = """ =#
    #=       # comment 0 =#
    #=  =#
    #=       a = 1 =#
    #=  =#
    #=       # comment 1 =#
    #=  =#
    #=       b = 2 =#
    #=  =#
    #=       c = 3 =#
    #=  =#
    #=       # comment 2 =#
    #=  =#
    #=       """ =#
    #= @test format("# comment 0\n\n\n\n\na=1\n\n# comment 1\n\n\n\n\nb = 2\n\n\nc=3\n\n# comment 2\n\n") == str =#

    str = """
    [a b c]"""
    @test format("[a   b         c   ]") == str

    str = """
    [a; b; c]"""
    @test format("[a;   b;         c;   ]") == str

    str = """
    T[a b c]"""
    @test format("T[a   b         c   ]") == str

    str = """
    T[a; b; c]"""
    @test format("T[a;   b;         c;   ]") == str

    str = """
    T[a; b; c; e d f]"""
    @test format("T[a;   b;         c;   e  d    f   ]") == str

    str = """
    T[e for e in x]"""
    @test format("T[e  for e in x  ]") == str

end

#= @testset "width aware" begin =#
#=     str = """ =#
#=     function f(arg1::A, =#
#=                key1 = val1; =#
#=                key2 = val2) where {A, =#
#=                                    F{B, =#
#=                                      C}} =#
#=         10 =#
#=         20 =#
#=     end""" =#
#=     @test format("function f(arg1::A,key1=val1;key2=val2) where {A,F{B,C}} 10; 20 end"; max_width=1) == str =#
#=  =#
#=     # (|, ||, &&, &) are foldable =#
#=     str = """ =#
#=     a | =#
#=     b | =#
#=     c | =#
#=     d""" =#
#=     @test format("a | b | c | d"; max_width=1) == str =#
#=  =#
#=     str = """ =#
#=     f(a, =#
#=       @g(b, c), =#
#=       d)""" =#
#=     @test format("f(a, @g(b, c), d)"; max_width=9) == str =#
#=  =#
#=     str = """ =#
#=     a, b, =#
#=     c, d""" =#
#=     @test format("a, b, c, d"; max_width=6) == str =#
#=  =#
#=     str = """ =#
#=     (a, b, =#
#=      c, d)""" =#
#=     @test format("(a, b, c, d)"; max_width=7) == str =#
#=  =#
#=     str = """ =#
#=     [a, =#
#=      b, =#
#=      c, =#
#=      d]""" =#
#=     @test format("[a, b, c, d]"; max_width=1) == str =#
#=  =#
#=     str = """ =#
#=     cond ? =#
#=     e1 : =#
#=     e2""" =#
#=     @test format("cond ? e1 : e2"; max_width=1) == str =#
#=  =#
#=     str = """ =#
#=     cond ? e1 : =#
#=     e2""" =#
#=     @test format("cond ? e1 : e2"; max_width=12) == str =#
#=  =#
#=     str = """ =#
#=     cond1 ? e1 : =#
#=     cond2 ? e2 : =#
#=     cond3 ? e3 : =#
#=     e4""" =#
#=     @test format("cond1 ? e1 : cond2 ? e2 : cond3 ? e3 : e4"; max_width=13) == str =#
#=  =#
#=     str = """ =#
#=     export a, =#
#=            b""" =#
#=     @test format("export a,b"; max_width=1) == str =#
#=  =#
#=     str = """ =#
#=     using a, =#
#=           b""" =#
#=     @test format("using a,b"; max_width=1) == str =#
#=  =#
#=     str = """ =#
#=     using M: a, =#
#=              b""" =#
#=     @test format("using M:a,b"; max_width=1) == str =#
#=  =#
#=     str = """ =#
#=     import M1.M2.M3: a, =#
#=                      b""" =#
#=     @test format("import M1.M2.M3:a,b"; max_width=1) == str =#
#=  =#
#=     str = """ =#
#=     foo() = =#
#=         (one, x -> (true, false))""" =#
#=     @test format("foo() = (one, x -> (true, false))"; max_width=30) == str =#
#=  =#
#=     str = """ =#
#=     foo() = =#
#=         (one, =#
#=          x -> (true, false))""" =#
#=     @test format("foo() = (one, x -> (true, false))"; max_width=24) == str =#
#=  =#
#=     str = """ =#
#=     foo() = =#
#=         (one, =#
#=          x -> (true, false))""" =#
#=     @test format("foo() = (one, x -> (true, false))"; max_width=24) == str =#
#=  =#
#=     str = """ =#
#=     foo() = =#
#=         (one, =#
#=          x -> (true, =#
#=                false))""" =#
#=     @test format("foo() = (one, x -> (true, false))"; max_width=20) == str =#
#=  =#
#=     str = """ =#
#=     @somemacro function (fcall_ | =#
#=                          fcall_) =#
#=                    body_ =#
#=                end""" =#
#=     @test format("@somemacro function (fcall_ | fcall_) body_ end"; max_width=1) == str =#
#= end =#

end
