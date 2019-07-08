import JLFmt: format
import JLFmt
using CSTParser
using Test

format(s) = format(s, 4, 80)

function run_nest(text::String, print_width::Int)
    d = JLFmt.Document(text)
    s = JLFmt.State(d, 4, 0, 1, 0, print_width)
    x = CSTParser.parse(text, true)
    t = JLFmt.pretty(x, s)
    JLFmt.nest!(t, s)
    s
end

@testset "All" begin

@testset "basic" begin
    @test format("a") == "a"
    @test format("") == ""
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
    @test format("""(a,    b ,
                        c)""") == "(a, b, c)"
end

@testset "curly" begin
    @test format("X{a,b}") == "X{a,b}"
    @test format("X{ a,b}") == "X{a,b}"
    @test format("X{a ,b}") == "X{a,b}"
    @test format("X{a, b}") == "X{a,b}"
    @test format("X{a,b }") == "X{a,b}"
    @test format("X{a,b }") == "X{a,b}"

    str = """
    mutable struct Foo{A<:Bar,Union{B<:Fizz,C<:Buzz},<:Any}
        a::A
    end"""
    @test format(str) == str

    str = """
    struct Foo{A<:Bar,Union{B<:Fizz,C<:Buzz},<:Any}
        a::A
    end"""
    @test format(str) == str
end

@testset "where op" begin
    str = "Atomic{T}(value) where {T<:AtomicTypes} = new(value)"
    @test format(str) == str

    str = "Atomic{T}(value) where T <: AtomicTypes = new(value)"
    @test format(str) == str
end

@testset "unary ops" begin
    @test format("! x") == "!x"
    @test format("x ...") == "x..."
end

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
    @test format("a +1 :b -1") == "a+1:b-1"
    @test format("a:b:c") == "a:b:c"
    @test format("a :b:c") == "a:b:c"
    @test format("a: b:c") == "a:b:c"
    @test format("a:b :c") == "a:b:c"
    @test format("a:b: c") == "a:b:c"
    @test format("a:b:c ") == "a:b:c"
    @test format("a::b:: c") == "a::b::c"
    @test format("a :: b::c") == "a::b::c"

    str = "!(typ <: ArithmeticTypes)"
    @test format(str) == str
end

@testset "op chain" begin
    @test format("a+b+c+d") == "a + b + c + d"
end

@testset "comparison chain" begin
    @test format("a<b==c≥d") == "a < b == c ≥ d"
end

@testset "single line block" begin
    @test format("(a;b;c)") == "(a; b; c)"
    @test format("(a;)") == "(a)"
end

@testset "func call" begin
    @test format("func(a, b, c)") == "func(a, b, c)"
    @test format("func(a,b,c)") == "func(a, b, c)"
    @test format("func(a,b,c,)") == "func(a, b, c,)"
    @test format("func(a,b,c, )") == "func(a, b, c,)"
    @test format("func( a,b,c    )") == "func(a, b, c)"
    @test format("func(a, b, c) ") == "func(a, b, c)"
    @test format("func(a, b; c)") == "func(a, b; c)"
    @test format("func(  a, b; c)") == "func(a, b; c)"
    @test format("func(a  ,b; c)") == "func(a, b; c)"
    @test format("func(a = 1,b; c = 1)") == "func(a=1, b; c=1)"
end

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

    str = """:(a = 10; b = 20; c = a * b)"""
    @test format(":(a = 10; b = 20; c = a * b)") == str

    str = """
    :(endidx = ndigits;
    while endidx > 1 && digits[endidx] == UInt8('0')
        endidx -= 1
    end;
    if endidx > 1
        print(out, '.')
        unsafe_write(out, pointer(digits) + 1, endidx - 1)
    end)"""

    str_ = """
:(endidx = ndigits;
            while endidx > 1 && digits[endidx] == UInt8('0')
                endidx -= 1
            end;
            if endidx > 1
                print(out, '.')
                unsafe_write(out, pointer(digits) + 1, endidx - 1)
            end)"""
    @test format(str_) == str
    @test format(str) == str
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

    # TODO: This should probably be aligned to match up with a ?
    str = """
    let x = a,
    # comment
    b,
    c
        body
    end"""
    @test format("""
    let x = a,
        # comment
           b,
          c
       body
       end""") == str
end

@testset "structs" begin
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

@testset "try" begin
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
end

@testset "strings" begin
    str = """
    \"""
    Interpolate using `\\\$`
    \"""
    a"""
    @test format(str) == str

    str = """error("foo\\n\\nbar")"""
    @test format(str) == str

    # nesting

    str = """
    \"""
    \\\\
    \"""
    x"""
    @test format("\"\\\\\" x") == str

    str = """
    begin
        s = \"\"\"This is a multiline string.
                This is another line.
                      Look another 1 that is indented a bit.

                      cool!\"\"\"
    end"""

    str_ = """
    begin
    s = \"\"\"This is a multiline string.
            This is another line.
                  Look another 1 that is indented a bit.

                  cool!\"\"\"
    end"""
    @test format(str_) == str


    str = """
    begin
        begin
            throw(ErrorException(\"""An error occured formatting \$filename. :-(

                                 Please file an issue at https://github.com/domluna/JLFmt.jl/issues
                                 with a link to a gist containing the contents of the file. A gist
                                 can be created at https://gist.github.com/.\"""))
        end
    end"""

    str_ = """
    begin
    begin
       throw(ErrorException(\"""An error occured formatting \$filename. :-(

                            Please file an issue at https://github.com/domluna/JLFmt.jl/issues
                            with a link to a gist containing the contents of the file. A gist
                            can be created at https://gist.github.com/.\"""))
       end
    end"""
    @test format(str_) == str

end

@testset "notcode" begin
    str = """
    module Foo
    # comment 0
    # comment 1
    begin

        # comment 2
        # comment 3

        begin



            # comment 4
            # comment 5
            a = 10
        end

    end

    end"""

    str_ = """
    module Foo
    # comment 0
    # comment 1
    begin

    # comment 2
    # comment 3

    begin



    # comment 4
    # comment 5
    a = 10
    end

    end

    end"""
    @test format(str_) == str
    @test format(str) == str

    str = "# comment 0\n\n\n\n\na = 1\n\n# comment 1\n\n\n\n\nb = 2\n\n\nc = 3\n\n# comment 2\n\n"
    @test format(str) == str
end

@testset "pretty" begin
    str = """function foo end"""
    @test format("""
        function  foo
        end""") == str

    str = """function foo() end"""
    @test format("""
                 function  foo()
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

    str = """return a, b, c"""
    @test format("""return a,b,
                 c""") == str

    str = """begin
                 a
                 b
                 c
             end"""
    @test format("""begin a; b; c end""") == str

    str = """begin end"""
    @test format("""begin \n            end""") == str

    str = """quote
                 a
                 b
                 c
             end"""
    @test format("""quote a; b; c end""") == str

    str = """quote end"""
    @test format("""quote \n end""") == str

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

    str = """T[e for e in x]"""
    @test format("T[e  for e in x  ]") == str

    str = """struct Foo end"""
    @test format("struct Foo\n      end") == str

    str = """
    struct Foo
        body
    end"""
    @test format("struct Foo\n    body  end") == str

    str = """macro foo() end"""
    @test format("macro foo()\n      end") == str

    str = """macro foo end"""
    @test format("macro foo\n      end") == str

    str = """
    macro foo()
        body
    end"""
    @test format("macro foo()\n    body  end") == str

    str = """mutable struct Foo end"""
    @test format("mutable struct Foo\n      end") == str

    str = """
    mutable struct Foo
        body
    end"""
    @test format("mutable struct Foo\n    body  end") == str

    str = """
    module Foo
    body
    end"""
    @test format("module Foo\n    body  end") == str

    str = """
    module Foo end"""
    @test format("module Foo\n    end") == str

    str = """
    if cond1
    elseif cond2
    elseif cond3
    elseif cond4
    elseif cond5
    elseif cond6
    elseif cond7
    else
    end"""
    @test format(str) == str

    str = """
    try
    catch
    finally
    end"""
    @test format(str) == str
end

@testset "nesting" begin
    str = """
    function f(
        arg1::A,
        key1=val1;
        key2=val2
    ) where {
        A,
        F{
          B,
          C
        }
    }
        10
        20
    end"""
    @test format("function f(arg1::A,key1=val1;key2=val2) where {A,F{B,C}} 10; 20 end", 4, 1) == str

    str = """
    function f(
        arg1::A,
        key1=val1;
        key2=val2
    ) where {
        A,
        F{B,C}
    }
        10
        20
    end"""
    @test format("function f(arg1::A,key1=val1;key2=val2) where {A,F{B,C}} 10; 20 end", 4, 17) == str

    str = """
    function f(
        arg1::A,
        key1=val1;
        key2=val2
    ) where {A,F{B,C}}
        10
        20
    end"""
    @test format("function f(arg1::A,key1=val1;key2=val2) where {A,F{B,C}} 10; 20 end", 4, 18) == str

    str = """
    a |
    b |
    c |
    d"""
    @test format("a | b | c | d", 4, 1) == str


    str = """
    a, b, c, d"""
    @test format("a, b, c, d", 4, 10) == str

    str = """a,\nb,\nc,\nd"""
    @test format("a, b, c, d", 4, 9) == str

    str = """(a, b, c, d)"""
    @test format("(a, b, c, d)", 4, 12) == str

    str = """
    (
     a,
     b,
     c,
     d
    )"""
    @test format("(a, b, c, d)", 4, 11) == str

    str = """{a,b,c,d}"""
    @test format("{a, b, c, d}", 4, 9) == str

    str = """
    {
     a,
     b,
     c,
     d
    }"""
    @test format("{a, b, c, d}", 4, 8) == str

    str = """[a, b, c, d]"""
    @test format("[a, b, c, d]", 4, 12) == str

    str = """
    [
     a,
     b,
     c,
     d
    ]"""
    @test format("[a, b, c, d]", 4, 11) == str

    str = """
    cond ?
    e1 :
    e2"""
    @test format("cond ? e1 : e2", 4, 1) == str

    str = """
    cond ? e1 :
    e2"""
    @test format("cond ? e1 : e2", 4, 12) == str

    str = """
    cond1 ? e1 :
    cond2 ? e2 :
    cond3 ? e3 :
    e4"""
    @test format("cond1 ? e1 : cond2 ? e2 : cond3 ? e3 : e4", 4, 13) == str

    str = """
    export a,
           b"""
    @test format("export a,b", 4, 1) == str

    str = """
    using a,
          b"""
    @test format("using a,b", 4, 1) == str

    str = """
    using M: a,
             b"""
    @test format("using M:a,b", 4, 1) == str

    str = """
    import M1.M2.M3: a,
                     b"""
    @test format("import M1.M2.M3:a,b", 4, 1) == str

    str = """
    foo() =
        (one, x -> (true, false))"""
    @test format("foo() = (one, x -> (true, false))", 4, 30) == str

    str = """
    foo() =
        (
         one,
         x -> (
             true,
             false
         )
        )"""
    @test format("foo() = (one, x -> (true, false))", 4, 20) == str

    str = """
    @somemacro function (fcall_ |
                         fcall_)
        body_
    end"""
    @test format("@somemacro function (fcall_ | fcall_) body_ end", 4, 1) == str
    
    str = "Val(x) = (@_pure_meta; Val{x}())"
    @test format("Val(x) = (@_pure_meta ; Val{x}())", 4, 80) == str

    str = "(a; b; c)"
    @test format("(a;b;c)", 4, 100) == str
    @test format("(a;b;c)", 4, 1) == str

    str = "(x for x in 1:10)"
    @test format("(x   for x  in  1 : 10)", 4, 100) == str
    @test format("(x   for x  in  1 : 10)", 4, 1) == str

    # indent for TupleH with no parens
    str = """
    function foo()
        arg1,
        arg2
    end"""
    @test format("function foo() arg1, arg2 end", 4, 1) == str

    str = """
    function foo()
        # comment
        arg
    end"""
    @test format(str, 4, 1) == str

    # don't nest < 2 args
    
    str = "A where {B}"
    @test format(str, 4, 1) == str

    str = "foo(arg1)"
    @test format(str, 4, 1) == str

    str = "[arg1]"
    @test format(str, 4, 1) == str

    str = "{arg1}"
    @test format(str, 4, 1) == str

    str = "(arg1)"
    @test format(str, 4, 1) == str

    str = """
    begin
        if foo
        elseif baz
        elseif (a ||
                b) &&
               c
        elseif bar
        else
        end
    end"""

    str_ = """
    begin
    if foo
    elseif baz
    elseif (a || b) && c
    elseif bar
    else
    end
    end"""
    @test format(str_, 4, 20) == str

    str = """
    begin
        if foo
        elseif baz
        elseif (a || b) &&
               c
        elseif bar
        else
        end
    end"""

    str_ = """
    begin
    if foo
    elseif baz
    elseif (a || b) && c
    elseif bar
    else
    end
    end"""
    @test format(str_, 4, 23) == str

    str = """
    begin
        if foo
        elseif baz
        elseif (a || b) && c
        elseif bar
        else
        end
    end"""

    str_ = """
    begin
    if foo
    elseif baz
    elseif (a || b) && c
    elseif bar
    else
    end
    end"""
    @test format(str_, 4, 24) == str

    # https://github.com/domluna/JLFmt.jl/issues/9#issuecomment-481607068
    str = """
    this_is_a_long_variable_name = Dict{Symbol,Any}(
        :numberofpointattributes => NAttributes,
        :numberofpointmtrs => NMTr,
        :numberofcorners => NSimplex,
        :firstnumber => Cint(1),
        :mesh_dim => Cint(3),
    )"""

    str_ = """this_is_a_long_variable_name = Dict{Symbol,Any}(:numberofpointattributes => NAttributes, 
           :numberofpointmtrs => NMTr, :numberofcorners => NSimplex, :firstnumber => Cint(1), 
           :mesh_dim => Cint(3),)"""
    @test format(str_, 4, 80) == str

    str = """this_is_a_long_variable_name = Dict{
         Symbol,
         Any
    }(
         :numberofpointattributes => NAttributes,
         :numberofpointmtrs => NMTr,
         :numberofcorners => NSimplex,
         :firstnumber => Cint(1),
         :mesh_dim => Cint(3),
    )"""
    @test format(str_, 5, 1) == str

    str = """
    this_is_a_long_variable_name = (
        :numberofpointattributes => NAttributes,
        :numberofpointmtrs => NMTr,
        :numberofcorners => NSimplex,
        :firstnumber => Cint(1),
        :mesh_dim => Cint(3),
    )"""

    str_ = """this_is_a_long_variable_name = (:numberofpointattributes => NAttributes, 
           :numberofpointmtrs => NMTr, :numberofcorners => NSimplex, :firstnumber => Cint(1), 
           :mesh_dim => Cint(3),)"""
    @test format(str_, 4, 80) == str

    str = """
    begin
        a && b
        a || b
    end"""
    @test format(str, 4, 1) == str

    # str = """
    # func(a, \"""this
    # is another
    # multi-line
    # string.
    # Longest line
    # \""", foo(b, c))"""
    # @test format(str, 4, 33) == str
    
    str = """
    func(
        a,
        \"""this
        is another
        multi-line
        string.
        Longest line
        \""",
        foo(b, c)
    )"""
    @test format(str, 4, 31) == str


end

@testset "nesting line offset" begin
    str = "a - b + c * d"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 5
    s = run_nest(str, 1)
    @test s.line_offset == 1

    str ="c ? e1 : e2"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 2
    s = run_nest(str, 8)
    @test s.line_offset == 2
    s = run_nest(str, 1)
    @test s.line_offset == 2

    str = "c1 ? e1 : c2 ? e2 : c3 ? e3 : c4 ? e4 : e5"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 32
    s = run_nest(str, 30)
    @test s.line_offset == 22
    s = run_nest(str, 20)
    @test s.line_offset == 12
    s = run_nest(str, 10)
    @test s.line_offset == 2
    s = run_nest(str, 1)
    @test s.line_offset == 2

    str = "f(a, b, c) where {A,B,C}"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 15
    s = run_nest(str, 14)
    @test s.line_offset == 1
    s = run_nest(str, 1)
    @test s.line_offset == 1

    str = "f(a, b, c) where Union{A,B,C}"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 20
    s = run_nest(str, 19)
    @test s.line_offset == 1
    s = run_nest(str, 1)
    @test s.line_offset == 1

    str = "f(a, b, c) where A"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, 1)
    @test s.line_offset == 9

    str = "f(a, b, c) where A <: S"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, 1)
    @test s.line_offset == 14

    str = "f(a, b, c) where Union{A,B,Union{C,D,E}}"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 31
    s = run_nest(str, 30)
    @test s.line_offset == 1
    s = run_nest(str, 1)
    @test s.line_offset == 1

    str = "f(a, b, c) where {A,{B,C,D},E}"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, 1)
    @test s.line_offset == 1

    str = "(a, b, c, d)"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 1

    str = "a, b, c, d"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 0

    str = """
    splitvar(arg) =
        @match arg begin
            ::T_ => (nothing, T)
            name_::T_ => (name, T)
            x_ => (x, :Any)
        end"""
    s = run_nest(str, 96)
    @test s.line_offset == 7
    s = run_nest(str, 1)
    @test s.line_offset == 7

    str = "prettify(ex; lines = false) = ex |> (lines ? identity : striplines) |> flatten |> unresolve |> resyntax |> alias_gensyms"
    s = run_nest(str, 80)
    @test s.line_offset == 17

    str = "foo() = a + b"
    s = run_nest(str, length(str))
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 9
    s = run_nest(str, 1)
    @test s.line_offset == 5


    str = "export @esc, isexpr, isline, iscall, rmlines, unblock, block, inexpr, namify, isdef"
    s = run_nest(str, length(str))
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 12

    # https://github.com/domluna/JLFmt.jl/issues/9#issuecomment-481607068
    str = """this_is_a_long_variable_name = Dict{Symbol,Any}(:numberofpointattributes => NAttributes, 
           :numberofpointmtrs => NMTr, :numberofcorners => NSimplex, :firstnumber => Cint(1), 
           :mesh_dim => Cint(3),)"""
    s = run_nest(str, 80)
    @test s.line_offset == 1

    str = """this_is_a_long_variable_name = (:numberofpointattributes => NAttributes, 
           :numberofpointmtrs => NMTr, :numberofcorners => NSimplex, :firstnumber => Cint(1), 
           :mesh_dim => Cint(3),)"""
    s = run_nest(str, 80)
    @test s.line_offset == 1
end

@testset "additional length" begin
    str = """
    f(
      a,
      @g(b, c),
      d
    )"""
    @test format("f(a, @g(b, c), d)", 4, 11) == str

    str = """
    f(
      a,
      @g(
         b,
         c
      ),
      d
    )"""
    @test format("f(a, @g(b, c), d)", 4, 10) == str

    str = """
    (
     a,
     (
      b,
      c
     ),
     d
    )"""
    @test format("(a, (b, c), d)", 4, 7) == str

    str = """
    (
     a,
     {
      b,
      c
     },
     d
    )"""
    @test format("(a, {b, c}, d)", 4, 6) == str

    str = """
    a,
    (
     b,
     c
    ),
    d"""
    @test format("a, (b, c), d", 4, 6) == str

    str = """
    a,
    (b, c),
    d"""
    @test format("a, (b, c), d", 4, 7) == str

    str = """
    (
     var1,
     var2
    ) && var3"""
    @test format("(var1,var2) && var3", 4, 10) == str

    str = """
    (var1, var2) && var3"""
    @test format("(var1,var2) && var3", 4, 15) == str

    str = """
    (var1, var2) ?
    (var3, var4) :
    var5"""
    @test format("(var1,var2) ? (var3,var4) : var5", 4, 14) == str

    str = """
    (
     var1,
     var2
    ) ?
    (
     var3,
     var4
    ) :
    var5"""
    @test format("(var1,var2) ? (var3,var4) : var5", 4, 13) == str

    str = """
    (var1, var2) ? (var3, var4) :
    var5"""
    @test format("(var1,var2) ? (var3,var4) : var5", 4, 30) == str

    str = """
    (var1, var2) ?
    (var3, var4) :
    var5"""
    @test format("(var1,var2) ? (var3,var4) : var5", 4, 29) == str

    str = """
    f(
      var1::A,
      var2::B
    ) where {A,B}"""
    @test format("f(var1::A, var2::B) where {A,B}", 4, 30) == str

    str = """
    f(
      var1::A,
      var2::B
    ) where {
        A,
        B
    }"""
    @test format("f(var1::A, var2::B) where {A,B}", 4, 12) == str

    str = "foo(a, b, c)::Rtype where {A,B} = 10"
    @test format(str, 4, length(str)) == str

    str_ = """
    foo(a, b, c)::Rtype where {A,B} =
        10"""
    @test format(str, 4, 35) == str_
    @test format(str, 4, 33) == str_

    str_ = """
    foo(a, b, c)::Rtype where {
        A,
        B
    } =
        10"""
    @test format(str, 4, 32) == str_
    @test format(str, 4, 19) == str_

    str_ = """
    foo(
        a,
        b,
        c
    )::Rtype where {
        A,
        B
    } =
        10"""
    @test format(str, 4, 18) == str_


end

#
# TODO: not sure how this should be formatted, revisit at some point
# push!(s::BitSet, ns::Integer...) = (for n in ns; push!(s, n); end; s)
#
# add another check in binary function defs to see if lifting
# the nested line back up again is possible
# 
# TODO: StringH should nest
# TODO: 

end
