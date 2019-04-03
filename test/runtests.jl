using JLFmt: format, Document, State, pretty, nest!
using CSTParser
using Test

function run_nest(text::String, max_line_length::Int)
    d = Document(text)
    s = State(d, 4, 0, 1, 0, max_line_length)
    x = CSTParser.parse(text, true)
    t = pretty(x, s)
    nest!(t, s)
    s
end

function run_pretty(text::String)
    d = JLFmt.Document(text)
    s = JLFmt.State(d, 4, 0, 1, 0, 1000)
    x = CSTParser.parse(text, true)
    JLFmt.pretty(x, s)
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

@testset "colon op" begin
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
    @test format("a:b:c") == "a:b:c"
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
end

@testset "strings" begin
    str = """
    \"""
    Interpolate using `\\\$`
    \"""
    """
    @test format(str) == str

    str = """error("foo\\n\\nbar")"""
    @test format(str) == str

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

    @test format("""
    begin
    s = \"\"\"This is a multiline string.
              This is another line.
                  Look another 1 that is indented a bit.

                  cool!\"\"\"
    end""") == str
end

@testset "notcode" begin
    str = """
    module Foo
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
    @test format("""
    module Foo
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
    end""") == str

    str = "# comment 0\n\n\n\n\na = 1\n\n# comment 1\n\n\n\n\nb = 2\n\n\nc = 3\n\n# comment 2\n\n"
    @test format("# comment 0\n\n\n\n\na=1\n\n# comment 1\n\n\n\n\nb = 2\n\n\nc=3\n\n# comment 2\n\n") == str
end

@testset "pretty" begin

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

end

@testset "nesting" begin
    str = """
    function f(arg1::A,
               key1=val1;
               key2=val2) where {A,
                                 F{B,
                                   C}}
        10
        20
    end"""
    @test format("function f(arg1::A,key1=val1;key2=val2) where {A,F{B,C}} 10; 20 end", max_line_length=1) == str

    str = """
    a |
    b |
    c |
    d"""
    @test format("a | b | c | d", max_line_length=1) == str


    str = """
    a, b,
    c, d"""
    @test format("a, b, c, d", max_line_length=6) == str

    str = """
    (a, b,
     c, d)"""
    @test format("(a, b, c, d)", max_line_length=7) == str

    str = """
    [a,
     b,
     c,
     d]"""
    @test format("[a, b, c, d]", max_line_length=1) == str

    str = """
    cond ?
    e1 :
    e2"""
    @test format("cond ? e1 : e2", max_line_length=1) == str

    str = """
    cond ? e1 :
    e2"""
    @test format("cond ? e1 : e2", max_line_length=12) == str

    str = """
    cond1 ? e1 :
    cond2 ? e2 :
    cond3 ? e3 :
    e4"""
    @test format("cond1 ? e1 : cond2 ? e2 : cond3 ? e3 : e4", max_line_length=13) == str

    str = """
    export a,
           b"""
    @test format("export a,b", max_line_length=1) == str

    str = """
    using a,
          b"""
    @test format("using a,b", max_line_length=1) == str

    str = """
    using M: a,
             b"""
    @test format("using M:a,b", max_line_length=1) == str

    str = """
    import M1.M2.M3: a,
                     b"""
    @test format("import M1.M2.M3:a,b", max_line_length=1) == str

    str = """
    foo() = (one,
             x -> (true, false))"""
    @test format("foo() = (one, x -> (true, false))", max_line_length=30) == str

    str = """
    foo() = (one,
             x -> (true,
                   false))"""
    @test format("foo() = (one, x -> (true, false))", max_line_length=20) == str

    str = """
    @somemacro function (fcall_ |
                         fcall_)
        body_
    end"""
    @test format("@somemacro function (fcall_ | fcall_) body_ end", max_line_length=1) == str

    str = "(a; b; c)"
    @test format("(a;b;c)", max_line_length=100) == str
    @test format("(a;b;c)", max_line_length=1) == str

    str = "(x for x in 1:10)"
    @test format("(x   for x  in  1 : 10)", max_line_length=100) == str
    @test format("(x   for x  in  1 : 10)", max_line_length=1) == str

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
    @test s.line_offset == 20
    s = run_nest(str, 20)
    @test s.line_offset == 20
    s = run_nest(str, 19)
    @test s.line_offset == 14
    s = run_nest(str, 1)
    @test s.line_offset == 14

    str = "f(a, b, c) where Union{A,B,C}"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 25
    s = run_nest(str, 25)
    @test s.line_offset == 25
    s = run_nest(str, 24)
    @test s.line_offset == 19
    s = run_nest(str, 1)
    @test s.line_offset == 19

    str = "f(a, b, c) where A"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, 1)
    @test s.line_offset == 12

    str = "f(a, b, c) where A <: S"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, 1)
    @test s.line_offset == 17

    str = "f(a, b, c) where Union{A,B,Union{C,D,E}}"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, 1)
    @test s.line_offset == 26

    str = "f(a, b, c) where {A,{B,C,D},E}"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, 1)
    @test s.line_offset == 14

    str = "(a, b, c, d)"
    s = run_nest(str, 100)
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 3

    str = """
    splitvar(arg) =
        @match arg begin
            ::T_ => (nothing, T)
            name_::T_ => (name, T)
            x_ => (x, :Any)
        end"""
    s = run_nest(str, 96)
    @test s.line_offset == 3
    s = run_nest(str, 1)
    @test s.line_offset == 3

    str = "prettify(ex; lines = false) = ex |> (lines ? identity : striplines) |> flatten |> unresolve |> resyntax |> alias_gensyms"
    s = run_nest(str, 80)
    @test s.line_offset == 41

    str = "export @esc, isexpr, isline, iscall, rmlines, unblock, block, inexpr, namify, isdef"
    s = run_nest(str, length(str))
    @test s.line_offset == length(str)
    s = run_nest(str, length(str)-1)
    @test s.line_offset == 12
end

@testset "additional length" begin
    str = """
    f(a,
      @g(b, c),
      d)"""
    @test format("f(a, @g(b, c), d)", max_line_length=11) == str

    str = """
    f(a,
      @g(b,
         c),
      d)"""
    @test format("f(a, @g(b, c), d)", max_line_length=10) == str

    str = """
    (a,
     (b,
      c),
     d)"""
    @test format("(a, (b, c), d)", max_line_length=7) == str

    str = """
    (a,
     {b,
      c},
     d)"""
    @test format("(a, {b, c}, d)", max_line_length=6) == str

    str = """
    a,
    (b,
     c), d"""
    @test format("a, (b, c), d", max_line_length=6) == str

    str = """
    a,
    (b, c),
    d"""
    @test format("a, (b, c), d", max_line_length=7) == str

    str = """
    (var1,
     var2) &&
    var3"""
    @test format("(var1,var2) && var3", max_line_length=14) == str

    str = """
    (var1, var2) &&
    var3"""
    @test format("(var1,var2) && var3", max_line_length=15) == str

    str = """
    (var1, var2) ?
    (var3, var4) :
    var5"""
    @test format("(var1,var2) ? (var3,var4) : var5", max_line_length=14) == str

    str = """
    (var1,
     var2) ?
    (var3,
     var4) :
    var5"""
    @test format("(var1,var2) ? (var3,var4) : var5", max_line_length=13) == str

    str = """
    (var1, var2) ? (var3, var4) :
    var5"""
    @test format("(var1,var2) ? (var3,var4) : var5", max_line_length=30) == str

    str = """
    (var1, var2) ?
    (var3, var4) :
    var5"""
    @test format("(var1,var2) ? (var3,var4) : var5", max_line_length=29) == str

    str = """
    f(var1::A, var2::B) where {A,
                               B}"""
    @test format("f(var1::A, var2::B) where {A,B}", max_line_length=30) == str

    str = """
    f(var1::A,
      var2::B) where {A,
                      B}"""
    @test format("f(var1::A, var2::B) where {A,B}", max_line_length=28) == str

end

end
