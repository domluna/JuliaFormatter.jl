# Style

`JuliaFormatter` consists of 3 stages:

1. Prettify
2. Nest
3. Print

## Prettify

Normalizes the `.jl` file into a canonical format. All unnecessary whitespace is removed, code is properly indented, and everything which can fit on a single line, does.

This stage creates a `PTree`.

**(all examples assume indentation of 4 spaces)**

Examples:

Functions, macros, structs with no arguments are placed on a single line.
This also applies to abstract and primitive types.

```julia
function  foo
 end

->

function foo end
```

Functions calls `foo(args)`, tuples `(args)`, arrays `[args]`, braces `{args}`, struct or where definitions `Foo{args}` are placed on a single line. Unless the arguments are surrounded by `{}` each argument is separated by a single space.

```julia
f(
a,b
,c )

->

f(a, b, c)
```

Blocks and their bodies are properly indented.

```julia
begin
  a
    b
c
       end

->

begin
    a
    b
    c
end

# ---

struct Foo{A, B}
 a::A
  b::B
end

->

struct Foo{A,B}
    a::A
    b::B
end
```

Binary calls are placed on a single line. The vast majority of operators and arguments are separated by a single space with the exception of colons and operations inside an indexing expression.

```julia
a+b

-> 

a + b

# ---

a : a : c

->

a:b:c

# ---

list[a + b]

->

list[a+b]
```

## Nesting

Lines going over the maximum margin are split into multiple lines such that they fit inside the margin.

This stage mutates the `PTree` generated from *prettification*.

Most expressions are nested left to right with the exception of binary operations and conditionals which are nested right to left.


Examples:


```julia
arg1 + arg2

->

arg1 + 
arg2

# ---

cond ? e1 : e2

->

cond ? e1 :
e2

->

cond ? 
e1 :
e2
```

Short function definitions and certain blocks - `for`, `while`, `do`, `try`, `if`, or `let` with arguments are initially nested such that the RHS (after `=`) is placed on the next line.

```julia
foo() = body

->

foo() =
    body

# ---

foo = if this_is_a_condition
  a
else
  b
end

->

foo =
    if this_is_a_condition
        a
    catch e
        b
    end
```

Function Calls `f(...)` (also applies to F{}, {}, (), [], etc)

The arguments are indented. The arguments will never pass the initial opening punctuation, i.e. `(` by more than a single space. If a comment is detected in between arguments nesting will be forced.

```julia
f(arg1, arg2, arg3)

->

f(
  arg1,
  arg2,
  arg3,
)

# ---

function longfunctionname_that_is_long(lots, of, args, even, more, args)
    body
end

->

function longfunctionname_that_is_long(
    lots, 
    of, 
    args,
    even, 
    more, 
    args,
)
    body
end

# ---

var = foo(
    a, b, # comment
    c,
)

->

var = foo(
    a,
    b, # comment
    c,
)
```

## Print

This stage prints the mutated `PTree`.
