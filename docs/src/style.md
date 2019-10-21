# Style

`JuliaFormatter` consists of 3 stages:

1. Prettify
2. Nest
3. Print

## Prettify

Normalizes the `.jl` file into a canonical format. All unnecessary whitespace is removed, code is properly indented, and everything which can fit on a single line, does. 

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

foo[a + b]

->

foo[a+b]
```

## Nesting

Lines going over the maximum margin are split into multiple lines so thatthey fit. 

Binary operations and conditionals are nested back to front.

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

Short function definitions and 

```julia
foo() = body

->

foo() =
    body
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

### Part 3: Printing

Finally, the `PTree` is printed to an `IOBuffer`. Prior to returning the formatted text a final validity
check is performed.
