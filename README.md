# JLFmt

[![Build Status](https://travis-ci.org/domluna/JLFmt.jl.svg?branch=master)](https://travis-ci.org/domluna/JLFmt.jl)
<!-- [![Coverage Status](https://coveralls.io/repos/github/domluna/JLFmt.jl/badge.svg?branch=master)](https://coveralls.io/github/domluna/JLFmt.jl?branch=master) -->

**WARNING.** This an alpha release meant to find bugs. Overwrite files at your peril!

Width-sensitive formatter for Julia code. Inspired by gofmt and refmt.

```julia
]add https://github.com/domluna/JLFmt.jl
```

`JLFmt` exports a singular function:

```julia
format(text::String; indent_size=4, print_width=80)
```

* `indent_size` - the number of spaces used for an indentation.
* `print_width` - the maximum number of characters of code on a single line. Lines over
the limit will be nested if possible.

## How It Works

`JLFmt` parses the `.jl` source file into a Concrete Syntax Tree (CST) using [`CSTParser`](https://github.com/ZacLN/CSTParser.jl).

### Pass 1: Prettify

The CST is _prettified_ using `pretty`, creating a `PTree`. The printing output of a `PTree` is a canonical representation of the code removing unnecessary whitespace and joining or separating lines of code. The [`pretty` testset](./test/runtests.jl) displays these transformations.

Example 1:

```julia
function  foo
end
```

becomes

```julia
function foo end
```

Example 2:

```julia
for cond 1; 2; 3 end
```

becomes

```julia
for cond
    1
    2
    3
end
```

### Pass 2: Nesting

`PTree` is nested using `nest!` to disjoin lines going over the print width. 
The `PTree` is modified in-place. All expressions are nested front to back with the exception of binary 
operations and conditionals.

**Binary Ops**

```julia
arg1 && arg2

->

arg1 && 
arg2
```

```julia
foo() = body

->

# The indentation of the body is based on `indent_size`
foo() =
    body
```

**Conditionals**

```julia
cond ? e1 : e2

->

cond ? e1 :
e2

->

cond ? 
e1 :
e2
```

**Calls** (also applies to {}, (), [], etc)

```julia
f(arg1, arg2, arg3)

->

f(
  arg1,
  arg2,
  arg3
)
```

```julia
function longfunctionname_that_is_long(lots, of, args, even, more, args)
    ..code..
end

->

# The arg is placed `indent_size` spaces after the start of the
# function name or one space after the opening parenthesis.
function longfunctionname_that_is_long(
             lots, 
             of, 
             args,
             even, 
             more, 
             args
         )
    ..code..
end
```

```julia
S <: Union{Type1,Type2,Type3}

->

S <: Union{
         Type1,
         Type2,
         Type3
     }
```

### Part 3: Printing

Finally, the `PTree` to an `IOBuffer` with `print_tree` which is then consumed as a `String`.

### Known Limitation(s)

If a comment is at the end of a line of code it will be removed.

```julia
var x = 10 # comment about x
```

Formatting will produce:

```julia
var x = 10
```

To get around with this write comments on separate lines.
