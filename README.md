# JLFmt

[![Build Status](https://travis-ci.org/domluna/JLFmt.jl.svg?branch=master)](https://travis-ci.org/domluna/JLFmt.jl)
<!-- [![Coverage Status](https://coveralls.io/repos/github/domluna/JLFmt.jl/badge.svg?branch=master)](https://coveralls.io/github/domluna/JLFmt.jl?branch=master) -->

Width-sensitive formatter for Julia code. Inspired by gofmt and refmt.

```julia
]add https://github.com/domluna/JLFmt.jl
```

`JLFmt` exports `format` and `format_file`:

```julia
format(text::String, indent_size, print_width)
format_file(text::String, indent_size, print_width; overwrite=false)
```

* `indent_size` - the number of spaces used for an indentation.
* `print_width` - the maximum number of characters of code on a single line. Lines over
the limit will be nested if possible.
* `overwrite` - if the file should be overwritten by the formatted output. Currently
the ouput is written to a separate file.


## How It Works

`JLFmt` parses the `.jl` source file into a Concrete Syntax Tree (CST) using [`CSTParser`](https://github.com/ZacLN/CSTParser.jl).

### Pass 1: Prettify

The CST is "prettified", creating a `PTree`. The printing output of a `PTree` is a canonical representation of the code removing unnecessary whitespace and joining or separating lines of code. The [`pretty` testset](./test/runtests.jl) displays these transformations.

Example:

```julia
function  foo
end

->

function foo end
```

### Pass 2: Nesting

In the nesting phase lines going over the print width are split into multiple lines such that they fit within
the allowed width. All expressions are nested front to back with the exception of binary operations and conditionals.

Examples:


```julia
arg1 + arg2

->

arg1 + 
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
    body
end

->

function longfunctionname_that_is_long(
    lots, 
    of, 
    args,
    even, 
    more, 
    args
)
    body
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

Finally, the `PTree` is printed to an `IOBuffer`. Prior to returning the formatted text a final validity
check is performed.

## Skipping Files

If you wish to not format a file this can be done by writing "nofmt" in a comment on the first line
of the file. Suppose this is the content of `foo.jl`:

```julia
# nofmt

module Foo
...
end
```

`JLFmt` will see the comment on the first line, notice it contains "nofmt", and return the original text.

## Present Limitation(s)

Inline comments inside of a nestable types are removed.

Example:

```julia
function foo(
    a, # a does ...
    b, # b does ...
    c
)
    body
end
```

When formatted will produce:

```julia
function foo(
    a,
    b,
    c
)
    body
end
```

