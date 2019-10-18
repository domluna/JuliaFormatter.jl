# JuliaFormatter.jl

[![Build Status](https://travis-ci.org/domluna/JuliaFormatter.jl.svg?branch=master)](https://travis-ci.org/domluna/JuliaFormatter.jl)
<!-- [![Coverage Status](https://coveralls.io/repos/github/domluna/JuliaFormatter.jl/badge.svg?branch=master)](https://coveralls.io/github/domluna/JuliaFormatter.jl?branch=master) -->

Width-sensitive formatter for Julia code. Inspired by gofmt and refmt.

```julia
]add JuliaFormatter
```

`JuliaFormatter` exports `format_text`, `format_file` and `format`:

```julia
format_text(
    text::AbstractString;
    indent = 4,
    margin = 92,
    always_for_in = false,
)

format_file(
    file::AbstractString;
    indent = 4,
    margin = 92,
    overwrite = true,
    verbose = false,
    always_for_in = false,
)

format(
    paths...;
    indent = 4,
    margin = 92,
    overwrite = true,
    verbose = false,
    always_for_in = false,
)
```

The `text` argument to `format_text` is a string containing the code to be formatted; the formatted code is retuned as a new string. The `file` argument to `format_file` is the path of a file to be formatted. The `format` function is either called with a singe string to format if it is a `.jl` file or to recuse into looking for `.jl` files if it is a directory. It can also be called with a collection of such paths to iterate over.

*Options:*

* `indent` - The number of spaces used for an indentation.
* `margin` - The maximum number of characters of code on a single line. Lines over
the limit will be wrapped if possible. There are cases where lines cannot be wrapped
and they will still end up wider than the requested margin.
* `overwrite` - If the file should be overwritten by the formatted output. If set to false, the formatted version of file named `foo.jl` will be written to `foo_fmt.jl`.
* `verbose` - Whether to print the name of the file being formatted along with relevant details to `stdout`.
* `always_for_in` - Always use `in` keyword for `for` loops. This defaults to `false`.

There is also a command-line tool `bin/format.jl` which can be invoked with `-i`/`--indent` and `-m`/`--margin` and with paths which will be passed to `format`.


## How It Works

`JuliaFormatter` parses a `.jl` source file into a Concrete Syntax Tree (CST) using [`CSTParser`](https://github.com/ZacLN/CSTParser.jl).

### Pass 1: Prettify

The CST is "prettified", creating a `PTree`. The printing output of a `PTree` is a canonical representation of the code removing unnecessary whitespace and joining or separating lines of code; expressions will be attempted to be placed on a single line.

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
  arg3,
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
    args,
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
   Type3,
}
```

If a comment is detected inside the `PTree` it nesting will be forced. For example:

```julia
var = foo(
    a, b, # comment
    c,
)
```

formatted result will be (regardless of the margin)

```julia
var = foo(
    a,
    b, # comment
    c,
)
```

### Part 3: Printing

Finally, the `PTree` is printed to an `IOBuffer`. Prior to returning the formatted text a final validity
check is performed.

## Skipping Formatting

By default formatting is always on. Formatting can be turned toggled with the following comments:

```julia
# format: off
# turns off formatting from this point onwards
...

# format: on
# turns formatting back on from this point onwards
```

These can be used throughout a file or if you wish an entire file not be formatted add "format: off" at the top of the file:

```julia
# format: off

module Foo
...
end
```

Note the formatter expects `# format: on` and `# format: off` to be on its own line and the whitespace to be an exact match.

## Syntax Tree Transformations

### `for in` vs. `for =`

By default if the RHS is a range, i.e. `1:10` then `for in` is converted to `for =`. Otherwise `for =` is converted to `for in`. See [this issue](https://github.com/domluna/JuliaFormatter.jl/issues/34) for the rationale and further explanation.

Alternative to the above - setting `always_for_in` to `true`, i.e. `format_text(..., always_for_in = true)` will always convert `=` to `in` even if the RHS is a range.

### Trailing Commas

If an iterable expression is nested a trailing comma is added to the last argument. The trailing comma is removed if the expressions is unnested:


```julia
f(a, b, c)

->

f(
  a,
  b,
  c,
)
```

See [this issue](https://github.com/domluna/JuliaFormatter.jl/issues/44) for more details.



### Trailing Semicolons

If a matrix expression is nested the semicolons are removed.

```julia
[1 0; 0 1]

->

[
 1 0
 0 1
]
```

See [this issue](https://github.com/domluna/JuliaFormatter.jl/issues/77) for more details.

### Leading and trailing 0s for float literals

If a float literal is missing a *trailing* 0 it is added:

```julia
a = 1.

->

a = 1.0
```

If a float literal is missing a *leading* 0 it is added:

```julia
a = .1

->

a = 0.1
```

See [this issue](https://github.com/domluna/JuliaFormatter.jl/issues/66) for more details.

### Surround `where` arguments with curly brackets

If the arguments of a `where` call are not surrounded by curly brackets, they are added:


```julia
foo(x::T) where T = ...

->

foo(x::T) where {T} = ...
```

See [this issue](https://github.com/domluna/JuliaFormatter.jl/issues/53) for more details.
