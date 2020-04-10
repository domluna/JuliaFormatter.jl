# JuliaFormatter.jl

[![Build Status](https://travis-ci.org/domluna/JuliaFormatter.jl.svg?branch=master)](https://travis-ci.org/domluna/JuliaFormatter.jl)

Width-sensitive formatter for Julia code. Inspired by gofmt, refmt, and black. Built with [`CSTParser`](https://github.com/ZacLN/CSTParser.jl).

![](https://user-images.githubusercontent.com/1813121/72941091-0b146300-3d68-11ea-9c95-75ec979caf6e.gif)

## Installation

```julia
]add JuliaFormatter
```

## Quick Start

```julia
julia> using JuliaFormatter

# Recursively formats all Julia files in the current directory
julia> format(".")

# Formats an individual file
julia> format_file("foo.jl")

# Formats a string (contents of a Julia file)
julia> format_text(str)
```

## Usage

`JuliaFormatter` exports `format_text`, `format_file` and `format`:

```julia
format_text(
    text::AbstractString;
    indent = 4,
    margin = 92,
    style::AbstractStyle = DefaultStyle(),
    always_for_in = false,
    whitespace_typedefs::Bool = false,
    whitespace_ops_in_indices::Bool = false,
    remove_extra_newlines::Bool = false,
    import_to_using::Bool = false,
    pipe_to_function_call::Bool = false,
    short_to_long_function_def::Bool = false,
    always_use_return::Bool = false,
)

format_file(
    file::AbstractString;
    overwrite = true,
    verbose = false,
    format_options...,
)

format(
    paths...;
    options...,
)
```

The `text` argument to `format_text` is a string containing the code to be formatted; the formatted code is retuned as a new string. The `file` argument to `format_file` is the path of a file to be formatted. The `format` function is either called with a singe string to format if it is a `.jl` file or to recuse into looking for `.jl` files if it is a directory. It can also be called with a collection of such paths to iterate over.

`format` calls `format_file` which in turn calls `format_text`.

### File Options

#### `overwrite`

The file will be reformatted in place, overwriting the existing file.
If it is `false`, the formatted version of `foo.jl` will be written to
`foo_fmt.jl` instead.

#### `verbose`

Details related to formatting the file will be printed to `stdout`.

### Formatting Options

Formats a Julia source passed in as a string, returning the formatted
code as another string.

#### `indent`

The number of spaces used for an indentation.

#### `margin`

The maximum length of a line. Code exceeding this margin will
be formatted across multiple lines.

#### `always_for_in`

If true `=` is always replaced with `in` if part of a `for` loop condition.
For example, `for i = 1:10` will be transformed to `for i in 1:10`.

#### `whitespace_typedefs`

If true, whitespace is added for type definitions.  Make this `true`
if you prefer `Union{A <: B, C}` to `Union{A<:B,C}`.

#### `whitespace_ops_in_indices`

If true, whitespace is added for binary operations in indices. Make this
`true` if you prefer `arr[a + b]` to `arr[a+b]`. Additionally, if there's
a colon `:` involved, parenthesis will be added to the LHS and RHS.

Example: `arr[(i1 + i2):(i3 + i4)]` instead of `arr[i1+i2:i3+i4]`.

#### `remove_extra_newlines`

If true superflous newlines will be removed. For example:

```julia
a = 1



b = 2
```

is rewritten as

```julia
a = 1

b = 2
```

#### `import_to_using`

If true `import` expressions are rewritten to `using` expressions
in the following cases:

```julia
import A

import A, B, C
```

is rewritten to:

```julia
using A: A

using A: A
using B: B
using C: C
```

#### `pipe_to_function_call`

If true `x |> f` is rewritten to `f(x)`.

#### `short_to_long_function_def`

Transforms a _short_ function definition

```julia
f(arg1, arg2) = body
```

to a _long_ function definition

```julia
function f(arg2, arg2)
    body
end
```

#### `always_use_return`

If true `return` will be prepended to the last expression where
applicable in function definitions, macro definitions, and do blocks.

Example:

```julia
function foo()
    expr1
    expr2
end
```

to

```julia
function foo()
    expr1
    return expr2
end
```
