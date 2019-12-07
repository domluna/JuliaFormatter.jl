# JuliaFormatter.jl

[![Build Status](https://travis-ci.org/domluna/JuliaFormatter.jl.svg?branch=master)](https://travis-ci.org/domluna/JuliaFormatter.jl)

Width-sensitive formatter for Julia code. Inspired by gofmt, refmt, and black. Built with [`CSTParser`](https://github.com/ZacLN/CSTParser.jl).

## Installation

```julia
]add JuliaFormatter
```

## Quick Start

```julia
julia> using JuliaFormatter

# Recursively formats all Julia files in the current directory
julia> format(".")
```

## Usage

`JuliaFormatter` exports `format_text`, `format_file` and `format`:

```julia
format_text(
    text::AbstractString;
    indent = 4,
    margin = 92,
    always_for_in = false,
    whitespace_typedefs::Bool = false,
    whitespace_ops_in_indices::Bool = false,
)

format_file(
    file::AbstractString;
    overwrite = true,
    verbose = false,
    indent = 4,
    margin = 92,
    always_for_in = false,
    whitespace_typedefs::Bool = false,
    whitespace_ops_in_indices::Bool = false,
)

format(
    paths...;
    overwrite = true,
    verbose = false,
    indent = 4,
    margin = 92,
    always_for_in = false,
    whitespace_typedefs::Bool = false,
    whitespace_ops_in_indices::Bool = false,
)
```

The `text` argument to `format_text` is a string containing the code to be formatted; the formatted code is retuned as a new string. The `file` argument to `format_file` is the path of a file to be formatted. The `format` function is either called with a singe string to format if it is a `.jl` file or to recuse into looking for `.jl` files if it is a directory. It can also be called with a collection of such paths to iterate over.

### File Options

If `overwrite` is `true` the file will be reformatted in place, overwriting
the existing file; if it is `false`, the formatted version of `foo.jl` will
be written to `foo_fmt.jl` instead.

If `verbose` is `true` details related to formatting the file will be printed
to `stdout`.

### Formatting Options

`indent` - the number of spaces used for an indentation.

`margin` - the maximum length of a line. Code exceeding this margin will be formatted
across multiple lines.

If `always_for_in` is true `=` is always replaced with `in` if part of a
`for` loop condition.  For example, `for i = 1:10` will be transformed
to `for i in 1:10`.

If `whitespace_typedefs` is true, whitespace is added for type definitions.
Make this `true` if you prefer `Union{A <: B, C}` to `Union{A<:B,C}`.

If `whitespace_ops_in_indices` is true, whitespace is added for binary operations
in indices. Make this `true` if you prefer `arr[a + b]` to `arr[a+b]`. Additionally,
if there's a colon `:` involved, parenthesis will be added to the LHS and RHS.
Example: `arr[(i1 + i2):(i3 + i4)]` instead of `arr[i1+i2:i3+i4]`.
