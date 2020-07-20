```@meta
CurrentModule = JuliaFormatter
```

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

The `text` argument to `format_text` is a string containing the code to be formatted; the formatted code is retuned as a new string. The `file` argument to `format_file` is the path of a file to be formatted. The `format` function is either called with a singe string to format if it is a `.jl` file or to recuse into looking for `.jl` files if it is a directory. It can also be called with a collection of such paths to iterate over. The `format_file` and `format` functions will return `true` if the file(s) are already formatted (i.e. if no changes were made to the file(s)) and `false` if changes were required to format the file(s).

[`format`](@ref) calls [`format_file`](@ref) which in turn calls [`format_text`](@ref).
