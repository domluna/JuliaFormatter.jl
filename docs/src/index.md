```@meta
CurrentModule = JuliaFormatter
```

# JuliaFormatter.jl

[![Build Status](https://travis-ci.org/domluna/JuliaFormatter.jl.svg?branch=master)](https://travis-ci.org/domluna/JuliaFormatter.jl)

Width-sensitive formatter for Julia code. Inspired by gofmt, refmt, black, and prettier. Built with [`CSTParser`](https://github.com/ZacLN/CSTParser.jl).

- Sane defaults out of the box with options to customize.
- Supports [YAS](https://github.com/jrevels/YASGuide) and [Blue](https://github.com/invenia/BlueStyle) style guides.
- `JuliaFormatter.toml` configuration file to store options.

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

`JuliaFormatter` exports [`format_text`](@ref), [`format_file`](@ref), [`format_text`](@ref), and [`format_md`](@ref):

`JuliaFormatter` should work on any valid Julia and Markdown files.
If `JuliaFormatter` cannot parse the code for any reason, it will throw an error pointing to the line that could not be parsed.
If running [`format`](@ref) on multiple files, you may want to set `verbose = true` to print information about which file is being formatted.
