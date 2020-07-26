# JuliaFormatter.jl

[![Documenter: stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://domluna.github.io/JuliaFormatter.jl/stable)
[![Documenter: dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://domluna.github.io/JuliaFormatter.jl/dev)
[![Build Status](https://travis-ci.org/domluna/JuliaFormatter.jl.svg?branch=master)](https://travis-ci.org/domluna/JuliaFormatter.jl)

Width-sensitive formatter for Julia code. Inspired by gofmt, refmt, and black.

![Screencast](https://user-images.githubusercontent.com/1813121/72941091-0b146300-3d68-11ea-9c95-75ec979caf6e.gif)

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

Check out the docs for further description of the formatter and its options.

[Use With Github Actions](https://github.com/julia-actions/julia-format)

### Editor Plugins

For integration with other editors:

  - [Atom](https://github.com/JunoLab/Atom.jl)
  - [VSCode](https://github.com/singularitti/vscode-julia-formatter/)
  - [Vim](https://github.com/kdheepak/JuliaFormatter.vim)
