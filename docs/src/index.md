# JuliaFormatter.jl

[![Build Status](https://travis-ci.org/domluna/JuliaFormatter.jl.svg?branch=master)](https://travis-ci.org/domluna/JuliaFormatter.jl)

Width-sensitive formatter for Julia code. Inspired by gofmt, refmt, and black built with [`CSTParser`](https://github.com/ZacLN/CSTParser.jl).

## Installation

```julia
]add JuliaFormatter
```

## Usage

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

