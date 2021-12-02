# JuliaFormatter.jl

[![Documenter: stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://domluna.github.io/JuliaFormatter.jl/stable)
[![Documenter: dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://domluna.github.io/JuliaFormatter.jl/dev)
![Build Status](https://github.com/domluna/Juliaformatter.jl/actions/workflows/ci.yml/badge.svg)

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

## Formatting Options

### `indent`

> default: `4`

The number of spaces used for an indentation.

### `margin`

> default: `92`

The maximum length of a line. Code exceeding this margin will
be formatted across multiple lines.

### `always_for_in`

> default: `false`

If true, `=` is always replaced with `in` if part of a `for` loop condition.
For example, `for i = 1:10` will be transformed to `for i in 1:10`. Set
this to `nothing` to leave the choice to the user.

### `whitespace_typedefs`

> default: `false`

If true, whitespace is added for type definitions. Make this `true`
if you prefer `Union{A <: B, C}` to `Union{A<:B,C}`.

### `whitespace_ops_in_indices`

> default: `false`

If true, whitespace is added for binary operations in indices. Make this
`true` if you prefer `arr[a + b]` to `arr[a+b]`. Additionally, if there's
a colon `:` involved, parenthesis will be added to the LHS and RHS.

Example: `arr[(i1 + i2):(i3 + i4)]` instead of `arr[i1+i2:i3+i4]`.

### `remove_extra_newlines`

> default: `false`

If true, superflous newlines will be removed. For example:

```julia
module M



a = 1

function foo()


    return nothing

end


b = 2


end
```

is rewritten as

```julia
module M

a = 1

function foo()
    return nothing
end

b = 2

end
```

Modules are the only type of code block allowed to keep a single newline
prior to the intial or after the final piece of code.

### `import_to_using`

> default: `false`

If true, `import` expressions are rewritten to `using` expressions
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

Exceptions:

If `as` is found in the import expression. `using` CANNOT be used in this context. The following example will NOT BE rewritten.

```julia
import Base.Threads as th
```

If `import` is used in the following context it is NOT rewritten. This may change in a future patch.

```julia
@everywhere import A, B
```

### `pipe_to_function_call`

> default: `false`

If true, `x |> f` is rewritten to `f(x)`.

### `short_to_long_function_def`

> default: `false`

Transforms a *short* function definition

```julia
f(arg1, arg2) = body
```

to a *long* function definition

```julia
function f(arg2, arg2)
    body
end
```

### `always_use_return`

> default: `false`

If true, `return` will be prepended to the last expression where
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

### `whitespace_in_kwargs`

> default: `true`

If true, `=` in keyword arguments will be surrounded by whitespace.

```julia
f(; a=4)
```

to

```julia
f(; a = 4)
```

An exception to this is if the LHS ends with "!" then even if `whitespace_in_kwargs` is
false, `=` will still be surrounded by whitespace. The logic behind this intervention being
on the following parse the `!` will be treated as part of `=`, as in a "not equal" binary
operation. This would change the semantics of the code and is therefore disallowed.

### `annotate_untyped_fields_with_any`

> default: `true`

Annotates fields in a type definitions with `::Any` if no type annotation is provided:

```julia
struct A
    arg1
end
```

to

```julia
struct A
    arg1::Any
end
```

### `format_docstrings`

> default: `false`

Format code docstrings with the same options used for the code source.

Markdown is formatted with [`CommonMark`](https://github.com/MichaelHatherly/CommonMark.jl) alongside Julia code.

### `align_*`

> default: `false`

See `Custom Alignment` documentation.

### `conditional_to_if`

> default: `false`

If the conditional `E ? A : B` exceeds the maximum margin converts it into the equivalent `if` block:

```julia
if E
    A
else
    B
end
```

### `normalize_line_endings`

> default: `"auto"`

One of `"unix"` (normalize all `\r\n` to `\n`), `"windows"` (normalize all `\n` to `\r\n`), `"auto"` (automatically
choose based on which line ending is more common in the file).

### `trailing_comma`

> default: `true`

One of `true`, `false`, or `nothing`.

Trailing commas are added after the final argument when nesting occurs and the closing punctuation appears on the next line.

For example when the following is nested (assuming `DefaultStyle`):

```julia
funccall(arg1, arg2, arg3)
```

it turns into:

```julia
funccall(
    arg1,
    arg2,
    arg3, # trailing comma added after `arg3` (final arugment) !!!
)
```

* When set to `true`, the trailing comma is always added during nesting.
* When set to `false`, the trailing comma is always removed during nesting.
* When set to `nothing`, the trailing comma appears as it does in the original source.

### `join_lines_based_on_source`

> default: `false`

When `true` lines are joined as they appear in the original source file.

```julia
function foo(arg1,
                       arg2, arg3
                       )
       body
end
```

When `false` and the maximum margin is > than the length of `"function foo(arg1, arg2, arg3)"`
this is formatted to

```julia
function foo(arg1, arg2, arg3)
    body
end
```

When `true`, `arg1` and `arg2, arg3` will remain on separate lines even if they can fit on the
same line since it's within maximum margin. The indentation is dependent on the style.

```julia
function foo(arg1,
    arg2, arg3,
)
end
```

There are exceptions to this:

```julia
if a body1 elseif b body2 else body3 end
```

will be formatted to the following, even if this option is set to `true`:

```julia
if a
    body1
elseif b
    body2
else
    body3
end
```

!!! warning

    The maximum margin still applies even when this option is set to `true`.

### `indent_submodule`

> default: `false`

When set to `true`, submodule(s) appearing in the same file will be indented.

```julia
module A
a = 1

module B
b = 2
module C
c = 3
end
end

d = 4

end
```

will be formatted to:

```julia
module A
a = 1

module B
    b = 2
    module C
        c = 3
    end
end

d = 4

end
```

### File Options

### `overwrite`

> default: `true`

If `true` the file will be reformatted in place, overwriting the existing file;
if it is `false`, the formatted version of foo.jl will not be written anywhere.

### `verbose`

> default: `false`

If `true` details related to formatting the file will be printed to `stdout`.

### `format_markdown`

> default: `false`

If `true`, Markdown files are also formatted. Julia code blocks will be formatted in
addition to the Markdown being normalized.

## Editor Plugins

For integration with other editors:

  - [Atom](https://github.com/JunoLab/Atom.jl)
  - [Emacs](https://codeberg.org/FelipeLema/julia-formatter.el)
  - [VSCode](https://github.com/singularitti/vscode-julia-formatter/)
  - [Vim](https://github.com/kdheepak/JuliaFormatter.vim)
