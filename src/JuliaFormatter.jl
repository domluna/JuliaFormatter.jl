module JuliaFormatter

using CSTParser
using Tokenize
using DataStructures
using Pkg.TOML: parsefile
import CommonMark: block_modifier
using CommonMark:
    AdmonitionRule,
    CodeBlock,
    enable!,
    FootnoteRule,
    markdown,
    MathRule,
    Parser,
    Rule,
    TableRule

export format, format_text, format_file, format_md, DefaultStyle, YASStyle, BlueStyle

abstract type AbstractStyle end

@inline options(s::AbstractStyle) = NamedTuple()

"""
    DefaultStyle

The default formatting style. See the [Style](@ref) section of the documentation
for more details.

See also: [`BlueStyle`](@ref), [`YASStyle`](@ref)
"""
struct DefaultStyle <: AbstractStyle
    innerstyle::Union{Nothing,AbstractStyle}
end
DefaultStyle() = DefaultStyle(nothing)

@inline getstyle(s::DefaultStyle) = s.innerstyle === nothing ? s : s.innerstyle
function options(s::DefaultStyle)
    return (;
        indent = 4,
        margin = 92,
        always_for_in = false,
        whitespace_typedefs = false,
        whitespace_ops_in_indices = false,
        remove_extra_newlines = false,
        import_to_using = false,
        pipe_to_function_call = false,
        short_to_long_function_def = false,
        always_use_return = false,
        whitespace_in_kwargs = true,
        annotate_untyped_fields_with_any = true,
        format_docstrings = false,
        align_struct_field = false,
        align_assignment = false,
        align_conditional = false,
        align_pair_arrow = false,
        conditional_to_if = false,
        normalize_line_endings = "auto",
        align_matrix = false,
        trailing_comma = true,
    )
end

if VERSION < v"1.3"
    # https://github.com/JuliaLang/julia/blob/1b93d53fc4bb59350ada898038ed4de2994cce33/base/regex.jl#L416-L428
    function count(t::Union{AbstractString,Regex}, s::AbstractString; overlap::Bool = false)
        n = 0
        i, e = firstindex(s), lastindex(s)
        while true
            r = findnext(t, s, i)
            r === nothing && break
            n += 1
            j = overlap || isempty(r) ? first(r) : last(r)
            j > e && break
            @inbounds i = nextind(s, j)
        end
        return n
    end
end
count(args...; kwargs...) = Base.count(args...; kwargs...)

include("document.jl")
include("options.jl")
include("state.jl")
include("fst.jl")
include("passes.jl")
include("align.jl")

include("styles/default/pretty.jl")
include("styles/default/nest.jl")
include("styles/yas/pretty.jl")
include("styles/yas/nest.jl")
include("styles/blue/pretty.jl")
include("styles/blue/nest.jl")

include("nest_utils.jl")

include("print.jl")

include("markdown.jl")

include("copied_from_documenter.jl")

const UNIX_TO_WINDOWS = r"\r?\n" => "\r\n"
const WINDOWS_TO_UNIX = "\r\n" => "\n"
function choose_line_ending_replacer(text)
    rn = count("\r\n", text)
    n = count(r"(?<!\r)\n", text)
    n >= rn ? WINDOWS_TO_UNIX : UNIX_TO_WINDOWS
end
normalize_line_ending(s::AbstractString, replacer = WINDOWS_TO_UNIX) = replace(s, replacer)

"""
    format_text(
        text::AbstractString;
        style::AbstractStyle = DefaultStyle(),
        indent::Int = 4,
        margin::Int = 92,
        always_for_in::Union{Bool,Nothing} = false,
        whitespace_typedefs::Bool = false,
        whitespace_ops_in_indices::Bool = false,
        remove_extra_newlines::Bool = false,
        import_to_using::Bool = false,
        pipe_to_function_call::Bool = false,
        short_to_long_function_def::Bool = false,
        always_use_return::Bool = false,
        whitespace_in_kwargs::Bool = true,
        annotate_untyped_fields_with_any::Bool = true,
        format_docstrings::Bool = false,
        align_struct_field::Bool = false,
        align_conditional::Bool = false,
        align_assignment::Bool = false,
        align_pair_arrow::Bool = false,
        conditional_to_if = false,
        normalize_line_endings = "auto",
        align_matrix::Bool = false,
        trailing_comma::Bool = false,
    )::String

Formats a Julia source passed in as a string, returning the formatted
code as another string.

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
"""
function format_text(text::AbstractString; style::AbstractStyle = DefaultStyle(), kwargs...)
    return format_text(text, style; kwargs...)
end

function format_text(text::AbstractString, style::AbstractStyle; kwargs...)
    isempty(text) && return text
    opts = Options(; merge(options(style), kwargs)...)
    return format_text(text, style, opts)
end

function format_text(text::AbstractString, style::AbstractStyle, opts::Options)
    cst, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    line, offset = ps.lt.endpos
    ps.errored && error("Parsing error for input occurred on line $line, offset: $offset")
    CSTParser.is_nothing(cst[1]) && length(cst) == 1 && return text
    return format_text(cst, style, State(Document(text), opts))
end

function format_text(cst::CSTParser.EXPR, style::AbstractStyle, s::State)
    fst = try
        pretty(style, cst, s)
    catch e
        loc = cursor_loc(s, s.offset - 1)
        @warn "Error occured during prettification" line = loc[1] offset = loc[2]
        rethrow()
    end
    hascomment(s.doc, fst.endline) && (add_node!(fst, InlineComment(fst.endline), s))

    s.opts.pipe_to_function_call && pipe_to_function_call_pass!(fst)

    flatten_fst!(fst)

    needs_alignment(s.opts) && align_fst!(fst, s.opts)

    nest!(style, fst, s)

    # ignore maximum width can be extra whitespace at the end of lines
    # remove it all before we print.
    s.opts.join_lines_based_on_source && remove_superflous_whitespace!(fst)

    s.line_offset = 0
    io = IOBuffer()

    # Print comments and whitespace before code.
    if fst.startline > 1
        format_check(io, Notcode(1, fst.startline - 1), s)
        print_leaf(io, Newline(), s)
    end
    print_tree(io, fst, s)
    if fst.endline < length(s.doc.range_to_line)
        print_leaf(io, Newline(), s)
        format_check(io, Notcode(fst.endline + 1, length(s.doc.range_to_line)), s)
    end

    text = String(take!(io))

    replacer = if s.opts.normalize_line_endings == "unix"
        WINDOWS_TO_UNIX
    elseif s.opts.normalize_line_endings == "windows"
        UNIX_TO_WINDOWS
    else
        choose_line_ending_replacer(s.doc.text)
    end
    text = normalize_line_ending(text, replacer)

    _, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    line, offset = ps.lt.endpos
    ps.errored && error(
        "Parsing error for formatted text:\n\n$text\n\n Error occured on line $line, offset $offset.",
    )
    return text
end

"""
    format_file(
        filename::AbstractString;
        overwrite::Bool = true,
        verbose::Bool = false,
        format_markdown::Bool = false,
        format_options...,
    )::Bool

Formats the contents of `filename` assuming it's a `.jl` or `.md` file.

## File Options

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

## Formatting Options

See [`format_text`](@ref) for description of formatting options.

## Output

Returns a boolean indicating whether the file was already formatted (`true`) or not (`false`).
"""
function format_file(
    filename::AbstractString;
    overwrite::Bool = true,
    verbose::Bool = false,
    format_markdown::Bool = false,
    format_options...,
)::Bool
    path, ext = splitext(filename)
    shebang_pattern = r"^#!\s*/.*\bjulia[0-9.-]*\b"
    formatted_str = if ext == ".md"
        format_markdown || return true
        verbose && println("Formatting $filename")
        str = String(read(filename))
        format_md(str; format_options...)
    elseif ext == ".jl" || match(shebang_pattern, readline(filename)) !== nothing
        verbose && println("Formatting $filename")
        str = String(read(filename))
        format_text(str; format_options...)
    else
        error("$filename must be a Julia (.jl) or Markdown (.md) source file")
    end
    formatted_str = replace(formatted_str, r"\n*$" => "\n")
    already_formatted = (formatted_str == str)
    if overwrite && !already_formatted
        write(filename, formatted_str)
    end
    return already_formatted
end

"""
    format_file(filename::AbstractString, style::AbstractStyle; kwargs...)::Bool
"""
function format_file(filename::AbstractString, style::AbstractStyle; kwargs...)
    return format_file(filename; style = style, kwargs...)
end

const CONFIG_FILE_NAME = ".JuliaFormatter.toml"

"""
    format(
        paths; # a path or collection of paths
        options...,
    )::Bool

Recursively descend into files and directories, formatting any `.jl` files by calling
`format_file` on them.

See [`format_file`](@ref) and [`format_text`](@ref) for a description of the options.

This function will look for `.JuliaFormatter.toml` in the location of the file being
formatted, and searching *up* the file tree until a config file is (or isn't) found.
When found, the configurations in the file will overwrite the given `options`.
See [Configuration File](@ref) for more details.

### Output

Returns a boolean indicating whether the file was already formatted (`true`)
or not (`false`).
"""
function format(paths; options...)::Bool
    dir2config = Dict{String,Any}()
    already_formatted = true
    function find_config_file(dir)
        next_dir = dirname(dir)
        config = if (next_dir == dir || # ensure to escape infinite recursion
                     isempty(dir)) # reached to the system root
            nothing
        elseif haskey(dir2config, dir)
            dir2config[dir]
        else
            path = joinpath(dir, CONFIG_FILE_NAME)
            isfile(path) ? parse_config(path) : find_config_file(next_dir)
        end
        return dir2config[dir] = config
    end

    for path in paths
        already_formatted &= if isfile(path)
            dir = dirname(realpath(path))
            opts = if (config = find_config_file(dir)) !== nothing
                overwrite_options(options, config)
            else
                options
            end
            format_file(path; opts...)
        else
            reduce(walkdir(path), init = true) do formatted_path, dir_branch
                root, dirs, files = dir_branch
                root = normpath(root)
                formatted_path & reduce(files, init = true) do formatted_file, file
                    _, ext = splitext(file)
                    full_path = joinpath(root, file)
                    formatted_file &
                    if ext in (".jl", ".md") &&
                       !(".git" in split(full_path, Base.Filesystem.path_separator))
                        dir = abspath(root)
                        opts = if (config = find_config_file(dir)) !== nothing
                            overwrite_options(options, config)
                        else
                            options
                        end
                        format_file(full_path; opts...)
                    else
                        true
                    end
                end
            end
        end
    end
    return already_formatted
end
format(path::AbstractString; options...) = format((path,); options...)

"""
    format(path, style::AbstractStyle; options...)::Bool
"""
format(path, style::AbstractStyle; options...) = format(path; style = style, options...)

"""
    format(mod::Module, args...; options...)
"""
format(mod::Module, args...; options...) = format(pkgdir(mod), args...; options...)

function kwargs(dict)
    ns = (Symbol.(keys(dict))...,)
    vs = (collect(values(dict))...,)
    return pairs(NamedTuple{ns}(vs))
end

function parse_config(tomlfile)
    config_dict = parsefile(tomlfile)
    if (style = get(config_dict, "style", nothing)) !== nothing
        @assert (style == "default" || style == "yas" || style == "blue") "currently $(CONFIG_FILE_NAME) accepts only \"default\" or \"yas\" or \"blue\" for the style configuration"
        config_dict["style"] = if (style == "yas" && @isdefined(YASStyle))
            YASStyle()
        elseif (style == "blue" && @isdefined(BlueStyle))
            BlueStyle()
        else
            DefaultStyle()
        end
    end
    return kwargs(config_dict)
end

overwrite_options(options, config) = merge(NamedTuple(options), NamedTuple(config))

if Base.VERSION >= v"1.5"
    include("other/precompile.jl")
    _precompile_()
end

end
