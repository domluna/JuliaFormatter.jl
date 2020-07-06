module JuliaFormatter

using CSTParser
using Tokenize
using DataStructures
using Pkg.TOML: parsefile

export format, format_text, format_file, DefaultStyle, YASStyle, DocumentFormatStyle

abstract type AbstractStyle end

"""
    DefaultStyle

The default formatting style. See the style section of the documentation
for more details.
"""
struct DefaultStyle <: AbstractStyle
    innerstyle::Union{Nothing,AbstractStyle}
end
DefaultStyle() = DefaultStyle(nothing)

@inline getstyle(s::DefaultStyle) = s.innerstyle === nothing ? s : s.innerstyle

include("document.jl")
include("options.jl")
include("state.jl")
include("fst.jl")
include("passes.jl")

# styles
include("styles/default/pretty.jl")
include("styles/default/nest.jl")
include("styles/yas/pretty.jl")
include("styles/yas/nest.jl")
include("styles/documentformat/pretty.jl")
include("styles/documentformat/nest.jl")

include("print.jl")

# on Windows lines can end in "\r\n"
normalize_line_ending(s::AbstractString) = replace(s, "\r\n" => "\n")

"""
    format_text(
        text::AbstractString;
        indent::Int = 4,
        margin::Int = 92,
        style::AbstractStyle = DefaultStyle(),
        always_for_in::Bool = false,
        whitespace_typedefs::Bool = false,
        whitespace_ops_in_indices::Bool = false,
        remove_extra_newlines::Bool = false,
        import_to_using::Bool = false,
        pipe_to_function_call::Bool = false,
        short_to_long_function_def::Bool = false,
        always_use_return::Bool = false,
        whitespace_in_kwargs::Bool = true,
    )::String

Formats a Julia source passed in as a string, returning the formatted
code as another string.

## Formatting Options

### `indent`

The number of spaces used for an indentation.

### `margin`

The maximum length of a line. Code exceeding this margin will
be formatted across multiple lines.

### `always_for_in`

If true, `=` is always replaced with `in` if part of a `for` loop condition.
For example, `for i = 1:10` will be transformed to `for i in 1:10`.

### `whitespace_typedefs`

If true, whitespace is added for type definitions. Make this `true`
if you prefer `Union{A <: B, C}` to `Union{A<:B,C}`.

### `whitespace_ops_in_indices`

If true, whitespace is added for binary operations in indices. Make this
`true` if you prefer `arr[a + b]` to `arr[a+b]`. Additionally, if there's
a colon `:` involved, parenthesis will be added to the LHS and RHS.

Example: `arr[(i1 + i2):(i3 + i4)]` instead of `arr[i1+i2:i3+i4]`.

### `remove_extra_newlines`

If true, superflous newlines will be removed. For example:

```julia
a = 1



b = 2
```

is rewritten as

```julia
a = 1

b = 2
```

### `import_to_using`

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

### `pipe_to_function_call`

If true, `x |> f` is rewritten to `f(x)`.

### `short_to_long_function_def`

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

### `always_use_return`

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

"""
function format_text(
    text::AbstractString;
    indent::Int = 4,
    margin::Int = 92,
    style::AbstractStyle = DefaultStyle(),
    always_for_in::Bool = false,
    whitespace_typedefs::Bool = false,
    whitespace_ops_in_indices::Bool = false,
    remove_extra_newlines::Bool = false,
    import_to_using::Bool = false,
    pipe_to_function_call::Bool = false,
    short_to_long_function_def::Bool = false,
    always_use_return::Bool = false,
    whitespace_in_kwargs::Bool = true,
    annotate_untyped_fields_with_any::Bool = true,
)
    isempty(text) && return text

    x, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    ps.errored && error("Parsing error for input:\n\n$text")

    # no actual code
    x.args[1].kind === Tokens.NOTHING && length(x) == 1 && return text

    opts = Options(
        always_for_in = always_for_in,
        whitespace_typedefs = whitespace_typedefs,
        whitespace_ops_in_indices = whitespace_ops_in_indices,
        remove_extra_newlines = remove_extra_newlines,
        import_to_using = import_to_using,
        pipe_to_function_call = pipe_to_function_call,
        short_to_long_function_def = short_to_long_function_def,
        always_use_return = always_use_return,
        whitespace_in_kwargs = whitespace_in_kwargs,
        annotate_untyped_fields_with_any = annotate_untyped_fields_with_any,
    )
    s = State(Document(text), indent, margin, opts)
    t = pretty(style, x, s)
    hascomment(s.doc, t.endline) && (add_node!(t, InlineComment(t.endline), s))

    opts.pipe_to_function_call && pipe_to_function_call_pass!(t)

    flatten_fst!(t)
    nest!(style, t, s)

    s.line_offset = 0
    io = IOBuffer()

    # Print comments and whitespace before code.
    if t.startline > 1
        format_check(io, Notcode(1, t.startline - 1), s)
        print_leaf(io, Newline(), s)
    end

    print_tree(io, t, s)

    if t.endline < length(s.doc.range_to_line)
        print_leaf(io, Newline(), s)
        format_check(io, Notcode(t.endline + 1, length(s.doc.range_to_line)), s)
    end

    text = String(take!(io))
    text = normalize_line_ending(text)

    _, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    ps.errored && error("Parsing error for formatted text:\n\n$text")
    return text
end

"""
    format_file(
        filename::AbstractString;
        overwrite::Bool = true,
        verbose::Bool = false,
        format_options...,
    )::Bool

Formats the contents of `filename` assuming it's a Julia source file.

### File Options

If `overwrite` is `true` the file will be reformatted in place, overwriting
the existing file; if it is `false`, the formatted version of `foo.jl` will
be written to `foo_fmt.jl` instead.

If `verbose` is `true` details related to formatting the file will be printed
to `stdout`.

### Formatting Options

See [`format_text`](@ref) for description of formatting options.

### Output
 
Returns a boolean indicating whether the file was already formatted (`true`)
or not (`false`). 
"""
function format_file(
    filename::AbstractString;
    overwrite::Bool = true,
    verbose::Bool = false,
    format_options...,
)::Bool
    path, ext = splitext(filename)
    shebang_pattern = r"^#!\s*/.*\bjulia[0-9.-]*\b"
    if ext != ".jl" && match(shebang_pattern, readline(filename)) === nothing
        error("$filename must be a Julia (.jl) source file")
    end
    verbose && println("Formatting $filename")
    str = String(read(filename))
    formatted_str = format_text(str; format_options...)
    formatted_str = replace(formatted_str, r"\n*$" => "\n")
    overwrite ? write(filename, formatted_str) : write(path * "_fmt" * ext, formatted_str)
    return formatted_str == str
end

if VERSION < v"1.1.0"
    # We define `splitpath` here, copying the definition from base/path.jl
    # because it was only added in Julia 1.1.

    # TODO(odow): remove this definition of splitpath once JuliaFormatter no
    # longer supports Julia 1.0.
    _splitdir_nodrive(path::String) = _splitdir_nodrive("", path)
    function _splitdir_nodrive(a::String, b::String)
        path_dir_splitter = if Sys.isunix()
            r"^(.*?)(/+)([^/]*)$"
        elseif Sys.iswindows()
            r"^(.*?)([/\\]+)([^/\\]*)$"
        else
            error("JuliaFormatter.jl does not work on this OS.")
        end
        m = match(path_dir_splitter, b)
        m === nothing && return (a, b)
        a = string(a, isempty(m.captures[1]) ? m.captures[2][1] : m.captures[1])
        a, String(m.captures[3])
    end
    splitpath(p::AbstractString) = splitpath(String(p))
    function splitpath(p::String)
        drive, p = splitdrive(p)
        out = String[]
        isempty(p) && (pushfirst!(out, p))  # "" means the current directory.
        while !isempty(p)
            dir, base = _splitdir_nodrive(p)
            dir == p && (pushfirst!(out, dir); break)  # Reached root node.
            if !isempty(base)  # Skip trailing '/' in basename
                pushfirst!(out, base)
            end
            p = dir
        end
        if !isempty(drive)  # Tack the drive back on to the first element.
            out[1] = drive * out[1]  # Note that length(out) is always >= 1.
        end
        return out
    end
end

const CONFIG_FILE_NAME = ".JuliaFormatter.toml"

"""
    format(
        paths; # a path or collection of paths
        options...,
    )::Bool

Recursively descend into files and directories, formatting any `.jl`
files by calling `format_file` on them.

See [`format_file`](@ref) and [`format_text`](@ref) for a description of the options.

This function will look for `.JuliaFormatter.toml` in the location of the file being
formatted, and searching _up_ the file tree until a config file is (or isn't) found.
When found, the configurations in the file will overwrite the given `options`.
See ["Configuration File"](@id) for more details.

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
                formatted_path & reduce(files, init = true) do formatted_file, file
                    _, ext = splitext(file)
                    full_path = joinpath(root, file)
                    formatted_file &
                    if ext == ".jl" && !(".git" in splitpath(full_path))
                        dir = realpath(root)
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

function kwargs(dict)
    ns = (Symbol.(keys(dict))...,)
    vs = (collect(values(dict))...,)
    return pairs(NamedTuple{ns}(vs))
end

function parse_config(tomlfile)
    config_dict = parsefile(tomlfile)
    if (style = get(config_dict, "style", nothing)) !== nothing
        @assert (style == "default" || style == "yas") "currently $(CONFIG_FILE_NAME) accepts only \"default\" or \"yas\" for the style configuration"
        config_dict["style"] =
            (style == "yas" && @isdefined(YASStyle)) ? YASStyle() : DefaultStyle()
    end
    return kwargs(config_dict)
end

overwrite_options(options, config) = kwargs(merge(options, config))

end
