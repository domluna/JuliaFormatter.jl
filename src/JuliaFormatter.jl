module JuliaFormatter

using CSTParser
using Tokenize
using DataStructures
using Pkg.TOML: parsefile
using Markdown: Markdown, MD, Code, plain
using Documenter.DocTests: repl_splitter
import Markdown

export format, format_text, format_file, DefaultStyle, YASStyle

is_str_or_cmd(t::Tokens.Kind) =
    t in (Tokens.CMD, Tokens.TRIPLE_CMD, Tokens.STRING, Tokens.TRIPLE_STRING)
is_str_or_cmd(typ::CSTParser.Head) =
    typ in (CSTParser.StringH, CSTParser.x_Str, CSTParser.x_Cmd)

# on Windows lines can end in "\r\n"
normalize_line_ending(s::AbstractString) = replace(s, "\r\n" => "\n")

# Implement Interval Tree using DataStructures's SortedDict
struct IntervalTreeOrder <: DataStructures.Ordering end
DataStructures.lt(::IntervalTreeOrder, a::UnitRange{Int}, b::UnitRange{Int}) =
    last(a) < first(b)
DataStructures.eq(::IntervalTreeOrder, a::UnitRange{Int}, b::UnitRange{Int}) =
    isequal(a, b) || first(a) in b || first(b) in a

struct Document
    text::AbstractString

    range_to_line::SortedDict{UnitRange{Int},Int,IntervalTreeOrder}
    line_to_range::Dict{Int,UnitRange{Int}}

    # mapping the offset in the file to the raw literal
    # string and what lines it starts and ends at.
    lit_strings::Dict{Int,Tuple{Int,Int,String}}
    comments::Dict{Int,Tuple{Int,String}}

    # CSTParser does not detect semicolons.
    # It's useful to know where these are for
    # a few node types.
    semicolons::Set{Int}

    # List of tuples where a tuple contains
    # the start and end lines of regions in the
    # file formatting should be skipped.
    format_skips::Vector{Tuple{Int,Int,String}}
end

function Document(text::AbstractString)
    ranges = UnitRange{Int}[]
    lit_strings = Dict{Int,Tuple{Int,Int,String}}()
    comments = Dict{Int,Tuple{Int,String}}()
    semicolons = Set{Int}()
    format_skips = Tuple{Int,Int,String}[]
    prev_tok = Tokens.Token() # dummy initial token
    stack = Int[]
    format_on = true
    str = ""

    for t in CSTParser.Tokenize.tokenize(text)
        if t.kind === Tokens.WHITESPACE
            offset = t.startbyte
            for c in t.val
                if c == '\n'
                    s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                    push!(ranges, s:offset+1)
                end
                offset += 1
            end
        elseif t.kind === Tokens.ENDMARKER
            s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
            push!(ranges, s:t.startbyte)
        elseif is_str_or_cmd(t.kind)
            lit_strings[t.startbyte] = (t.startpos[1], t.endpos[1], t.val)
            if t.startpos[1] != t.endpos[1]
                offset = t.startbyte
                nls = findall(x -> x == '\n', t.val)
                for nl in nls
                    s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                    push!(ranges, s:offset+nl)
                end
            end
        elseif t.kind === Tokens.COMMENT
            ws = 0
            if prev_tok.kind === Tokens.WHITESPACE
                # Handles the case where the value of the
                # WHITESPACE token is like " \n ".
                i = findlast(c -> c == '\n', prev_tok.val)
                i === nothing && (i = 1)
                ws = count(c -> c == ' ', prev_tok.val[i:end])
            end

            if t.startpos[1] == t.endpos[1]
                # Determine the number of spaces prior to a possible inline comment
                comments[t.startpos[1]] = (ws, t.val)
            else
                # multiline comment of the form
                # #=
                #
                # #=

                line = t.startpos[1]
                offset = t.startbyte
                cs = ""
                for (i, c) in enumerate(t.val)
                    cs *= c
                    if c == '\n'
                        s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                        push!(ranges, s:offset+1)
                        fc = findfirst(c -> !isspace(c), cs)
                        idx = fc === nothing ? 1 : min(fc, ws + 1)
                        comments[line] = (ws, cs[idx:end])
                        line += 1
                        cs = ""
                    end
                    offset += 1
                end
                # last comment
                idx = min(findfirst(c -> !isspace(c), cs), ws + 1)
                comments[line] = (ws, cs[idx:end])
            end

            if occursin(r"^#!\s*format\s*:\s*off\s*$", t.val) && length(stack) == 0
                # There should not be more than 1
                # "off" tag on the stack at a time.
                push!(stack, t.startpos[1])
                format_on = false
            elseif occursin(r"^#!\s*format\s*:\s*on\s*$", t.val) && length(stack) > 0
                # If "#! format: off" has not been seen
                # "#! format: on" is treated as a normal comment.
                idx1 = findfirst(c -> c == '\n', str)
                idx2 = findlast(c -> c == '\n', str)
                str = str[idx1:idx2]
                push!(format_skips, (pop!(stack), t.startpos[1], str))
                str = ""
                format_on = true
            end
        elseif t.kind === Tokens.SEMICOLON
            push!(semicolons, t.startpos[1])
        end
        prev_tok = t

        if !format_on
            str *= Tokenize.untokenize(t)
        end
    end

    range_to_line = SortedDict{UnitRange{Int},Int}(IntervalTreeOrder())
    line_to_range = Dict{Int,UnitRange{Int}}()
    for (l, r) in enumerate(ranges)
        insert!(range_to_line, r, l)
        line_to_range[l] = r
    end

    # If there is a SINGLE "#! format: off" tag
    # do not format from the "off" tag onwards.
    if length(stack) == 1 && length(format_skips) == 0
        # -1 signifies everything afterwards "#! format: off"
        # will not formatted.
        idx1 = findfirst(c -> c == '\n', str)
        str = str[idx1:end]
        push!(format_skips, (stack[1], -1, str))
    end
    Document(
        text,
        range_to_line,
        line_to_range,
        lit_strings,
        comments,
        semicolons,
        format_skips,
    )
end

Base.@kwdef struct Options
    always_for_in::Bool = false
    whitespace_typedefs::Bool = false
    whitespace_ops_in_indices::Bool = false
    remove_extra_newlines::Bool = false
    import_to_using::Bool = false
    pipe_to_function_call::Bool = false
    short_to_long_function_def::Bool = false
    always_use_return::Bool = false
end

mutable struct State
    doc::Document
    indent_size::Int
    indent::Int
    offset::Int
    line_offset::Int
    margin::Int

    # If true, output is formatted text otherwise
    # it's source text
    on::Bool
    opts::Options
end
State(doc, indent_size, margin, opts) = State(doc, indent_size, 0, 1, 0, margin, true, opts)

@inline nspaces(s::State) = s.indent
@inline hascomment(d::Document, line::Integer) = haskey(d.comments, line)
@inline has_semicolon(d::Document, line::Integer) = line in d.semicolons

@inline function cursor_loc(s::State, offset::Integer)
    l = s.doc.range_to_line[offset:offset]
    r = s.doc.line_to_range[l]
    return (l, offset - first(r) + 1, length(r))
end
@inline cursor_loc(s::State) = cursor_loc(s, s.offset)

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

include("fst.jl")
include("passes.jl")
include("pretty.jl")
include("nest.jl")
include("print.jl")
include("styles/yas.jl")

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

If true `=` is always replaced with `in` if part of a `for` loop condition.
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

### `import_to_using`

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

### `pipe_to_function_call`

If true `x |> f` is rewritten to `f(x)`.

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
    )
    s = State(Document(text), indent, margin, opts)
    _format_text(style, s, x)
end

function format_text(style, state, text)
    x, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    ps.errored && error("Parsing error for input:\n\n$text")
    s = State(Document(text), state.indent, state.margin, state.opts)
    _format_text(style, s, x)
end

function _format_text(style, s, x)
    opts = s.opts
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
    )

Formats the contents of `filename` assuming it's a Julia source file.

### File Options

If `overwrite` is `true` the file will be reformatted in place, overwriting
the existing file; if it is `false`, the formatted version of `foo.jl` will
be written to `foo_fmt.jl` instead.

If `verbose` is `true` details related to formatting the file will be printed
to `stdout`.

### Formatting Options

See [`format_text`](@ref) for description of formatting options.
"""
function format_file(
    filename::AbstractString;
    overwrite::Bool = true,
    verbose::Bool = false,
    format_options...,
)
    path, ext = splitext(filename)
    shebang_pattern = r"^#!\s*/.*\bjulia[0-9.-]*\b"
    if ext != ".jl" && match(shebang_pattern, readline(filename)) === nothing
        error("$filename must be a Julia (.jl) source file")
    end
    verbose && println("Formatting $filename")
    str = String(read(filename))
    str = format_text(str; format_options...)
    str = replace(str, r"\n*$" => "\n")
    overwrite ? write(filename, str) : write(path * "_fmt" * ext, str)
    nothing
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
    )

Recursively descend into files and directories, formatting any `.jl`
files by calling `format_file` on them.

See [`format_file`](@ref) and [`format_text`](@ref) for a description of the options.

This function will look for `.JuliaFormatter.toml` in the location of the file being
formatted, and searching _up_ the file tree until a config file is (or isn't) found.
When found, the configurations in the file will overwrite the given `options`.
See ["Configuration File"](@id) for more details.
"""
function format(paths; options...)
    dir2config = Dict{String,Any}()
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
        if isfile(path)
            dir = dirname(realpath(path))
            opts = if (config = find_config_file(dir)) !== nothing
                overwrite_options(options, config)
            else
                options
            end
            format_file(path; opts...)
        else
            for (root, dirs, files) in walkdir(path)
                for file in files
                    _, ext = splitext(file)
                    ext == ".jl" || continue
                    full_path = joinpath(root, file)
                    ".git" in splitpath(full_path) && continue
                    dir = relpath(root)
                    opts = if (config = find_config_file(dir)) !== nothing
                        overwrite_options(options, config)
                    else
                        options
                    end
                    format_file(full_path; opts...)
                end
            end
        end
    end
    nothing
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
