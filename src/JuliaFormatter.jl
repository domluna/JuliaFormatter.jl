module JuliaFormatter

if isdefined(Base, :Experimental) && isdefined(Base.Experimental, Symbol("@max_methods"))
    @eval Base.Experimental.@max_methods 1
end

using CSTParser
using Tokenize
using DataStructures
using Pkg.TOML: parsefile
using Glob
import CommonMark: block_modifier
import Base: get, pairs
using Combinatorics: permutations
using CommonMark:
    AdmonitionRule,
    CodeBlock,
    enable!,
    FootnoteRule,
    markdown,
    MathRule,
    Parser,
    Rule,
    TableRule,
    FrontMatterRule

export format,
    format_text,
    format_file,
    format_md,
    DefaultStyle,
    YASStyle,
    BlueStyle,
    SciMLStyle,
    MinimalStyle

struct Configuration
    args::Dict{String,Any}
    file::Dict{String,Any}
end
Configuration() = Configuration(Dict{String,Any}(), Dict{String,Any}())
Configuration(args) = Configuration(args, Dict{String,Any}())
function get(config::Configuration, s::String, default)
    haskey(config.args, s) && return config.args[s]
    haskey(config.file, s) && return config.file[s]
    return default
end
function pairs(conf::Configuration)
    pairs(merge(conf.file, conf.args))
end

abstract type AbstractStyle end

@inline options(s::AbstractStyle) = NamedTuple()

struct NoopStyle <: AbstractStyle end

"""
    DefaultStyle

The default formatting style. See the [Style](@ref) section of the documentation
for more details.

See also: [`BlueStyle`](@ref), [`YASStyle`](@ref), [`SciMLStyle`](@ref), [`MinimalStyle`](@ref)
"""
struct DefaultStyle <: AbstractStyle
    innerstyle::AbstractStyle
end

DefaultStyle() = DefaultStyle(NoopStyle())

function getstyle(s::AbstractStyle)::AbstractStyle
    s.innerstyle isa NoopStyle ? s : s.innerstyle
end
function getstyle(s::NoopStyle)
    return s
end

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
        long_to_short_function_def = false,
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
        join_lines_based_on_source = false,
        trailing_comma = true,
        trailing_zero = true,
        indent_submodule = false,
        separate_kwargs_with_semicolon = false,
        surround_whereop_typeparameters = true,
        variable_call_indent = [],
        short_circuit_to_if = false,
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

# multidimensional array syntax has nodes that appear as
# Symbol("integer_value"), i.e. Symbol("2") for ";;"
const SEMICOLON_LOOKUP = Dict(Symbol(n) => n for n in 1:20)

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
include("styles/sciml/pretty.jl")
include("styles/sciml/nest.jl")
include("styles/minimal/pretty.jl")

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
        for_in_replacement::String = "in",
        whitespace_typedefs::Bool = false,
        whitespace_ops_in_indices::Bool = false,
        remove_extra_newlines::Bool = false,
        import_to_using::Bool = false,
        pipe_to_function_call::Bool = false,
        short_to_long_function_def::Bool = false,
        long_to_short_function_def::Bool = false,
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
        trailing_zero::Bool = true,
        indent_submodule::Bool = false,
        separate_kwargs_with_semicolon::Bool = false,
        surround_whereop_typeparameters::Bool = true,
        variable_call_indent::Vector{String} = []
        short_circuit_to_if::Bool = false,
    )::String

Formats a Julia source passed in as a string, returning the formatted
code as another string.

See https://domluna.github.io/JuliaFormatter.jl/dev/#Formatting-Options for details on available options.
"""
function format_text(text::AbstractString; style::AbstractStyle = DefaultStyle(), kwargs...)
    return format_text(text, style; kwargs...)
end

function format_text(text::AbstractString, style::AbstractStyle; kwargs...)
    isempty(text) && return text
    opts = Options(; merge(options(style), kwargs)...)
    return format_text(text, style, opts)
end

function format_text(text::AbstractString, style::SciMLStyle; maxiters = 3, kwargs...)
    isempty(text) && return text
    opts = Options(; merge(options(style), kwargs)...)
    # We need to iterate to a fixpoint because the result of short to long
    # form isn't properly formatted
    formatted_text = format_text(text, style, opts)
    return formatted_text
    iter = 1
    while formatted_text != text
        iter > maxiters &&
            error("formatted_text hasn't reached to a fixpoint in $iter iterations")
        text = formatted_text
        formatted_text = format_text(text, style, opts)
        iter += 1
    end
    return formatted_text
end

function format_text(text::AbstractString, style::AbstractStyle, opts::Options)
    if opts.always_for_in == true
        @assert valid_for_in_op(opts.for_in_replacement) "`for_in_replacement` is set to an invalid operator \"$(opts.for_in_replacement)\", valid operators are $(VALID_FOR_IN_OPERATORS). Change it to one of the valid operators and then reformat."
    end
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
        @warn "Error occurred during prettification" line = loc[1] offset = loc[2]
        rethrow()
    end
    hascomment(s.doc, fst.endline) && (add_node!(fst, InlineComment(fst.endline), s))

    s.opts.pipe_to_function_call && pipe_to_function_call_pass!(fst)

    flatten_fst!(fst)

    if s.opts.short_circuit_to_if
        short_circuit_to_if_pass!(fst, s)
    end

    needs_alignment(s.opts) && align_fst!(fst, s.opts)

    nest!(style, fst, s)

    # ignore maximum width can be extra whitespace at the end of lines
    # remove it all before we print.
    s.opts.join_lines_based_on_source && remove_superfluous_whitespace!(fst)

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
    # TODO: This line info is not correct since it doesn't stop at the error.
    # At the moment it doesn't seem there's a way to get the error line info.
    # https://github.com/julia-vscode/CSTParser.jl/issues/335
    line, offset = ps.lt.startpos
    if ps.errored
        buf = IOBuffer()
        lines = split(text, '\n')
        padding = ndigits(length(lines))
        for (i, l) in enumerate(lines)
            write(buf, "$i", repeat(" ", (padding - ndigits(i) + 1)), l, "\n")
            if i == line
                write(buf, "\n...")
                break
            end
        end
        error_text = String(take!(buf))
        error(
            "Error while PARSING formatted text:\n\n$error_text\n\nError occurred on line $line, offset $offset of formatted text.\n\nThe error might not be precisely on this line but it should be in the region of the code block. Try commenting the region out and see if that removes the error.",
        )
    end
    return text
end

function _format_file(
    filename::AbstractString;
    overwrite::Bool = true,
    verbose::Bool = false,
    format_markdown::Bool = false,
    format_options...,
)::Bool
    path, ext = splitext(filename)
    shebang_pattern = r"^#!\s*/.*\bjulia[0-9.-]*\b"
    formatted_str = if match(r"^\.[jq]*md$", ext) ≠ nothing
        format_markdown || return true
        verbose && println("Formatting $filename")
        str = String(read(filename))
        format_md(str; format_options...)
    elseif ext == ".jl" || match(shebang_pattern, readline(filename)) !== nothing
        verbose && println("Formatting $filename")
        str = String(read(filename))
        format_text(str; format_options...)
    else
        error("$filename must be a Julia (.jl) or Markdown (.md, .jmd or .qmd) source file")
    end
    formatted_str = replace(formatted_str, r"\n*$" => "\n")
    already_formatted = (formatted_str == str)
    if overwrite && !already_formatted
        write(filename, formatted_str)
    end
    return already_formatted
end

function _format_file(filename::AbstractString, style::AbstractStyle; kwargs...)
    return _format_file(filename; style = style, kwargs...)
end

const CONFIG_FILE_NAME = ".JuliaFormatter.toml"

"""
    format_file(
        filename::AbstractString;
        overwrite::Bool = true,
        verbose::Bool = false,
        format_markdown::Bool = false,
        format_options...,
    )::Bool

Formats the contents of `filename` assuming it's a `.jl`, `.md`, `.jmd` or `.qmd` file.

See https://domluna.github.io/JuliaFormatter.jl/dev/#File-Options for details on available options.

## Output

Returns a boolean indicating whether the file was already formatted (`true`) or not (`false`).
"""
function format_file(
    filename::AbstractString;
    overwrite::Bool = true,
    verbose::Bool = false,
    format_markdown::Bool = false,
    format_options...,
)
    format(
        filename;
        overwrite = overwrite,
        verbose = verbose,
        format_markdown = format_markdown,
        format_options...,
    )
end

function format_file(filename::AbstractString, style::AbstractStyle; kwargs...)
    return format_file(filename; style = style, kwargs...)
end

"""
    format(
        paths; # a path or collection of paths
        options...,
    )::Bool

Recursively descend into files and directories, formatting any `.jl` files.

See [`format_file`](@ref) and [`format_text`](@ref) for a description of the options.

This function will look for `.JuliaFormatter.toml` in the location of the file being
formatted, and searching *up* the file tree until a config file is (or isn't) found.
When found, the configurations in the file will overwrite the given `options`.
See [Configuration File](@ref) for more details.

### Output

Returns a boolean indicating whether the file was already formatted (`true`)
or not (`false`).
"""
format(paths; options...) =
    format(paths, Configuration(Dict{String,Any}(String(k) => v for (k, v) in options)))
function format(paths, options::Configuration)::Bool
    already_formatted = true
    # Don't parallelize this, since there could be a race condition on a
    # subdirectory that is included in both paths
    for path in paths
        already_formatted &= format(path, options)
    end
    return already_formatted
end

format(path::AbstractString; options...) =
    format(path, Configuration(Dict{String,Any}(String(k) => v for (k, v) in options)))
function format(path::AbstractString, options::Configuration)
    path = realpath(path)
    if !get(options, "config_applied", false)
        # Run recursive search upward *once*
        options =
            Configuration(copy(options.args), Dict{String,Any}(find_config_file(path)))
        options.args["config_applied"] = true
    end
    isignored(path, options) && return true
    if isdir(path)
        configpath = joinpath(path, CONFIG_FILE_NAME)
        if isfile(configpath)
            options = Configuration(copy(options.args), parse_config(configpath))
        end
        formatted = Threads.Atomic{Bool}(true)
        Threads.@threads for subpath in readdir(path)
            subpath = joinpath(path, subpath)
            is_formatted = format(subpath, options)
            Threads.atomic_and!(formatted, is_formatted)
        end
        return formatted.value
    end
    # `path` is not a directory but a file
    _, ext = splitext(path)
    if !in(ext, (".jl", ".md", ".jmd", ".qmd"))
        return true
    end
    try
        return _format_file(path; [Symbol(k) => v for (k, v) in pairs(options)]...)
    catch err
        @info "Error in formatting file $path"
        @debug "formatting failed due to" exception = (err, catch_backtrace())
        return _format_file(path; [Symbol(k) => v for (k, v) in pairs(options)]...)
    end
end

"""
    format(path, style::AbstractStyle; options...)::Bool
"""
format(path, style::AbstractStyle; options...) = format(path; style = style, options...)

"""
    format(mod::Module, args...; options...)
"""
function format(mod::Module, args...; options...)
    path = pkgdir(mod)
    path === nothing && throw(ArgumentError("couldn't find a directory of module `$mod`"))
    format(path, args...; options...)
end

function kwargs(dict)
    ns = (Symbol.(keys(dict))...,)
    vs = (collect(values(dict))...,)
    return pairs(NamedTuple{ns}(vs))
end

fieldnts(T::Type) = ((fieldname(T, i), fieldtype(T, i)) for i in 1:fieldcount(T))

function parse_config(tomlfile)
    config_dict = parsefile(tomlfile)
    for (field, type) in fieldnts(Options)
        if type == Union{Bool,Nothing}
            field = string(field)
            if get(config_dict, field, "") == "nothing"
                config_dict[field] = nothing
            end
        end
    end
    if (style = get(config_dict, "style", nothing)) !== nothing
        @assert (
            style == "default" ||
            style == "yas" ||
            style == "blue" ||
            style == "sciml" ||
            style == "minimal"
        ) "currently $(CONFIG_FILE_NAME) accepts only \"default\" or \"yas\", \"blue\", \"sciml\", or \"minimal\" for the style configuration"
        config_dict["style"] = if (style == "yas" && @isdefined(YASStyle))
            YASStyle()
        elseif (style == "blue" && @isdefined(BlueStyle))
            BlueStyle()
        elseif (style == "sciml" && @isdefined(SciMLStyle))
            SciMLStyle()
        elseif (style == "minimal" && @isdefined(MinimalStyle))
            MinimalStyle()
        else
            DefaultStyle()
        end
    end
    return config_dict
end

function find_config_file(path)
    dir = dirname(path)
    (path == dir || isempty(path)) && return NamedTuple()
    configpath = joinpath(dir, CONFIG_FILE_NAME)
    isfile(configpath) && return parse_config(configpath)
    return find_config_file(dir)
end

function isignored(path, options)
    ignore = get(options, "ignore", String[])
    return any(x -> occursin(Glob.FilenameMatch("*$x"), path), ignore)
end

if Base.VERSION >= v"1.5"
    include("other/precompile.jl")
end

end
