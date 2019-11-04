module JuliaFormatter

using CSTParser
using CSTParser.Tokenize

export format, format_text, format_file, format_dir

is_str_or_cmd(x::Tokens.Kind) =
    x in (Tokens.CMD, Tokens.TRIPLE_CMD, Tokens.STRING, Tokens.TRIPLE_STRING)

# on Windows lines can end in "\r\n"
normalize_line_ending(s::AbstractString) = replace(s, "\r\n" => "\n")

struct Document
    text::AbstractString

    ranges::Vector{UnitRange{Int}}
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
    line_to_range = Dict{Int,UnitRange{Int}}()
    lit_strings = Dict{Int,Tuple{Int,Int,String}}()
    comments = Dict{Int,Tuple{Int,String}}()
    semicolons = Set{Int}()
    format_skips = Tuple{Int,Int,String}[]
    prev_tok = Tokens.Token() # dummy initial token
    stack = Int[]
    format_on = true
    str = ""

    for t in CSTParser.Tokenize.tokenize(text)
        # @info "token" t
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

            # There should not be more than 1
            # "off" tag on the stack at a time.
            if occursin(r"^#!\s*format\s*:\s*off\s*$", t.val) && length(stack) == 0
                push!(stack, t.startpos[1])
                format_on = false
            # If "#! format: off" has not been seen
            # "#! format: on" is treated as a normal comment.
            elseif occursin(r"^#!\s*format\s*:\s*on\s*$", t.val) && length(stack) > 0
                idx1 = findfirst(c -> c == '\n', str)
                idx2 = findlast(c -> c == '\n', str)
                str = str[idx1:idx2]
                # @info "format skip str" str
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
    for (l, r) in enumerate(ranges)
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
    # @info "" lit_strings
    Document(text, ranges, line_to_range, lit_strings, comments, semicolons, format_skips)
end

mutable struct State
    doc::Document
    indent_size::Int
    indent::Int
    offset::Int
    line_offset::Int
    margin::Int
    always_for_in::Bool
    # if true will output the formatted text
    # otherwise will output the current text
    on::Bool
end
State(doc, indent_size, margin; always_for_in = false) =
    State(doc, indent_size, 0, 1, 0, margin, always_for_in, true)

@inline nspaces(s::State) = s.indent
@inline hascomment(d::Document, line::Integer) = haskey(d.comments, line)
@inline has_semicolon(d::Document, line::Integer) = line in d.semicolons

@inline function cursor_loc(s::State, offset::Integer)
    for (l, r) in enumerate(s.doc.ranges)
        if offset in r
            return (l, offset - first(r) + 1, length(r))
        end
    end
    error("Indexing range 1 - $(last(s.doc.ranges[end])), index used = $(offset)")
end
@inline cursor_loc(s::State) = cursor_loc(s, s.offset)

include("pretty.jl")
include("nest.jl")
include("print.jl")

"""
    format_text(
        text::AbstractString;
        indent::Int = 4,
        margin::Int = 92,
        always_for_in::Bool = false,
    ) :: String

Formats a Julia source passed in as a string, returning the formatted
code as another string. The formatting options are:

- `indent` which defaults to 4 spaces
- `margin` which defaults to 92 columns

If `always_for_in` is true `=` is always replaced with `in` if part of a
`for` loop condition.  For example, `for i = 1:10` will be transformed
to `for i in 1:10`.
"""
function format_text(
    text::AbstractString;
    indent::Int = 4,
    margin::Int = 92,
    always_for_in::Bool = false,
)
    isempty(text) && return text

    x, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    ps.errored && error("Parsing error for input:\n\n$text")

    # no actual code
    x.args[1].kind === Tokens.NOTHING && length(x) == 1 && return text

    d = Document(text)
    s = State(d, indent, margin, always_for_in = always_for_in)
    t = pretty(x, s)
    hascomment(s.doc, t.endline) && (add_node!(t, InlineComment(t.endline), s))
    nest!(t, s)

    s.line_offset = 0
    io = IOBuffer()

    # Print comments and whitespace before code.
    if t.startline > 1
        print_tree(io, Notcode(1, t.startline - 1), s)
        print_tree(io, Newline(), s)
    end

    if t.endline < length(s.doc.ranges)
        add_node!(t, Newline(), s)
        add_node!(t, Notcode(t.endline + 1, length(s.doc.ranges)), s)
    end

    print_tree(io, t, s)

    text = String(take!(io))
    text = normalize_line_ending(text)

    _, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    ps.errored && error("Parsing error for formatted text:\n\n$text")
    return text
end

"""
    format_file(
        filename::AbstractString;
        indent::Integer = 4,
        margin::Integer = 92,
        overwrite::Bool = true,
        verbose::Bool = false,
        always_for_in::Bool = false,
    )

Formats the contents of `filename` assuming it's a Julia source file.

The formatting options are:

- `indent` which defaults to 4 spaces
- `margin` which defaults to 92 columns

If `overwrite` is `true` the file will be reformatted in place, overwriting
the existing file; if it is `false`, the formatted version of `foo.jl` will
be written to `foo_fmt.jl` instead.

If `verbose` is `true` details related to formatting the file will be printed
to `stdout`.

If `always_for_in` is true `=` is always replaced with `in` if part of a
`for` loop condition.  For example, `for i = 1:10` will be transformed
to `for i in 1:10`.
"""
function format_file(
    filename::AbstractString;
    indent::Integer = 4,
    margin::Integer = 92,
    overwrite::Bool = true,
    verbose::Bool = false,
    always_for_in::Bool = false,
)
    path, ext = splitext(filename)
    if ext != ".jl"
        error("$filename must be a Julia (.jl) source file")
    end
    verbose && println("Formatting $filename with indent = $indent, margin = $margin")
    str = String(read(filename))
    str = format_text(str, indent = indent, margin = margin, always_for_in = always_for_in)
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

"""
    format(
        paths; # a path or collection of paths
        indent::Integer = 4,
        margin::Integer = 92,
        overwrite::Bool = true,
        verbose::Bool = false,
        always_for_in::Bool = false,
    )

Recursively descend into files and directories, formatting and `.jl`
files by calling `format_file` on them.
"""
function format(paths; options...)
    for path in paths
        if isfile(path)
            format_file(path; options...)
        else
            for (root, dirs, files) in walkdir(path)
                for file in files
                    _, ext = splitext(file)
                    ext == ".jl" || continue
                    full_path = joinpath(root, file)
                    ".git" in splitpath(full_path) && continue
                    format_file(full_path; options...)
                end
            end
        end
    end
    nothing
end
format(path::AbstractString; options...) = format((path,); options...)

end
