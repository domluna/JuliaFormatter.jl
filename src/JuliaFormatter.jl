module JuliaFormatter

using CSTParser
import CSTParser.Tokenize.Tokens

export format, format_file, format_dir

is_str_or_cmd(x::Tokens.Kind) =
    x in (Tokens.CMD, Tokens.TRIPLE_CMD, Tokens.STRING, Tokens.TRIPLE_STRING)

struct Document
    text::AbstractString

    ranges::Vector{UnitRange{Int}}
    line_to_range::Dict{Int,UnitRange{Int}}

    # mapping the offset in the file to the raw literal
    # string and what lines it starts and ends at.
    lit_strings::Dict{Int,Tuple{Int,Int,String}}
    comments::Dict{Int,String}

    # CSTParser does not detect semicolons.
    # It's useful to know where these are for
    # a few node types.
    semicolons::Set{Int}
end
function Document(text::AbstractString)
    ranges = UnitRange{Int}[]
    line_to_range = Dict{Int,UnitRange{Int}}()
    lit_strings = Dict{Int,Tuple{Int,Int,String}}()
    comments = Dict{Int,String}()
    semicolons = Set{Int}()

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
            if t.startpos[1] == t.endpos[1]
                comments[t.startpos[1]] = t.val
            else
                sl = t.startpos[1]
                offset = t.startbyte
                comment_offset = 1
                for (i, c) in enumerate(t.val)
                    if c == '\n'
                        s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                        push!(ranges, s:offset+1)
                        comments[sl] = t.val[comment_offset:i-1]
                        sl += 1
                        comment_offset = i + 1
                    end
                    offset += 1
                end
                # last comment
                comments[sl] = t.val[comment_offset:end]
            end
        elseif t.kind === Tokens.SEMICOLON
            push!(semicolons, t.startpos[1])
        end
    end
    for (l, r) in enumerate(ranges)
        line_to_range[l] = r
    end
    # @debug "" lit_strings comments
    Document(text, ranges, line_to_range, lit_strings, comments, semicolons)
end

mutable struct State
    doc::Document
    indent_size::Int
    indent::Int
    offset::Int
    line_offset::Int
    margin::Int
end
State(doc, indent_size, margin) = State(doc, indent_size, 0, 1, 0, margin)

@inline nspaces(s::State) = s.indent
@inline getline(d::Document, line::Int) = d.text[d.line_to_range[line]]
@inline hascomment(d::Document, line::Int) = haskey(d.comments, line)
@inline has_semicolon(d::Document, line::Int) = line in d.semicolons

@inline function cursor_loc(s::State, offset::Int)
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
        indent::Integer = 4,
        margin::Integer = 92,
    ) :: String

Formats a Julia source passed in as a string, returning the formatted
code as another string. The formatting options are:

- `indent` which defaults to 4 spaces
- `margin` which defaults to 92 columns

"""
function format_text(text::AbstractString; indent::Integer = 4, margin::Integer = 92,)
    if isempty(text)
        return text
    end

    x, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    ps.errored && error("Parsing error for input $text")

    d = Document(text)
    # If "nofmt" occurs in a comment on line 1 do not format
    occursin("nofmt", get(d.comments, 1, "")) && (return text)

    s = State(d, indent, margin)
    t = pretty(x, s)
    hascomment(s.doc, t.endline) && (add_node!(t, InlineComment(t.endline), s))
    nest!(t, s)

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

    _, ps = CSTParser.parse(CSTParser.ParseState(text), true)
    ps.errored && error("Parsing error for formatted $text")
    return text
end

"""
    format_file(
        filename::AbstractString;
        indent::Integer = 4,
        margin::Integer = 92,
        overwrite::Bool = true,
        verbose::Bool = false,
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
"""
function format_file(
    filename::AbstractString;
    indent::Integer = 4,
    margin::Integer = 92,
    overwrite::Bool = true,
    verbose::Bool = false,
)
    path, ext = splitext(filename)
    if ext != ".jl"
        error("$filename must be a Julia (.jl) source file")
    end
    verbose && println("Formatting $filename with indent = $indent, margin = $margin")
    str = read(filename) |> String
    str = format_text(str, indent = indent, margin = margin)
    overwrite ? write(filename, str) : write(path * "_fmt" * ext, str)
    nothing
end

"""
    format(
        paths; # a path or collection of paths
        indent::Integer = 4,
        margin::Integer = 92,
        overwrite::Bool = true,
        verbose::Bool = false,
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
