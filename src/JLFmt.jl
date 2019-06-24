module JLFmt

using CSTParser
import CSTParser.Tokenize.Tokens

export format, format_file

is_str_or_cmd(x::Tokens.Kind) = x in (Tokens.CMD, Tokens.TRIPLE_CMD, Tokens.STRING, Tokens.TRIPLE_STRING)

function file_line_ranges(text::AbstractString)
    ranges = UnitRange{Int}[]
    lit_strings = Dict{Int, Tuple{Int,Int,String}}()
    comments = Dict{Int,String}()
    for t in CSTParser.Tokenize.tokenize(text)
        if t.kind == Tokens.WHITESPACE
            offset = t.startbyte
            for c in t.val
                if c == '\n'
                    s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                    push!(ranges, s:offset+1)
                end
                offset += 1
            end
        elseif t.kind == Tokens.ENDMARKER
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
        elseif t.kind == Tokens.COMMENT
            comments[t.startpos[1]] = t.val
        end
    end
    # @info "" lit_strings
    ranges, lit_strings, comments
end

struct Document
    text::AbstractString
    ranges::Vector{UnitRange{Int}}
    # mapping the offset in the file to the raw literal
    # string and what lines it starts and ends at.
    lit_strings::Dict{Int, Tuple{Int, Int, String}}
    comments::Dict{Int, String}
end
Document(s::AbstractString) = Document(s, file_line_ranges(s)...)

mutable struct State
    doc::Document
    indent_size::Int
    indent::Int
    offset::Int
    line_offset::Int
    print_width::Int
end

@inline nspaces(s::State) = s.indent

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
# include("nest.jl")
include("print.jl")

"""
    format(text::AbstractString; indent_size=4, print_width=80)

Formats a Julia source file (.jl).

`indent_size` - The number of spaces used for an indentation.

`print_width` - The maximum number of characters of code on a single line. Lines 
over the width will be nested if possible.
"""
function format(text::AbstractString, indent_size, print_width)
    if isempty(text)
        return text
    end
    d = Document(text)
    s = State(d, indent_size, 0, 1, 0, print_width)
    x = CSTParser.parse(text, true)
    t = pretty(x, s)
    # nest!(t, s)
    #
    # @info "" t

    io = IOBuffer()
    # Print comments and whitespace before code.
    # if t.startline > 1
    #     print_tree(io, Notcode(1, t.startline-1, 0), s)
    #     print_tree(io, Newline(), s)
    # end
    print_tree(io, t, s)
    # Print comments and whitespace after code.
    # if t.endline < length(s.doc.ranges)
    #     print_tree(io, Newline(), s)
    #     print_tree(io, Notcode(t.endline+1, length(s.doc.ranges), 0), s)
    # end

    String(take!(io))
end

"""
    format_file(filename, indent_size, print_width; overwrite=false)

Formats the contents of `filename` assuming it's a Julia source file.

If `overwrite` is `false` the formatted output will be written to the a file with
a "fmt" suffix. For example, if the `filename` is "foo.jl", the output will
be written to "foo_fmt.jl".

If `overwrite` is `true` the file will be overwritten with the formatted output.
"""
function format_file(filename::AbstractString, indent_size, print_width; overwrite=false)
    path, ext = splitext(filename)
    if ext != ".jl"
        throw(ArgumentError("$filename must be a Julia (.jl) source file"))
    end
    str = read(filename) |> String
    try
        str = format(str, indent_size, print_width)
    catch e
        @info "" e
        throw(ErrorException("""An error occured formatting $filename. :-(

                             Please file an issue at https://github.com/domluna/JLFmt.jl/issues
                             with a link to a gist containing the contents of the file. A gist 
                             can be created at https://gist.github.com/."""))
    end

    if overwrite
        write(filename, str)
    else
        write(path * "_fmt" * ext, str)
    end
end

end
