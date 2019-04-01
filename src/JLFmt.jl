module JLFmt

using CSTParser
import CSTParser.Tokenize.Tokens

export format

function file_line_ranges(text::String)
    ranges = UnitRange{Int}[]
    lit_strings = LitString[]
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
        elseif (t.kind == Tokens.TRIPLE_STRING || t.kind == Tokens.STRING) && t.startpos[1] != t.endpos[1]
            offset = t.startbyte
            nls = findall(x -> x == '\n', t.val)
            for nl in nls
                s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                push!(ranges, s:offset+nl)
            end
        elseif t.kind == Tokens.COMMENT
            @info "comment token" t
        end
        if (t.kind == Tokens.TRIPLE_STRING || t.kind == Tokens.STRING)
        end
    end
    ranges
end

"""
Raw literal string representation along
with the location in the file.

`CSTParser` unescapes strings to mimic behaviour
of `Meta.parse`. This is problematic for formatting
strings. 

When a `CSTParser.LITERAL` that is of kind `Tokens.STRING`
or `Tokens.TRIPLE_STRING` is encountered the value repsented
in the original token will be used instead of the val in the
`CSTParser` node.
"""
struct LitString
    startline::Int
    endline::Int
    val::String
end

struct Document
    text::String
    ranges::Vector{UnitRange{Int}}
    #= lit_strings::Vector{LitString} =#
    #= inline_commments::Vector{LitString} =#
end
Document(s::String) = Document(s, file_line_ranges(s))

mutable struct State
    doc::Document
    indent_size::Int
    indents::Int
    offset::Int
    line_offset::Int
    max_line_length::Int
end

@inline nspaces(s::State) = s.indent_size * s.indents

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

function format(text::String; indent_size=4, max_line_length=80)
    if isempty(text)
        return text
    end
    d = Document(text)
    s = State(d, indent_size, 0, 1, 0, max_line_length)
    x = CSTParser.parse(text, true)
    t = pretty(x, s)
    nest!(t, s)

    io = IOBuffer()
    # Print comments and whitespace before any code.
    if t.startline > 1
        print_tree(io, NotCode(1, t.startline-1, 0), s)
    end
    print_tree(io, t, s)
    # Print comments and whitespace after any code.
    if t.endline < length(s.doc.ranges)
        print_tree(io, newline, s)
        print_tree(io, NotCode(t.endline+1, length(s.doc.ranges), 0), s)
    end

    String(take!(io))
end

end # module
