module JLFmt

# One pass requires use of .span in order to check lengths.
#
# PROBLEM: The number of characters in .span doesn't equal a pretty printed version of the CST.
# Ex: (a,b) should be 6 since the pretty printed version is (a, b) but span will return 5
# 
# If A comes before B, then if A has to be indented B will also need to be indented.
# But we could indent B without indenting A. Assuming A and B don't need to be joined:
#
#   E1 where E2
#
# A is "E1 where "
# B is "E2"
#
# Anyway ... here's how this will go.
#
# 1) Prettify the CST, basically this is
# a CST but with prettified text instead of a CST node.
# 2) Nest (width-sensitive) pass
# 3) Comment pass
# 4) Print pass
# 5) Profit (jk no money in OSS)
#

using CSTParser
import CSTParser.Tokenize.Tokens

export format

function newline_ranges(text::String)
    ranges = UnitRange{Int}[]
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
            #= push!(ranges, 1:t.startbyte+1) =#
            s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
            push!(ranges, s:t.startbyte)
        elseif (t.kind == Tokens.TRIPLE_STRING || t.kind == Tokens.STRING) && t.startpos[1] != t.endpos[1]
            offset = t.startbyte
            nls = findall(x -> x == '\n', t.val)
            for nl in nls
                s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                push!(ranges, s:offset+nl)
            end
        end
    end
    ranges
end

struct Document
    text::String
    ranges::Vector{UnitRange{Int}}
end
Document(s::String) = Document(s, newline_ranges(s))


mutable struct State
    doc::Document
    indent_width::Int
    indents::Int
    offset::Int
    line_offset::Int
    max_width::Int
end

@inline nspaces(s::State) = s.indent_width * s.indents

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

function format(text::String; indent_width=4, max_width=80)
    d = Document(text)
    s = State(d, indent_width, 0, 1, 0, max_width)
    x = CSTParser.parse(text, true)
    tree = pretty(x, s)
    #= @info "" tree =#

    nest!(tree, s)
    @info "" tree

    io = IOBuffer()
    print_tree(io, tree, s)
    return String(take!(io))
end

end # module
