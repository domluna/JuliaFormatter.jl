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
# 2) Indent pass
# 3) Comment pass
# 3) Profit (jk no money in OSS)
#
# ## Prettify pass
#
# At this stage just output the prettified version of the CST node. Don't worry about proper
# indentation/nesting.
#
# ## Indent pass
#
# Indent based on indent width multiplier and desired maximum width.
#
# ## Comment pass
#
# The start and end line information will allow us to collect the comments in between
# edits.
# 
using CSTParser
import CSTParser.Tokenize.Tokens

export format

struct Document
    text::AbstractString
    ranges::Vector{UnitRange{Int}}
end

mutable struct State
    indent_width::Int
    max_width::Int
    indents::Int
    offset::Int
    # 0 indicates the start of the line
    line_offset::Int
    doc::Document
end

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

function cursor_loc(s::State, offset::Int)
    for (l, r) in enumerate(s.doc.ranges)
        if offset in r
            return (l, offset - first(r) + 1, length(r))
        end
    end
    error("indexing range 1 - $(last(s.doc.ranges[end])), index used = $(offset)")
end
cursor_loc(s::State) = cursor_loc(s, s.offset)

const Indent = Union{Int, Nothing}

include("pretty.jl")
#= include("flatten.jl") =#

#= function format(text::String; indent_width=4, max_width=80) =#
#=     d = Document(text, newline_ranges(text)) =#
#=     s = State(indent_width, max_width, 0, 1, 0, d) =#
#=     x = CSTParser.parse(text, true) =#
#=     edits = flatten(x, s) =#
#=     #= @info "" edits =# =#
#=     #= return edits =# =#
#=     io = IOBuffer() =#
#=     print_tree(io, edits) =#
#=     #= comments = gather_comments(s, edits.endline+1, length(s.doc.ranges)-1) =# =#
#=     #= write(io, comments) =# =#
#=     return String(take!(io)) =#
#= end =#

function format(text::AbstractString; indent_width=4, max_width=80)
    d = Document(text, newline_ranges(text))
    s = State(indent_width, max_width, 0, 1, 0, d)
    x = CSTParser.parse(text, true)
    e = pretty(x, s)::Edit
    if e.startline != 1
        e = merge_edits(Edit(1, 1, d.text[d.ranges[1]]), e, s)
    end
    if e.endline != length(d.ranges)
        e = merge_edits(e, Edit(length(d.ranges), length(d.ranges), text[d.ranges[end]]), s)
    end
    e.text
end

end # module
