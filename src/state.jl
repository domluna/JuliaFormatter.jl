mutable struct State
    doc::Document
    indent::Int
    offset::Int
    line_offset::Int

    # If true, output is formatted text otherwise
    # it's source text
    on::Bool
    opts::Options
end
State(doc, opts) = State(doc, 0, 1, 0, true, opts)

nspaces(s::State) = s.indent
hascomment(d::Document, line::Integer) = haskey(d.comments, line)

"""
    has_semicolon(d::Document, line::Integer)

Returns whether `d` has a valid semicolon grouping on `line`.
"""
function has_semicolon(d::Document, line::Integer)
    !haskey(d.semicolons, line) && return false
    return length(d.semicolons[line]) > 0
end

function cursor_loc(s::State, offset::Integer)
    l = JuliaSyntax.source_line(s.doc.srcfile, offset)
    r = JuliaSyntax.source_line_range(s.doc.srcfile, offset)
    # return (l, first(r), last(r))
    return (l, offset - first(r) + 1, length(r))
end
cursor_loc(s::State) = cursor_loc(s, s.offset)

function on_same_line(s::State, offset1::Int, offset2::Int)
    l1 = JuliaSyntax.source_line(s.doc.srcfile, offset1)
    l2 = JuliaSyntax.source_line(s.doc.srcfile, offset2)
    return l1 == l2
end

function linerange(s::State, line::Int)
    f = s.doc.srcfile.line_starts[line]
    r = JuliaSyntax.source_line_range(s.doc.srcfile, f)
    return (first(r), last(r))
end
