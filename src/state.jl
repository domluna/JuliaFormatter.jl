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

@inline nspaces(s::State) = s.indent
@inline hascomment(d::Document, line::Integer) = haskey(d.comments, line)
@inline has_semicolon(d::Document, line::Integer) = line in d.semicolons

@inline function cursor_loc(s::State, offset::Integer)
    l = s.doc.range_to_line[offset:offset]
    r = s.doc.line_to_range[l]
    return (l, offset - first(r) + 1, length(r))
end
@inline cursor_loc(s::State) = cursor_loc(s, s.offset)
@inline function on_same_line(s::State, offset1::Int, offset2::Int)
    l = s.doc.range_to_line[offset1:offset1]
    r = s.doc.line_to_range[l]
    return offset2 in r
end
