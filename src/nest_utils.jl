function skip_indent(fst::FST)
    if fst.typ == LITERAL && fst.val == ""
        return true
    elseif fst.typ === NEWLINE || fst.typ === NOTCODE
        return true
    end
    false
end

function walk(f, nodes::Vector{FST}, s::State, indent::Int)
    for (i, n) in enumerate(nodes)
        if n.typ === NEWLINE && i < length(nodes)
            if is_closer(nodes[i+1])
                s.line_offset = nodes[i+1].indent
            elseif !skip_indent(nodes[i+1])
                s.line_offset = indent
            end
        else
            walk(f, n, s)
        end
    end
end

"""
    walk(f, fst::FST, s::State)

Walks `fst` calling `f` on each node.

In situations where descending further into a subtree is not desirable `f`
should return a value other than `nothing`.
"""
function walk(f, fst::FST, s::State)
    stop = f(fst, s)
    (stop != nothing || is_leaf(fst)) && return
    walk(f, fst.nodes, s, fst.indent)
end

function increment_line_offset!(fst::FST, s::State)
    is_leaf(fst) || return
    s.line_offset += length(fst)
    return nothing
end

function add_indent!(fst::FST, s::State, indent)
    indent == 0 && return
    lo = s.line_offset
    f = (fst::FST, s::State) -> begin
        fst.indent += indent
        return nothing
    end
    walk(f, fst, s)
    s.line_offset = lo
end

# unnest, converts newlines to whitespace
function nl_to_ws!(fst::FST, nl_inds::Vector{Int})
    for (i, ind) in enumerate(nl_inds)
        fst[ind] = Whitespace(fst[ind].len)
        i == length(nl_inds) || continue
        pn = fst[ind-1]
        if pn.typ === TRAILINGCOMMA || pn.typ === TRAILINGSEMICOLON
            pn.val = ""
            pn.len = 0
        elseif pn.typ === INVERSETRAILINGSEMICOLON
            pn.val = ";"
            pn.len = 1
        elseif fst.typ === Binary && fst[ind+1].typ === WHITESPACE
            # remove additional indent
            fst[ind+1] = Whitespace(0)
        end
    end
end

function nl_to_ws!(fst::FST, s::State)
    nl_inds = findall(n -> n.typ === NEWLINE && can_nest(n), fst.nodes)
    length(nl_inds) > 0 || return
    margin = s.line_offset + fst.extra_margin + length(fst)
    margin <= s.opts.margin && nl_to_ws!(fst, nl_inds)
    return
end

function dedent!(style::S, fst::FST, s::State) where {S<:AbstractStyle}
    if is_closer(fst) || fst.typ === NOTCODE
        fst.indent -= s.opts.indent
    elseif is_leaf(fst) || fst.typ === StringN
        return
    else
        fst.indent -= s.opts.indent
    end
end

function dedent!(style::YASStyle, fst::FST, s::State)
    if is_closer(fst) || fst.typ === NOTCODE
        fst.indent -= s.opts.indent
    elseif is_leaf(fst) || fst.typ === StringN
        return
    elseif is_unnamed_iterable(fst)
        fst.indent = s.line_offset
        if is_opener(fst[1])
            fst.indent += 1
        end
    elseif is_named_iterable(fst)
        fst.indent = s.line_offset + length(fst[1]) + length(fst[2])
    else
        fst.indent = s.line_offset
    end
end

function unnest!(style::S, fst::FST, s::State) where {S<:AbstractStyle}
    if is_leaf(fst)
        s.line_offset += length(fst)
    end

    dedent!(style, fst, s)

    if is_leaf(fst) || fst.typ === StringN || !can_nest(fst)
        return
    end

    nl_to_ws!(fst, s)

    return
end

function unnest!(style::S) where {S<:AbstractStyle}
    (fst::FST, s::State) -> begin
        unnest!(style, fst, s)
    end
end

"""
    nest_if_over_margin!(
        style,
        fst::FST,
        s::State,
        idx::Int;
        stop_idx::Union{Int,Nothing} = nothing,
    )::Bool

Converts the node at `idx` to a `NEWLINE` if the current margin plus the additional margin
from `fst[idx:stop_idx-1]` is greater than the allowed margin.

If `stop_idx == nothing` the range is `fst[idx:end]`.

Returns whether nesting occurred.
"""
function nest_if_over_margin!(
    style,
    fst::FST,
    s::State,
    idx::Int;
    stop_idx::Union{Int,Nothing} = nothing,
)::Bool
    @assert fst[idx].typ == PLACEHOLDER
    margin = s.line_offset
    if stop_idx === nothing
        margin += sum(length.(fst[idx:end])) + fst.extra_margin
    else
        margin += sum(length.(fst[idx:stop_idx-1]))
    end

    if margin > s.opts.margin ||
       (idx < length(fst.nodes) && is_comment(fst[idx+1])) ||
       (idx > 1 && is_comment(fst[idx-1]))
        fst[idx] = Newline(length = fst[idx].len)
        s.line_offset = fst.indent
        return true
    end

    nest!(style, fst[idx], s)
    return false
end
