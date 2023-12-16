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

!!! note
    This function mutates the State's (`s`) `line_offset`. If this is not desired
    you should save the value before calling this function and restore it after.
"""
function walk(f, fst::FST, s::State)
    stop = f(fst, s)
    (stop !== nothing || is_leaf(fst)) && return
    walk(f, fst.nodes::Vector, s, fst.indent)
end

function increment_line_offset!(fst::FST, s::State)
    is_leaf(fst) || return
    s.line_offset += length(fst)
    return nothing
end

function add_indent!(fst::FST, s::State, indent)
    indent == 0 && return
    f = (fst::FST, s::State) -> begin
        fst.indent += indent
        return nothing
    end
    lo = s.line_offset
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

function unnest!(style::S, fst::FST, s::State; dedent::Bool) where {S<:AbstractStyle}
    if is_leaf(fst)
        s.line_offset += length(fst)
    end

    dedent && dedent!(style, fst, s)

    if is_leaf(fst) || fst.typ === StringN || !can_nest(fst)
        return
    end

    nl_to_ws!(fst, s)

    return
end

function unnest!(style::S; dedent::Bool) where {S<:AbstractStyle}
    (fst::FST, s::State) -> begin
        unnest!(style, fst, s; dedent = dedent)
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
    style::S,
    fst::FST,
    s::State,
    idx::Int;
    stop_idx::Union{Int,Nothing} = nothing,
)::Bool where {S<:AbstractStyle}
    @assert fst[idx].typ == PLACEHOLDER
    margin = s.line_offset
    if stop_idx === nothing
        margin += sum(length.(fst[idx:end])) + fst.extra_margin
    else
        margin += sum(length.(fst[idx:stop_idx-1]))
    end

    if margin > s.opts.margin ||
       (idx < length(fst.nodes::Vector) && is_comment(fst[idx+1])) ||
       (idx > 1 && is_comment(fst[idx-1]))
        fst[idx] = Newline(length = fst[idx].len)
        s.line_offset = fst.indent
        return true
    end

    nest!(style, fst[idx], s)
    return false
end

function find_all_segment_splits(n::Int, k::Int)
    res = Vector{Int}[]

    if n == k
        return [fill(1, k)]
    elseif k == 1
        return [[n]]
    end

    _bp =
        (t::Vector{Int}, current_sum::Int) -> begin
            if length(t) == k
                if current_sum == n
                    push!(res, t)
                end
                return
            end

            start_val = isempty(t) ? 1 : last(t)
            max_val = n - current_sum - (k - length(t) - 1)

            for i in start_val:min(n, max_val)
                _bp([t; i], current_sum + i)
            end
        end

    _bp(Int[], 0)

    all_splits = Vector{Int}[]
    for r in res
        for c in unique(permutations(r))
            push!(all_splits, c)
        end
    end

    return all_splits
end

"""
Finds the optimal placeholders to turn into a newlines such that the length of the arguments on each line is as close as possible while following margin constraints.
"""
function find_optimal_placeholders_nest(
    fst::FST,
    start_line_offset::Int,
    max_margin::Int,
)::Vector{Int}
    # Placeholder indices including start and end
    placeholder_inds = findall(n -> n.typ === PLACEHOLDER, fst.nodes)
    fst_line_offset = fst.indent
    @info "TYP" fst.typ start_line_offset fst[1].val

    # Function to calculate the length of a segment
    segment_length =
        (start_idx::Int, end_idx::Int) -> begin
            if placeholder_inds[end] == end_idx
                sum(length.(fst[start_idx:end])) + fst.extra_margin
            else
                sum(length.(fst[start_idx:end_idx-1]))
            end
        end

    n = length(placeholder_inds)
    dp = fill(0, n - 1, n - 1)

    # Initialize the lengths of segments with single placeholders
    for i in 1:n-1
        for j in i+1:n
            len = segment_length(placeholder_inds[i], placeholder_inds[j])
            dp[i, j-1] = len
        end
    end

    N = size(dp, 1)

    function find_best_segments(s::Int)
        if s == 1
            return [(1, N)]
        elseif s == N
            return [(i, i) for i in 1:N]
        end

        _f = (t::Vector{Int}) -> begin
            local n = sum(t)
            local ranges = UnitRange{Int}[]
            local s = 1
            for tt in t
                push!(ranges, s:s+tt-1)
                s += tt
            end
            ranges
        end

        all_splits = find_all_segment_splits(N, s)

        best_split = UnitRange{Int}[]
        min_diff = 1_000_000
        @info "all_splits" s all_splits
        for split in all_splits
            ranges = _f(split)
            @info "" split ranges
            lens = [dp[r[1], r[end]] for r in ranges]
            diff = maximum(lens) - minimum(lens)
            if diff < min_diff
                min_diff = diff
                best_split = ranges
            end
        end
        return best_split
    end

    # Calculate best splits for each number of segments s
    segments = Tuple{Int,Int}[]
    for s in 2:N
        segments = find_best_segments(s)
        fits = true
        for (i, s) in enumerate(segments)
            if i == 1
                fits &= fst_line_offset + dp[first(s), last(s)] <= max_margin
            else
                fits &= fst_line_offset + dp[first(s), last(s)] <= max_margin
            end
        end
        fits && break
    end
    @info "" segments

    if length(segments) == length(placeholder_inds) - 1
        return placeholder_inds
    end

    # ex: (ph, arg, ph, arg, ph, arg, ph)
    # Convert segments to placeholder indices
    optimal_placeholders = Int[]
    @info "old" placeholder_inds
    for i in 1:length(segments)-1
        s2 = segments[i+1]
        i1 = first(s2)
        push!(optimal_placeholders, placeholder_inds[i1])
    end
    @info "" optimal_placeholders
    return optimal_placeholders
end
