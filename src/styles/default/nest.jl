# Nest
#
# If the line exceeds the print width it will be nested.
#
# This is done by replacing `PLACEHOLDER` nodes with `NEWLINE`
# nodes and updating the FST's indent.
#
# `extra_margin` provides additional width to consider from
# the top-level node.
#
# Example:
#
#     LHS op RHS
#
# the length of " op" will be considered when nesting LHS

function skip_indent(fst::FST)
    if fst.typ === CSTParser.LITERAL && fst.val == ""
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

function walk(f, fst::FST, s::State)
    f(fst, s)
    is_leaf(fst) && return
    walk(f, fst.nodes, s, fst.indent)
end

function reset_line_offset!(fst::FST, s::State)
    is_leaf(fst) || return
    s.line_offset += length(fst)
end

function add_indent!(fst::FST, s::State, indent)
    indent == 0 && return
    lo = s.line_offset
    f = (fst::FST, s::State) -> fst.indent += indent
    walk(f, fst, s)
    s.line_offset = lo
end

# unnest, converts newlines to whitespace
function unnest!(fst::FST, nl_inds::Vector{Int})
    for (i, ind) in enumerate(nl_inds)
        fst[ind] = Whitespace(fst[ind].len)
        i == length(nl_inds) || continue
        if fst[ind-1].typ === TRAILINGCOMMA || fst[ind-1].typ === TRAILINGSEMICOLON
            fst[ind-1].val = ""
            fst[ind-1].len = 0
        elseif fst[ind-1].typ === INVERSETRAILINGSEMICOLON
            fst[ind-1].val = ";"
            fst[ind-1].len = 1
        elseif fst.typ === CSTParser.BinaryOpCall && fst[ind+1].typ === WHITESPACE
            # remove additional indent
            fst[ind+1] = Whitespace(0)
        end
    end
end

function dedent!(fst::FST, s::State)
    if is_leaf(fst)
        s.line_offset += length(fst)
        if is_closer(fst) || fst.typ === NOTCODE
            fst.indent -= s.indent_size
        end
        return
    end
    fst.typ === CSTParser.ConditionalOpCall && return
    fst.typ === CSTParser.StringH && return

    # dedent
    fst.indent -= s.indent_size

    fst.force_nest && return

    nl_inds = findall(n -> n.typ === NEWLINE && !n.force_nest, fst.nodes)
    length(nl_inds) > 0 || return
    margin = s.line_offset + fst.extra_margin + length(fst)
    margin <= s.margin || return
    unnest!(fst, nl_inds)
end

"""
    nest_if_over_margin!(
        style,
        fst::FST,
        s::State,
        idx::Int;
        stop_idx::Union{Int,Nothing} = nothing,
    )

Converts the node at `idx` to a `NEWLINE` if the margin until `stop_idx` is greater than
the allowed margin.

If `stop_idx` is `nothing`, the margin of all nodes in `fst` including and after `idx` will
be included.
"""
function nest_if_over_margin!(
    style,
    fst::FST,
    s::State,
    idx::Int;
    stop_idx::Union{Int,Nothing} = nothing,
)
    @assert fst[idx].typ == PLACEHOLDER
    margin = s.line_offset
    if stop_idx === nothing
        margin += sum(length.(fst[idx:end])) + fst.extra_margin
    else
        margin += sum(length.(fst[idx:stop_idx-1]))
    end

    if margin > s.margin || is_comment(fst[idx+1]) || is_comment(fst[idx-1])
        fst[idx] = Newline(length = fst[idx].len)
        s.line_offset = fst.indent
    else
        nest!(style, fst[idx], s)
    end
end

function nest!(
    ds::DefaultStyle,
    nodes::Vector{FST},
    s::State,
    indent::Int;
    extra_margin = 0,
)
    style = getstyle(ds)
    for (i, n) in enumerate(nodes)
        if n.typ === NEWLINE && nodes[i+1].typ === CSTParser.Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NOTCODE && nodes[i+1].typ === CSTParser.Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NEWLINE
            s.line_offset = indent
        else
            n.extra_margin = extra_margin
            nest!(style, n, s)
        end
    end
end
nest!(
    style::S,
    nodes::Vector{FST},
    s::State,
    indent::Int;
    extra_margin = 0,
) where {S<:AbstractStyle} =
    nest!(DefaultStyle(style), nodes, s, indent, extra_margin = extra_margin)

function nest!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    if is_leaf(fst)
        s.line_offset += length(fst)
        return
    end

    if fst.typ === CSTParser.Import
        n_import!(style, fst, s)
    elseif fst.typ === CSTParser.Export
        n_export!(style, fst, s)
    elseif fst.typ === CSTParser.Using
        n_using!(style, fst, s)
    elseif fst.typ === CSTParser.WhereOpCall
        n_whereopcall!(style, fst, s)
    elseif fst.typ === CSTParser.ConditionalOpCall
        n_conditionalopcall!(style, fst, s)
    elseif fst.typ === CSTParser.BinaryOpCall
        line_margin = s.line_offset + length(fst) + fst.extra_margin
        if s.opts.short_to_long_function_def &&
           line_margin > s.margin &&
           fst.ref !== nothing &&
           CSTParser.defines_function(fst.ref[])

            short_to_long_function_def!(fst, s)
        end
        n_binaryopcall!(style, fst, s)
    elseif fst.typ === CSTParser.Curly
        n_curly!(style, fst, s)
    elseif fst.typ === CSTParser.Call
        n_call!(style, fst, s)
    elseif fst.typ === CSTParser.MacroCall
        n_macrocall!(style, fst, s)
    elseif fst.typ === CSTParser.Ref
        n_ref!(style, fst, s)
    elseif fst.typ === CSTParser.TypedVcat
        n_typedvcat!(style, fst, s)
    elseif fst.typ === CSTParser.TupleH
        n_tupleh!(style, fst, s)
    elseif fst.typ === CSTParser.Vect
        n_vect!(style, fst, s)
    elseif fst.typ === CSTParser.Vcat
        n_vcat!(style, fst, s)
    elseif fst.typ === CSTParser.Braces
        n_braces!(style, fst, s)
    elseif fst.typ === CSTParser.BracesCat
        n_bracescat!(style, fst, s)
    elseif fst.typ === CSTParser.InvisBrackets
        n_invisbrackets!(style, fst, s)
    elseif fst.typ === CSTParser.Comprehension
        n_comprehension!(style, fst, s)
    elseif fst.typ === CSTParser.TypedComprehension
        n_typedcomprehension!(style, fst, s)
    elseif fst.typ === CSTParser.Do
        n_do!(style, fst, s)
    elseif fst.typ === CSTParser.Generator
        n_generator!(style, fst, s)
    elseif fst.typ === CSTParser.Filter
        n_filter!(style, fst, s)
    elseif fst.typ === CSTParser.Flatten
        n_flatten!(style, fst, s)
    elseif fst.typ === CSTParser.Block
        n_block!(style, fst, s)
    elseif fst.typ === CSTParser.ChainOpCall
        n_chainopcall!(style, fst, s)
    elseif fst.typ === CSTParser.Comparison
        n_comparison!(style, fst, s)
    elseif fst.typ === CSTParser.For
        n_for!(style, fst, s)
    elseif fst.typ === CSTParser.Let
        n_let!(style, fst, s)
    elseif fst.typ === CSTParser.UnaryOpCall && fst[2].typ === CSTParser.OPERATOR
        n_unaryopcall!(style, fst, s)
    elseif fst.typ === CSTParser.StringH
        n_stringh!(style, fst, s)
    else
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
nest!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    nest!(DefaultStyle(style), fst, s)

function n_stringh!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    # difference in positioning of the string
    # from the source document to the formatted document
    diff = s.line_offset - fst.indent

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst[i+1].indent + diff
        else
            nest!(style, n, s)
        end
    end
end
n_stringh!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_stringh!(DefaultStyle(style), fst, s)

function n_unaryopcall!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    fst[1].extra_margin = fst.extra_margin + length(fst[2])
    nest!(style, fst[1], s)
    nest!(style, fst[2], s)
end
n_unaryopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_unaryopcall!(DefaultStyle(style), fst, s)

function n_do!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    extra_margin = sum(length.(fst[2:3]))
    # make sure there are nodes after "do"
    if fst[4].typ === WHITESPACE
        extra_margin += length(fst[4])
        extra_margin += length(fst[5])
    end
    fst[1].extra_margin = fst.extra_margin + extra_margin
    nest!(style, fst[1], s)
    nest!(style, fst[2:end], s, fst.indent, extra_margin = fst.extra_margin)
end
n_do!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_do!(DefaultStyle(style), fst, s)

# Import,Using,Export
function n_using!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        fst.indent += s.indent_size

        if !fst.force_nest
            if fst.indent + sum(length.(fst[idx+1:end])) <= s.margin
                fst[idx] = Newline(length = fst[idx].len)
                walk(reset_line_offset!, fst, s)
                return
            end
        end

        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                i < length(fst.nodes) && (n.extra_margin = 1)
                nest!(style, n, s)
            end
        end
    else
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
n_using!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_using!(DefaultStyle(style), fst, s)

@inline n_export!(ds::DefaultStyle, fst::FST, s::State) = n_using!(ds, fst, s)
n_export!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_export!(DefaultStyle(style), fst, s)

@inline n_import!(ds::DefaultStyle, fst::FST, s::State) = n_using!(ds, fst, s)
n_import!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_import!(DefaultStyle(style), fst, s)

function n_tupleh!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes)
    opener = length(fst.nodes) > 0 ? is_opener(fst[1]) : false
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        if opener
            fst[end].indent = fst.indent
        end
        if fst.typ !== CSTParser.TupleH || opener
            fst.indent += s.indent_size
        end

        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ";"
                n.len = 1
                nest!(style, n, s)
            elseif n.typ === INVERSETRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(style, n, s)
            elseif opener && (i == 1 || i == length(fst.nodes))
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(style, n, s)
            end
        end

        if opener
            s.line_offset = fst[end].indent + 1
        end
    else
        extra_margin = fst.extra_margin
        opener && (extra_margin += 1)
        nest!(style, fst.nodes, s, fst.indent, extra_margin = extra_margin)
    end
end
n_tupleh!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_tupleh!(DefaultStyle(style), fst, s)

@inline n_vect!(ds::DefaultStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
n_vect!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_vect!(DefaultStyle(style), fst, s)

@inline n_vcat!(ds::DefaultStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
n_vcat!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_vcat!(DefaultStyle(style), fst, s)

@inline n_braces!(ds::DefaultStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
n_braces!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_braces!(DefaultStyle(style), fst, s)

@inline n_bracescat!(ds::DefaultStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
n_bracescat!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_bracescat!(DefaultStyle(style), fst, s)

@inline n_parameters!(ds::DefaultStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
n_parameters!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_parameters!(DefaultStyle(style), fst, s)

@inline n_invisbrackets!(ds::DefaultStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
n_invisbrackets!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_invisbrackets!(DefaultStyle(style), fst, s)

function n_comprehension!(ds::DefaultStyle, fst::FST, s::State; indent = -1)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    closer = is_closer(fst[end])
    if closer && (line_margin > s.margin || fst.force_nest)
        idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
        if idx !== nothing
            fst[idx] = Newline(length = fst[idx].len)
        end
        idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes)
        if idx !== nothing
            fst[idx] = Newline(length = fst[idx].len)
        end

        add_indent!(fst, s, s.indent_size)
        fst[end].indent = fst.indent - s.indent_size
    end

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            if fst.force_nest
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            else
                nest_if_over_margin!(style, fst, s, i)
            end
        elseif i == length(fst.nodes) && !closer
            nest!(style, n, s)
        else
            n.extra_margin = 1
            nest!(style, n, s)
        end
    end

    if closer
        s.line_offset = fst[end].indent + 1
    end
end

n_comprehension!(style::S, fst::FST, s::State; indent = -1) where {S<:AbstractStyle} =
    n_comprehension!(DefaultStyle(style), fst, s, indent = indent)

@inline n_typedcomprehension!(ds::DefaultStyle, fst::FST, s::State) =
    n_comprehension!(ds, fst, s)
n_typedcomprehension!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_typedcomprehension!(DefaultStyle(style), fst, s)

@inline n_generator!(ds::DefaultStyle, fst::FST, s::State) =
    n_comprehension!(ds, fst, s, indent = fst.indent)
n_generator!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_generator!(DefaultStyle(style), fst, s)

@inline n_filter!(ds::DefaultStyle, fst::FST, s::State) =
    n_comprehension!(ds, fst, s, indent = fst.indent)
n_filter!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_filter!(DefaultStyle(style), fst, s)

@inline n_flatten!(ds::DefaultStyle, fst::FST, s::State) =
    n_comprehension!(ds, fst, s, indent = fst.indent)
n_flatten!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_flatten!(DefaultStyle(style), fst, s)

function n_call!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes)

    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        fst[end].indent = fst.indent
        fst.indent += s.indent_size

        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif n.typ === INVERSETRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(style, n, s)
            elseif i == 1 || i == length(fst.nodes)
                n.extra_margin = 1
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(style, n, s)
            end
        end

        s.line_offset = fst[end].indent + 1
    else
        extra_margin = fst.extra_margin
        is_closer(fst[end]) && (extra_margin += 1)
        nest!(style, fst.nodes, s, fst.indent, extra_margin = extra_margin)
    end
end
n_call!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_call!(DefaultStyle(style), fst, s)

@inline n_curly!(ds::DefaultStyle, fst::FST, s::State) = n_call!(ds, fst, s)
n_curly!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_curly!(DefaultStyle(style), fst, s)

@inline n_macrocall!(ds::DefaultStyle, fst::FST, s::State) = n_call!(ds, fst, s)
n_macrocall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_macrocall!(DefaultStyle(style), fst, s)

@inline n_ref!(ds::DefaultStyle, fst::FST, s::State) = n_call!(ds, fst, s)
n_ref!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_ref!(DefaultStyle(style), fst, s)

@inline n_typedvcat!(ds::DefaultStyle, fst::FST, s::State) = n_call!(ds, fst, s)
n_typedvcat!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_typedvcat!(DefaultStyle(style), fst, s)

function n_whereopcall!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    if line_margin > s.margin || fst.force_nest
        line_offset = s.line_offset
        Blen = sum(length.(fst[2:end]))

        fst[1].extra_margin = Blen + fst.extra_margin
        nest!(style, fst[1], s)

        # "B"
        has_braces = is_closer(fst[end])
        if has_braces
            fst[end].indent = fst.indent
        end

        over = (s.line_offset + Blen + fst.extra_margin > s.margin) || fst.force_nest
        fst.indent += s.indent_size
        for (i, n) in enumerate(fst[2:end])
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif is_opener(n)
                if fst.indent - s.line_offset > 1
                    fst.indent = s.line_offset + 1
                    fst[end].indent = s.line_offset
                end
                nest!(style, n, s)
            elseif n.typ === PLACEHOLDER && over
                fst[i+1] = Newline(length = length(n))
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA && over
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif has_braces
                n.extra_margin = 1 + fst.extra_margin
                nest!(style, n, s)
            else
                n.extra_margin = fst.extra_margin
                nest!(style, n, s)
            end
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, fst, s)
    else
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
n_whereopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_whereopcall!(DefaultStyle(style), fst, s)

function n_conditionalopcall!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    if line_margin > s.margin || fst.force_nest
        line_offset = s.line_offset
        fst.indent = s.line_offset
        phs = reverse(findall(n -> n.typ === PLACEHOLDER, fst.nodes))
        for (i, idx) in enumerate(phs)
            if i == 1
                fst[idx] = Newline(length = fst[idx].len)
            else
                nidx = phs[i-1]
                l1 = sum(length.(fst[1:idx-1]))
                l2 = sum(length.(fst[idx:nidx-1]))
                width = line_offset + l1 + l2
                if fst.force_nest || width > s.margin
                    fst[idx] = Newline(length = fst[idx].len)
                end
            end
        end

        for (i, n) in enumerate(fst.nodes)
            if i == length(fst.nodes)
                n.extra_margin = fst.extra_margin
                nest!(style, n, s)
            elseif fst[i+1].typ === WHITESPACE
                n.extra_margin = length(fst[i+1]) + length(fst[i+2])
                nest!(style, n, s)
            elseif n.typ === NEWLINE
                s.line_offset = fst.indent
            else
                nest!(style, n, s)
            end
        end

        s.line_offset = line_offset
        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE && !is_comment(fst[i+1]) && !is_comment(fst[i-1])
                # +1 for newline to whitespace conversion
                width = s.line_offset + 1
                if i == length(fst.nodes) - 1
                    width += sum(length.(fst[i+1:end])) + fst.extra_margin
                else
                    width += sum(length.(fst[i+1:i+3]))
                end
                # @debug "" s.line_offset l  s.margin
                if width <= s.margin
                    fst[i] = Whitespace(1)
                else
                    s.line_offset = fst.indent
                end
            end
            s.line_offset += length(fst[i])
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, fst, s)
    else
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
n_conditionalopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_conditionalopcall!(DefaultStyle(style), fst, s)

no_unnest(fst::FST) = fst.typ === CSTParser.BinaryOpCall && contains_comment(fst)

function n_binaryopcall!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    # If there's no placeholder the binary call is not nestable
    idxs = findall(n -> n.typ === PLACEHOLDER, fst.nodes)
    rhs = fst[end]
    rhs.typ === CSTParser.Block && (rhs = rhs[1])
    if length(idxs) == 2 && (line_margin > s.margin || fst.force_nest || rhs.force_nest)
        line_offset = s.line_offset
        i1 = idxs[1]
        i2 = idxs[2]
        fst[i1] = Newline(length = fst[i1].len)
        cst = fst.ref[]

        indent_nest =
            CSTParser.defines_function(cst) ||
            nest_assignment(cst) ||
            cst[2].kind === Tokens.PAIR_ARROW ||
            cst[2].kind === Tokens.ANON_FUNC ||
            is_standalone_shortcircuit(cst)

        if indent_nest
            s.line_offset = fst.indent + s.indent_size
            fst[i2] = Whitespace(s.indent_size)
            add_indent!(fst[end], s, s.indent_size)
        else
            fst.indent = s.line_offset
        end

        # rhs
        fst[end].extra_margin = fst.extra_margin
        nest!(style, fst[end], s)

        # "lhs op" rhs
        s.line_offset = line_offset

        # extra margin for " op"
        fst[1].extra_margin = length(fst[2]) + length(fst[3])
        nest!(style, fst[1], s)
        for n in fst[2:i1]
            nest!(style, n, s)
        end

        # Undo nest if possible
        if !fst.force_nest && !no_unnest(rhs)
            cst = rhs.ref[]
            line_margin = s.line_offset

            if (rhs.typ === CSTParser.BinaryOpCall && cst[2].kind !== Tokens.IN) ||
               rhs.typ === CSTParser.UnaryOpCall ||
               rhs.typ === CSTParser.ChainOpCall ||
               rhs.typ === CSTParser.Comparison
                line_margin += length(fst[end])
            elseif rhs.typ === CSTParser.Do && is_iterable(rhs[1])
                rw, _ = length_to(fst, (NEWLINE,), start = i2 + 1)
                line_margin += rw
            elseif is_block(cst)
                idx = findfirst(n -> n.typ === NEWLINE, rhs.nodes)
                if idx === nothing
                    line_margin += length(fst[end])
                else
                    line_margin += sum(length.(rhs[1:idx-1]))
                end
            else
                rw, _ = length_to(fst, (NEWLINE,), start = i2 + 1)
                line_margin += rw
            end

            if line_margin + fst.extra_margin <= s.margin
                fst[i1] = Whitespace(1)
                if indent_nest
                    fst[i2] = Whitespace(0)
                    walk(dedent!, rhs, s)
                end
            end
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, fst, s)
    else
        # length of op and surrounding whitespace
        oplen = sum(length.(fst[2:end]))

        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif i == 1
                n.extra_margin = oplen + fst.extra_margin
                nest!(style, n, s)
            elseif i == length(fst.nodes)
                n.extra_margin = fst.extra_margin
                nest!(style, n, s)
            else
                nest!(style, n, s)
            end
        end
    end
end
n_binaryopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_binaryopcall!(DefaultStyle(style), fst, s)

function n_for!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    block_idx = findfirst(n -> !is_leaf(n), fst.nodes)
    if block_idx === nothing
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
        return
    end
    ph_idx = findfirst(n -> n.typ === PLACEHOLDER, fst[block_idx].nodes)
    nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)

    # return if the argument block was nested
    ph_idx !== nothing && fst[3][ph_idx].typ === NEWLINE && return

    idx = 5
    n = fst[idx]
    if n.typ === NOTCODE && n.startline == n.endline
        res = get(s.doc.comments, n.startline, (0, ""))
        res == (0, "") && (fst[idx-1] = Whitespace(0))
    end
end
n_for!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_for!(DefaultStyle(style), fst, s)

@inline n_let!(ds::DefaultStyle, fst::FST, s::State) = n_for!(ds, fst, s)
n_let!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_let!(DefaultStyle(style), fst, s)

function n_block!(ds::DefaultStyle, fst::FST, s::State; indent = -1)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
    if idx !== nothing && (line_margin > s.margin || fst.force_nest)
        line_offset = s.line_offset
        indent >= 0 && (fst.indent = indent)

        if fst.typ === CSTParser.ChainOpCall && is_standalone_shortcircuit(fst.ref[])
            fst.indent += s.indent_size
        end

        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif i < length(fst.nodes) - 1 && fst[i+2].typ === CSTParser.OPERATOR
                # chainopcall / comparison
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1 + length(fst[i+2])
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(style, n, s)
            end
        end
    else
        nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
n_block!(style::S, fst::FST, s::State; indent = 0) where {S<:AbstractStyle} =
    n_block!(DefaultStyle(style), fst, s, indent = indent)

@inline n_comparison!(ds::DefaultStyle, fst::FST, s::State) =
    n_block!(ds, fst, s, indent = s.line_offset)
n_comparison!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_comparison!(DefaultStyle(style), fst, s)

@inline n_chainopcall!(ds::DefaultStyle, fst::FST, s::State) =
    n_block!(ds, fst, s, indent = s.line_offset)
n_chainopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_chainopcall!(DefaultStyle(style), fst, s)
