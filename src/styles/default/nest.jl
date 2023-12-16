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

function nest!(
    ds::DefaultStyle,
    nodes::Vector{FST},
    s::State,
    indent::Int;
    extra_margin = 0,
)
    style = getstyle(ds)
    for (i, n) in enumerate(nodes)
        if n.typ === NEWLINE && nodes[i+1].typ === Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NOTCODE && nodes[i+1].typ === Block
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

    if cant_nest(fst)
        walk(increment_line_offset!, fst, s)
        return
    end

    if fst.typ === FunctionN &&
       s.opts.long_to_short_function_def &&
       fst.ref !== nothing &&
       CSTParser.defines_function(fst.ref[])
        long_to_short_function_def!(fst, s)
    end

    if fst.typ === Import
        n_import!(style, fst, s)
    elseif fst.typ === Export
        n_export!(style, fst, s)
    elseif fst.typ === Using
        n_using!(style, fst, s)
    elseif fst.typ === Where
        n_whereopcall!(style, fst, s)
    elseif fst.typ === Conditional
        line_margin = s.line_offset + length(fst) + fst.extra_margin
        if s.opts.conditional_to_if && line_margin > s.opts.margin
            conditional_to_if_block!(fst, s)
            nest!(style, fst, s)
        else
            n_conditionalopcall!(style, fst, s)
        end
    elseif fst.typ === Binary
        line_margin = s.line_offset + length(fst) + fst.extra_margin
        if s.opts.short_to_long_function_def &&
           line_margin > s.opts.margin &&
           fst.ref !== nothing &&
           CSTParser.defines_function(fst.ref[]) &&
           !parent_is(fst.ref[], n -> n.head == :let)
            short_to_long_function_def!(fst, s)
        end
        if fst.typ === Binary
            n_binaryopcall!(style, fst, s)
        else
            nest!(style, fst, s)
        end
    elseif fst.typ === Curly
        n_curly!(style, fst, s)
    elseif fst.typ === Call
        n_call!(style, fst, s)
    elseif fst.typ === MacroCall
        n_macrocall!(style, fst, s)
    elseif fst.typ === RefN
        n_ref!(style, fst, s)
    elseif fst.typ === TypedVcat
        n_typedvcat!(style, fst, s)
    elseif fst.typ === TypedNcat
        n_typedvcat!(style, fst, s)
    elseif fst.typ === TupleN
        n_tuple!(style, fst, s)
    elseif fst.typ === Vect
        n_vect!(style, fst, s)
    elseif fst.typ === Vcat
        n_vcat!(style, fst, s)
    elseif fst.typ === Ncat
        n_vcat!(style, fst, s)
    elseif fst.typ === Braces
        n_braces!(style, fst, s)
    elseif fst.typ === BracesCat
        n_bracescat!(style, fst, s)
    elseif fst.typ === Brackets
        n_invisbrackets!(style, fst, s)
    elseif fst.typ === Comprehension
        n_comprehension!(style, fst, s)
    elseif fst.typ === TypedComprehension
        n_typedcomprehension!(style, fst, s)
    elseif fst.typ === Do
        n_do!(style, fst, s)
    elseif fst.typ === Generator
        n_generator!(style, fst, s)
    elseif fst.typ === Filter
        n_filter!(style, fst, s)
    elseif fst.typ === Flatten
        n_flatten!(style, fst, s)
    elseif fst.typ === Block
        n_block!(style, fst, s)
    elseif fst.typ === Chain
        n_chainopcall!(style, fst, s)
    elseif fst.typ === Comparison
        n_comparison!(style, fst, s)
    elseif fst.typ === For
        n_for!(style, fst, s)
    elseif fst.typ === Let
        n_let!(style, fst, s)
    elseif fst.typ === Unary && length(fst.nodes::Vector) > 1 && fst[2].typ === OPERATOR
        n_unaryopcall!(style, fst, s)
    elseif fst.typ === StringN
        n_string!(style, fst, s)
    elseif fst.typ === FunctionN
        n_functiondef!(style, fst, s)
    elseif fst.typ === Macro
        n_macro!(style, fst, s)
    else
        nest!(style, fst.nodes::Vector, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
nest!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    nest!(DefaultStyle(style), fst, s)

function n_functiondef!(ds::DefaultStyle, fst::FST, s::State)
    nest!(ds, fst.nodes::Vector, s, fst.indent, extra_margin = fst.extra_margin)
end
n_functiondef!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_functiondef!(DefaultStyle(style), fst, s)

n_macro!(ds::DefaultStyle, fst::FST, s::State) = n_functiondef!(ds, fst, s)
n_macro!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_macro!(DefaultStyle(style), fst, s)

function n_string!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    # difference in positioning of the string
    # from the source document to the formatted document
    diff = s.line_offset - fst.indent

    for (i, n) in enumerate(fst.nodes::Vector)
        if n.typ === NEWLINE
            s.line_offset = fst[i+1].indent + diff
        else
            nest!(style, n, s)
        end
    end
end
n_string!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_string!(DefaultStyle(style), fst, s)

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
    nodes = fst.nodes::Vector
    idx = findfirst(n -> n.typ === PLACEHOLDER, nodes)
    fst.indent += s.opts.indent

    if idx !== nothing && (line_margin > s.opts.margin || must_nest(fst))
        if can_nest(fst)
            if fst.indent + sum(length.(fst[idx+1:end])) <= s.opts.margin
                fst[idx] = Newline(length = fst[idx].len)
                walk(increment_line_offset!, fst, s)
                return
            end
        end

        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                if s.opts.join_lines_based_on_source
                    si = findnext(n -> n.typ === PLACEHOLDER, nodes, i + 1)
                    nest_if_over_margin!(style, fst, s, i; stop_idx = si)
                else
                    fst[i] = Newline(length = n.len)
                    s.line_offset = fst.indent
                end
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                i < length(nodes) && (n.extra_margin = 1)
                nest!(style, n, s)
            end
        end
    else
        nest!(style, nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
n_using!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_using!(DefaultStyle(style), fst, s)

n_export!(ds::DefaultStyle, fst::FST, s::State) = n_using!(ds, fst, s)
n_export!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_export!(DefaultStyle(style), fst, s)

n_import!(ds::DefaultStyle, fst::FST, s::State) = n_using!(ds, fst, s)
n_import!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_import!(DefaultStyle(style), fst, s)

function n_tuple!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    nodes = fst.nodes::Vector
    idx = findlast(n -> n.typ === PLACEHOLDER, nodes)
    has_closer = is_closer(fst[end])

    if has_closer
        fst[end].indent = fst.indent
    end
    if fst.typ !== TupleN || has_closer
        fst.indent += s.opts.indent
    end

    # "foo(a, b, c)" is true if "foo" and "c" are on different lines
    src_diff_line = if s.opts.join_lines_based_on_source
        last_arg_idx = findlast(is_iterable_arg, nodes)
        last_arg = last_arg_idx === nothing ? fst[end] : fst[last_arg_idx]
        fst[1].endline != last_arg.startline
    else
        false
    end

    if idx !== nothing && (line_margin > s.opts.margin || must_nest(fst) || src_diff_line)
        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                if src_diff_line
                    si = findnext(
                        n -> n.typ === PLACEHOLDER || n.typ === NEWLINE,
                        nodes,
                        i + 1,
                    )
                    nested = nest_if_over_margin!(style, fst, s, i; stop_idx = si)
                    if has_closer && !nested && n.startline == fst[end].startline
                        # trailing types are automatically converted, undo this if
                        # there is no nest and the closer is on the same in the
                        # original source.
                        if fst[i-1].typ === TRAILINGCOMMA ||
                           fst[i-1].typ === TRAILINGSEMICOLON
                            fst[i-1].val = ""
                            fst[i-1].len = 0
                        end
                    end
                else
                    fst[i] = Newline(length = n.len)
                    s.line_offset = fst.indent
                end
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ";"
                n.len = 1
                nest!(style, n, s)
            elseif n.typ === INVERSETRAILINGSEMICOLON && !(
                s.opts.join_lines_based_on_source &&
                (fst.typ === Vcat || fst.typ === TypedVcat)
            )
                n.val = ""
                n.len = 0
                nest!(style, n, s)
            elseif has_closer && (i == 1 || i == length(nodes))
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(style, n, s)
            end
        end

        if has_closer
            s.line_offset = fst[end].indent + 1
        end
    else
        extra_margin = fst.extra_margin
        has_closer && (extra_margin += 1)
        nest!(style, nodes, s, fst.indent, extra_margin = extra_margin)
    end

    return
end
n_tuple!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_tuple!(DefaultStyle(style), fst, s)

n_vect!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_vect!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_vect!(DefaultStyle(style), fst, s)

n_vcat!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_vcat!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_vcat!(DefaultStyle(style), fst, s)

n_braces!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_braces!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_braces!(DefaultStyle(style), fst, s)

n_bracescat!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_bracescat!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_bracescat!(DefaultStyle(style), fst, s)

n_parameters!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_parameters!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_parameters!(DefaultStyle(style), fst, s)

n_invisbrackets!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_invisbrackets!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_invisbrackets!(DefaultStyle(style), fst, s)

n_call!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_call!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_call!(DefaultStyle(style), fst, s)

n_curly!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_curly!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_curly!(DefaultStyle(style), fst, s)

n_macrocall!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_macrocall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_macrocall!(DefaultStyle(style), fst, s)

n_ref!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_ref!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_ref!(DefaultStyle(style), fst, s)

n_typedvcat!(ds::DefaultStyle, fst::FST, s::State) = n_tuple!(ds, fst, s)
n_typedvcat!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_typedvcat!(DefaultStyle(style), fst, s)

function n_comprehension!(ds::DefaultStyle, fst::FST, s::State)
    if s.opts.join_lines_based_on_source
        n_tuple!(ds, fst, s)
    else
        _n_comprehension!(ds, fst, s)
    end
    return
end

function _n_comprehension!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_offset = s.line_offset
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    nested = false

    line_margin = s.line_offset + length(fst) + fst.extra_margin
    closer = is_closer(fst[end])
    nodes = fst.nodes::Vector

    if closer && (line_margin > s.opts.margin || must_nest(fst))
        idx = findfirst(n -> n.typ === PLACEHOLDER, nodes)
        if idx !== nothing
            fst[idx] = Newline(length = fst[idx].len)
        end
        idx = findlast(n -> n.typ === PLACEHOLDER, nodes)
        if idx !== nothing
            fst[idx] = Newline(length = fst[idx].len)
        end

        add_indent!(fst, s, s.opts.indent)
        fst[end].indent = fst.indent - s.opts.indent
        nested = true
    end

    for (i, n) in enumerate(nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === PLACEHOLDER
            if must_nest(fst)
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            else
                nest_if_over_margin!(style, fst, s, i)
            end
        elseif i == length(nodes) && !closer
            nest!(style, n, s)
        else
            n.extra_margin = nested ? 0 : 1
            nest!(style, n, s)
        end
    end

    if closer
        s.line_offset = fst[end].indent + 1
    end
end

n_comprehension!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_comprehension!(DefaultStyle(style), fst, s)

n_typedcomprehension!(ds::DefaultStyle, fst::FST, s::State) = n_comprehension!(ds, fst, s)
n_typedcomprehension!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_typedcomprehension!(DefaultStyle(style), fst, s)

function n_generator!(ds::DefaultStyle, fst::FST, s::State; indent = -1)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    line_offset = s.line_offset
    fst.indent = s.line_offset

    nodes = fst.nodes::Vector
    if line_margin > s.opts.margin || must_nest(fst) || s.opts.join_lines_based_on_source
        phs = reverse(findall(n -> n.typ === PLACEHOLDER, nodes))
        if s.opts.join_lines_based_on_source
            phs = filter(idx -> fst[idx+1].typ !== NEWLINE, phs)
        end
        for (i, idx) in enumerate(phs)
            if i == 1
                fst[idx] = Newline(length = fst[idx].len)
            else
                nidx = phs[i-1]
                l1 = sum(length.(fst[1:idx-1]))
                l2 = sum(length.(fst[idx:nidx-1]))
                width = line_offset + l1 + l2
                if must_nest(fst) || width > s.opts.margin
                    fst[idx] = Newline(length = fst[idx].len)
                end
            end
        end

        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif i == length(nodes)
                nest!(style, n, s)
            else
                n.extra_margin = 1
                nest!(style, n, s)
            end
        end

        s.line_offset = line_offset
        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE &&
               !is_comment(fst[i+1]) &&
               !is_comment(fst[i-1]) &&
               i in phs
                # +1 for newline to whitespace conversion
                width = s.line_offset + 1
                w, _ = length_to(fst, (NEWLINE,), start = i + 1)
                width += w
                if width <= s.opts.margin
                    fst[i] = Whitespace(1)
                    s.line_offset += length(fst[i])
                else
                    s.line_offset = fst.indent
                end
            elseif n.typ === NEWLINE
                s.line_offset = fst.indent
            else
                walk(increment_line_offset!, fst[i], s)
            end
        end

        s.line_offset = line_offset
        walk(increment_line_offset!, fst, s)
    else
        nest!(style, nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end

n_generator!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_generator!(DefaultStyle(style), fst, s)

n_filter!(ds::DefaultStyle, fst::FST, s::State) =
    n_generator!(ds, fst, s, indent = fst.indent)
n_filter!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_filter!(DefaultStyle(style), fst, s)

n_flatten!(ds::DefaultStyle, fst::FST, s::State) =
    n_generator!(ds, fst, s, indent = fst.indent)
n_flatten!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_flatten!(DefaultStyle(style), fst, s)

function n_whereopcall!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    # "B"
    has_braces = is_closer(fst[end])
    if has_braces
        fst[end].indent = fst.indent
    end

    if line_margin > s.opts.margin || must_nest(fst)
        line_offset = s.line_offset
        Blen = sum(length.(fst[2:end]))

        fst[1].extra_margin = Blen + fst.extra_margin
        nest!(style, fst[1], s)

        over = (s.line_offset + Blen + fst.extra_margin > s.opts.margin) || must_nest(fst)
        fst.indent += s.opts.indent
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
        walk(increment_line_offset!, fst, s)
        return
    end

    nest!(style, fst.nodes::Vector, s, fst.indent, extra_margin = fst.extra_margin)
    return
end
n_whereopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_whereopcall!(DefaultStyle(style), fst, s)

function n_conditionalopcall!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    line_offset = s.line_offset
    fst.indent = s.line_offset

    nodes = fst.nodes::Vector
    if line_margin > s.opts.margin || must_nest(fst) || s.opts.join_lines_based_on_source
        phs = reverse(findall(n -> n.typ === PLACEHOLDER, nodes))
        if s.opts.join_lines_based_on_source
            phs = filter(idx -> fst[idx+1].typ !== NEWLINE, phs)
        end
        for (i, idx) in enumerate(phs)
            if i == 1
                fst[idx] = Newline(length = fst[idx].len)
            else
                nidx = phs[i-1]
                l1 = sum(length.(fst[1:idx-1]))
                l2 = sum(length.(fst[idx:nidx-1]))
                width = line_offset + l1 + l2
                if must_nest(fst) || width > s.opts.margin
                    fst[idx] = Newline(length = fst[idx].len)
                end
            end
        end

        for (i, n) in enumerate(nodes)
            if i == length(nodes)
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
        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE &&
               !is_comment(fst[i+1]) &&
               !is_comment(fst[i-1]) &&
               i in phs
                # +1 for newline to whitespace conversion
                width = s.line_offset + 1
                if i == length(nodes) - 1
                    width += sum(length.(fst[i+1:end])) + fst.extra_margin
                else
                    width += sum(length.(fst[i+1:i+3]))
                end
                if width <= s.opts.margin
                    fst[i] = Whitespace(1)
                    s.line_offset += length(fst[i])
                else
                    s.line_offset = fst.indent
                end
            elseif n.typ === NEWLINE
                s.line_offset = fst.indent
            else
                walk(increment_line_offset!, n, s)
            end
        end

        s.line_offset = line_offset
        walk(increment_line_offset!, fst, s)
    else
        nest!(style, nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
end
n_conditionalopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_conditionalopcall!(DefaultStyle(style), fst, s)

function no_unnest(fst::FST)
    if fst.typ === Binary ||
       fst.typ === Conditional ||
       fst.typ === Chain ||
       fst.typ === Comparison
        return contains_comment(fst)
    end
    return false
end

function n_binaryopcall!(ds::DefaultStyle, fst::FST, s::State; indent::Int = -1)
    style = getstyle(ds)
    line_offset = s.line_offset
    line_margin = line_offset + length(fst) + fst.extra_margin

    # If there's no placeholder the binary call is not nestable
    nodes = fst.nodes::Vector
    idxs = findall(n -> n.typ === PLACEHOLDER, nodes)

    rhs = fst[end]
    rhs.typ === Block && (rhs = rhs[1])

    # is the LHS on a different line than the RHS ?
    src_diff_line =
        s.opts.join_lines_based_on_source && fst[1].endline != fst[end].startline

    if length(idxs) == 2 &&
       (line_margin > s.opts.margin || must_nest(fst) || must_nest(rhs) || src_diff_line)
        i1 = idxs[1]
        i2 = idxs[2]
        fst[i1] = Newline(length = fst[i1].len)
        cst = fst.ref[]

        indent_nest =
            CSTParser.defines_function(cst) ||
            is_assignment(cst) ||
            op_kind(fst) === Tokens.PAIR_ARROW ||
            op_kind(fst) === Tokens.ANON_FUNC ||
            (fst.ref !== nothing && is_standalone_shortcircuit(cst))

        if indent_nest
            s.line_offset = fst.indent + s.opts.indent
            fst[i2] = Whitespace(s.opts.indent)

            # reset the indent of the RHS
            if fst[end].indent > fst.indent
                fst[end].indent = fst.indent
            end
            add_indent!(fst[end], s, s.opts.indent)
        else
            if indent >= 0
                fst.indent = indent
            else
                fst.indent = s.line_offset
            end
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
        if can_nest(fst) && !no_unnest(rhs) && !src_diff_line
            line_margin = s.line_offset

            # replace IN with all of precedence level 6
            if (
                   rhs.typ === Binary &&
                   !(op_kind(rhs) === Tokens.IN || op_kind(rhs) === Tokens.DECLARATION)
               ) ||
               rhs.typ === Unary && rhs[end].typ !== Brackets ||
               rhs.typ === Chain ||
               rhs.typ === Comparison ||
               rhs.typ === Conditional
                line_margin += length(fst[end])
            elseif rhs.typ === Do && is_iterable(rhs[1])
                rw, _ = length_to(fst, (NEWLINE,), start = i2 + 1)
                line_margin += rw
            elseif is_block(rhs)
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

            if line_margin + fst.extra_margin <= s.opts.margin
                fst[i1] = Whitespace(1)
                if indent_nest || style isa YASStyle
                    fst[i2] = Whitespace(0)
                    walk(unnest!(style), rhs, s)
                end
            end
        end

        s.line_offset = line_offset
        walk(increment_line_offset!, fst, s)
        return
    end

    # length of op and surrounding whitespace
    oplen = sum(length.(fst[2:end]))

    for (i, n) in enumerate(nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif i == 1
            n.extra_margin = oplen + fst.extra_margin
            nest!(style, n, s)
        elseif i == length(nodes)
            n.extra_margin = fst.extra_margin
            nest!(style, n, s)
        else
            nest!(style, n, s)
        end
    end
end
n_binaryopcall!(style::S, fst::FST, s::State; indent = -1) where {S<:AbstractStyle} =
    n_binaryopcall!(DefaultStyle(style), fst, s; indent = indent)

function n_for!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    nest!(style, fst.nodes::Vector, s, fst.indent, extra_margin = fst.extra_margin)
end
n_for!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_for!(DefaultStyle(style), fst, s)

n_let!(ds::DefaultStyle, fst::FST, s::State) = n_for!(ds, fst, s)
n_let!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_let!(DefaultStyle(style), fst, s)

function n_block!(ds::DefaultStyle, fst::FST, s::State; indent = -1)
    style = getstyle(ds)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    nodes = fst.nodes::Vector
    idx = findfirst(n -> n.typ === PLACEHOLDER, nodes)
    has_nl = false
    indent >= 0 && (fst.indent = indent)

    if fst.typ === Chain && fst.ref !== nothing && is_standalone_shortcircuit(fst.ref[])
        fst.indent += s.opts.indent
    end

    if idx !== nothing &&
       (line_margin > s.opts.margin || must_nest(fst) || s.opts.join_lines_based_on_source)
        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
                has_nl = true
            elseif n.typ === PLACEHOLDER
                # NOTE:
                #
                # Placeholder at the end of a block is the separator
                # to the body. For example:
                #
                # for x in (a, b), y in (c, d)
                #     body
                # end
                #
                # If "x in (a, b), y in (c, d)" is nested then a newline
                # is inserted to separte "y in (c, d)" from "body":
                #
                # for x in (a, b),
                #     y in (c, d)
                #
                #     body
                # end
                #
                if s.opts.join_lines_based_on_source
                    if i < length(nodes)
                        si = findnext(
                            n -> n.typ === PLACEHOLDER || n.typ === NEWLINE,
                            nodes,
                            i + 1,
                        )
                        nest_if_over_margin!(style, fst, s, i; stop_idx = si)
                    elseif has_nl
                        fst[i] = Newline(length = n.len)
                        s.line_offset = fst.indent
                    else
                        nest!(style, n, s)
                    end
                else
                    fst[i] = Newline(length = n.len)
                    s.line_offset = fst.indent
                    has_nl = true
                end
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                if i < length(nodes) - 1 && fst[i+2].typ === OPERATOR
                    # chainopcall / comparison
                    n.extra_margin = 1 + length(fst[i+2])
                elseif i < length(nodes)
                    if !(i == length(nodes) - 1 && fst[i+1].typ === PLACEHOLDER)
                        n.extra_margin = 1
                    end
                end
                nest!(style, n, s)
            end
        end
    else
        nest!(style, nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end
    return
end
n_block!(style::S, fst::FST, s::State; indent = -1) where {S<:AbstractStyle} =
    n_block!(DefaultStyle(style), fst, s, indent = indent)

n_comparison!(ds::DefaultStyle, fst::FST, s::State) =
    n_block!(ds, fst, s, indent = s.line_offset)
n_comparison!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_comparison!(DefaultStyle(style), fst, s)

n_chainopcall!(ds::DefaultStyle, fst::FST, s::State) =
    n_block!(ds, fst, s, indent = s.line_offset)
n_chainopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    n_chainopcall!(DefaultStyle(style), fst, s)
