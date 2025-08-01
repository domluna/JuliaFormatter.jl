for f in [
    :n_import!,
    :n_using!,
    :n_export!,
    :n_public!,
    :n_vcat!,
    :n_ncat!,
    :n_typedvcat!,
    :n_typedncat!,
    :n_row!,
    :n_nrow!,
    :n_hcat!,
    :n_comprehension!,
    :n_typedcomprehension!,
    :n_generator!,
    :n_filter!,
]
    @eval function $f(
        ss::SciMLStyle,
        fst::FST,
        s::State,
        lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
    )
        $f(YASStyle(getstyle(ss)), fst, s, lineage)
    end
end

function n_functiondef!(
    ss::SciMLStyle,
    fst::FST,
    s::State,
    lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
)
    style = getstyle(ss)
    nested = false
    if s.opts.yas_style_nesting
        nested |= nest!(
            YASStyle(style),
            fst.nodes::Vector,
            s,
            fst.indent,
            lineage;
            extra_margin = fst.extra_margin,
        )
    else
        base_indent = fst.indent
        add_indent!(fst[3], s, s.opts.indent)

        nested |= nest!(
            ss,
            fst.nodes::Vector,
            s,
            fst.indent,
            lineage;
            extra_margin = fst.extra_margin,
        )

        f =
            (fst::FST, s::State) -> begin
                if is_closer(fst) && fst.indent == base_indent + s.opts.indent
                    fst.indent -= s.opts.indent
                end
            end
        lo = s.line_offset
        walk(f, fst[3], s)
        s.line_offset = lo
    end
    return nested
end

function n_macro!(
    ss::SciMLStyle,
    fst::FST,
    s::State,
    lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
)
    n_functiondef!(ss, fst, s, lineage)
end

function _n_tuple!(
    ss::SciMLStyle,
    fst::FST,
    s::State,
    lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
)
    style = getstyle(ss)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    nodes = fst.nodes::Vector
    has_closer = is_closer(fst[end])
    start_line_offset = s.line_offset

    # Initialize variables needed for LHS tuple detection
    is_lhs_tuple = false
    has_newline = false

    if has_closer
        fst[end].indent = fst.indent
    end
    if !(fst.typ in (TupleN, CartesianIterator, Parameters)) || has_closer
        fst.indent += s.opts.indent
    elseif fst.typ === TupleN && is_lhs_tuple && has_newline
        # For LHS tuples that are already split, we still need proper indentation
        fst.indent = s.opts.indent  # Ensure proper indentation for split LHS tuples
    end

    # "foo(a, b, c)" is true if "foo" and "c" are on different lines
    src_diff_line = if s.opts.join_lines_based_on_source
        last_arg_idx = findlast(is_iterable_arg, nodes)
        last_arg = last_arg_idx === nothing ? fst[end] : fst[last_arg_idx]
        fst[1].endline != last_arg.startline
    else
        false
    end

    nested = false

    # Check if this is a LHS tuple that shouldn't be broken
    skip_optimal_nesting = false
    if fst.typ === TupleN && length(lineage) >= 1
        for (node_type, metadata) in reverse(lineage)
            if node_type === Binary && !isnothing(metadata) && metadata.is_assignment
                # This is a LHS tuple in an assignment
                is_lhs_tuple = true
                # Check if it's not already split
                for (i, n) in enumerate(nodes)
                    if n.typ === NEWLINE
                        has_newline = true
                        # Ensure nodes after newline have proper indent
                        for j in (i+1):length(nodes)
                            if nodes[j].typ !== PLACEHOLDER &&
                               nodes[j].typ !== WHITESPACE &&
                               nodes[j].typ !== NEWLINE
                                nodes[j].indent = fst.indent
                            end
                        end
                        break
                    end
                end
                if !has_newline
                    # Don't break LHS tuples - mark as NeverNest
                    fst.nest_behavior = NeverNest
                    skip_optimal_nesting = true
                end
                break
            end
        end
    end

    optimal_placeholders = if skip_optimal_nesting
        Int[]
    else
        find_optimal_nest_placeholders(fst, fst.indent, s.opts.margin)
    end
    if length(optimal_placeholders) > 0
        nested = true
    end

    for i in optimal_placeholders
        fst[i] = Newline(; length = fst[i].len)
    end

    placeholder_inds = findall(n -> n.typ === PLACEHOLDER, fst.nodes)
    for (i, ph) in enumerate(placeholder_inds)
        if i == 1 ||
           i == length(placeholder_inds) ||
           (ph < length(fst) && is_comment(fst[ph+1])) ||
           (ph > 1 && is_comment(fst[ph-1]))
            continue
        end
        fst[ph] = Whitespace(fst[ph].len)
    end

    # macrocall doesn't have a placeholder before the closing parenthesis
    if fst.typ !== MacroCall && has_closer && length(placeholder_inds) > 0
        fst[placeholder_inds[end]] = Whitespace(0)
    end
    idx = findlast(n -> n.typ === PLACEHOLDER, nodes)

    # Check if we should apply conservative nesting rules
    should_nest = line_margin > s.opts.margin || must_nest(fst) || src_diff_line

    # For certain types, be more conservative about nesting
    if should_nest && !must_nest(fst) && !src_diff_line
        total_length = line_margin
        if (
            fst.typ === Call &&
            length(placeholder_inds) <= 5 &&
            total_length <= s.opts.margin + 20
        )
            should_nest = false
        elseif (fst.typ === Binary || fst.typ === Chain) &&
               length(placeholder_inds) <= 6 &&
               total_length <= s.opts.margin + 20
            should_nest = false
        elseif (
            fst.typ === RefN &&
            length(placeholder_inds) <= 4 &&
            total_length <= s.opts.margin + 30
        )
            # Keep array indexing together when reasonable (e.g., du[i, j, 1])
            should_nest = false
        end
    end

    # Override should_nest for LHS tuples that shouldn't be broken
    if is_lhs_tuple && !has_newline
        # For LHS tuples, only nest if they're extremely long
        # This prevents breaking "p_a, p_b" just because the RHS is long
        tuple_length = 0
        for n in nodes
            if n.typ !== PLACEHOLDER
                tuple_length += length(n)
            end
        end
        # Only break if the tuple itself is very long (>80 chars)
        if tuple_length <= 80
            should_nest = false
        end
    end

    if idx !== nothing && should_nest
        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                si = findnext(n -> n.typ === PLACEHOLDER || n.typ === NEWLINE, nodes, i + 1)
                nested2 = nest_if_over_margin!(style, fst, s, i, lineage; stop_idx = si)
                nested |= nested2
                if has_closer && !nested2 && n.startline == fst[end].startline
                    # trailing types are automatically converted, undo this if
                    # there is no nest and the closer is on the same in the
                    # original source.
                    if fst[i-1].typ === TRAILINGCOMMA
                        fst[i-1].val = ""
                        fst[i-1].len = 0
                    end
                end
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nested |= nest!(style, n, s, lineage)
            elseif has_closer && (i == 1 || i == length(nodes))
                nested |= nest!(style, n, s, lineage)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1

                nested |= nest!(style, n, s, lineage)
            end
        end

        if has_closer
            s.line_offset = fst[end].indent + 1
        end
    else
        extra_margin = fst.extra_margin
        if has_closer
            (extra_margin += 1)
        else
            false
        end

        # Special handling for split LHS tuples to ensure proper indentation
        if is_lhs_tuple && has_newline && fst.typ === TupleN
            # Ensure all nodes after newlines get proper indentation
            after_newline = false
            for (i, n) in enumerate(nodes)
                if n.typ === NEWLINE
                    after_newline = true
                    nested |= nest!(style, n, s, lineage)
                    s.line_offset = fst.indent
                elseif after_newline && n.typ !== PLACEHOLDER && n.typ !== WHITESPACE
                    # Add indentation to nodes after newline
                    add_indent!(n, s, fst.indent)
                    nested |= nest!(style, n, s, lineage)
                    s.line_offset += length(n)
                else
                    nested |= nest!(style, n, s, lineage)
                    if n.typ !== NEWLINE
                        s.line_offset += length(n)
                    end
                end
            end
        else
            nested |=
                nest!(style, nodes, s, fst.indent, lineage; extra_margin = extra_margin)
        end
    end

    s.line_offset = start_line_offset
    walk(unnest!(style; dedent = false), fst, s)
    s.line_offset = start_line_offset
    walk(increment_line_offset!, fst, s)

    return nested
end

# Custom n_call! to align arguments with opening parenthesis
function n_call!(
    ss::SciMLStyle,
    fst::FST,
    s::State,
    lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
)
    if s.opts.yas_style_nesting
        # Use YAS style directly
        n_call!(YASStyle(getstyle(ss)), fst, s, lineage)
    else
        # Implement alignment to opening parenthesis
        style = getstyle(ss)
        nodes = fst.nodes::Vector
        
        nested = false
        for (i, n) in enumerate(nodes)
            if is_opener(n)
                # The key: set indent to align with position after opener
                # This is what YAS does
                fst.indent = s.line_offset + length(n)
            end
            
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                si = findnext(n -> n.typ === PLACEHOLDER || n.typ === NEWLINE, nodes, i + 1)
                nested |= nest_if_over_margin!(style, fst, s, i, lineage; stop_idx = si)
            elseif is_gen(n)
                n.indent = fst.indent
                n.extra_margin = 1
                nested |= nest!(style, n, s, lineage)
            else
                diff = fst.indent - n.indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nested |= nest!(style, n, s, lineage)
            end
        end
        
        return nested
    end
end

for f in [
    :n_tuple!,
    :n_curly!,
    :n_macrocall!,
    :n_braces!,
    :n_parameters!,
    :n_invisbrackets!,
    :n_bracescat!,
]
    @eval function $f(
        ss::SciMLStyle,
        fst::FST,
        s::State,
        lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
    )
        if s.opts.yas_style_nesting
            $f(YASStyle(getstyle(ss)), fst, s, lineage)
        else
            # Special case: use YAS style nesting for macro calls to preserve alignment
            if $(Meta.quot(f)) === :n_macrocall!
                $f(YASStyle(getstyle(ss)), fst, s, lineage)
            else
                _n_tuple!(getstyle(ss), fst, s, lineage)
            end
        end
    end
end

# Custom n_ref! to preserve alignment in typed arrays (Issue #935)
function n_ref!(
    ss::SciMLStyle,
    fst::FST,
    s::State,
    lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
)
    # Always use YAS style for typed arrays to preserve alignment
    n_ref!(YASStyle(getstyle(ss)), fst, s, lineage)
end

function n_vect!(
    ss::SciMLStyle,
    fst::FST,
    s::State,
    lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
)
    if s.opts.yas_style_nesting
        # Allow a line break after the opening brackets without aligning
        n_vect!(DefaultStyle(getstyle(ss)), fst, s, lineage)
    else
        _n_tuple!(getstyle(ss), fst, s, lineage)
    end
end

function n_binaryopcall!(
    ss::SciMLStyle,
    fst::FST,
    s::State,
    lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
)
    style = getstyle(ss)
    line_margin = s.line_offset + length(fst) + fst.extra_margin

    # Check if this is an assignment with a tuple LHS
    if !isnothing(fst.metadata) && (fst.metadata::Metadata).is_assignment
        lhs = fst[1]

        # Check if LHS is a tuple that isn't already split
        if lhs.typ === TupleN
            has_newline = false
            for n in lhs.nodes
                if n.typ === NEWLINE
                    has_newline = true
                    break
                end
            end

            if !has_newline
                # Calculate just the LHS length
                lhs_length = length(lhs)

                # Only break after LHS if the LHS itself is very long
                if lhs_length <= 80 && line_margin > s.opts.margin
                    # Don't let the default handler break after a short LHS
                    # Instead, handle the nesting ourselves
                    nodes = fst.nodes::Vector
                    idxs = findall(n -> n.typ === PLACEHOLDER, nodes)

                    if length(idxs) == 2
                        # Keep the first placeholder (after LHS) as whitespace
                        # Only potentially break at the second placeholder (in RHS)
                        nested = false

                        # Nest the LHS without breaking it
                        nested |= nest!(style, lhs, s, lineage)

                        # Handle the operator and RHS
                        for i in 2:length(nodes)
                            if i == idxs[1]
                                # First placeholder - keep as space
                                fst[i] = Whitespace(1)
                                s.line_offset += 1
                            else
                                nested |= nest!(style, fst[i], s, lineage)
                                if fst[i].typ !== NEWLINE
                                    s.line_offset += length(fst[i])
                                else
                                    s.line_offset = fst.indent
                                end
                            end
                        end

                        return nested
                    end
                end
            end
        end
    end

    # Fall back to default behavior
    if s.opts.yas_style_nesting
        n_binaryopcall!(YASStyle(style), fst, s, lineage)
    else
        n_binaryopcall!(DefaultStyle(style), fst, s, lineage)
    end
end

for f in [:n_chainopcall!, :n_comparison!, :n_for!]
    @eval function $f(
        ss::SciMLStyle,
        fst::FST,
        s::State,
        lineage::Vector{Tuple{FNode,Union{Nothing,Metadata}}},
    )
        if s.opts.yas_style_nesting
            $f(YASStyle(getstyle(ss)), fst, s, lineage)
        else
            $f(DefaultStyle(getstyle(ss)), fst, s, lineage)
        end
    end
end
