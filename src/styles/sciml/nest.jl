for f in [
    :n_import!,
    :n_using!,
    :n_export!,
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
    @eval function $f(ss::SciMLStyle, fst::FST, s::State; kwargs...)
        $f(YASStyle(getstyle(ss)), fst, s; kwargs...)
    end
end

# function n_binaryopcall!(ss::SciMLStyle, fst::FST, s::State; indent::Int = -1,
# @nospecialize(kwargs...))
#     style = getstyle(ss)
#     line_margin = s.line_offset + length(fst) + fst.extra_margin
#     if line_margin > s.opts.margin && !isnothing(fst.metadata) && fst.metadata.is_short_form_function
#         transformed = short_to_long_function_def!(fst, s)
#         transformed && nest!(style, fst, s; kwargs...)
#         return
#     end
#
#     if findfirst(n -> n.typ === PLACEHOLDER, fst.nodes) !== nothing
#         n_binaryopcall!(DefaultStyle(style), fst, s; kwargs..., indent = indent)
#         return
#     end
#
#     start_line_offset = s.line_offset
#     walk(increment_line_offset!, (fst.nodes::Vector)[1:end-1], s, fst.indent)
#     nest!(style, fst[end], s; kwargs...)
# end

function n_functiondef!(ss::SciMLStyle, fst::FST, s::State; @nospecialize(kwargs...))
    style = getstyle(ss)
    nested = false
    if s.opts.yas_style_nesting
        nested |= nest!(
            YASStyle(style),
            fst.nodes::Vector,
            s,
            fst.indent;
            kwargs...,
            extra_margin = fst.extra_margin,
        )
    else
        base_indent = fst.indent
        add_indent!(fst[3], s, s.opts.indent)

        nested |= nest!(
            ss,
            fst.nodes::Vector,
            s,
            fst.indent;
            kwargs...,
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

function n_macro!(ss::SciMLStyle, fst::FST, s::State; @nospecialize(kwargs...))
    n_functiondef!(ss, fst, s; kwargs...)
end

function _n_tuple!(ss::SciMLStyle, fst::FST, s::State; @nospecialize(kwargs...))
    style = getstyle(ss)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    nodes = fst.nodes::Vector
    has_closer = is_closer(fst[end])
    start_line_offset = s.line_offset

    if has_closer
        fst[end].indent = fst.indent
    end
    if !(fst.typ in (TupleN, CartesianIterator, Parameters)) || has_closer
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

    nested = false
    optimal_placeholders = find_optimal_nest_placeholders(fst, fst.indent, s.opts.margin)
    if length(optimal_placeholders) > 0
        nested = true
    end

    for i in optimal_placeholders
        fst[i] = Newline(length = fst[i].len)
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

    if idx !== nothing && (line_margin > s.opts.margin || must_nest(fst) || src_diff_line)
        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                si = findnext(n -> n.typ === PLACEHOLDER || n.typ === NEWLINE, nodes, i + 1)
                nested2 = nest_if_over_margin!(style, fst, s, i; stop_idx = si, kwargs...)
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
                nested |= nest!(style, n, s; kwargs...)
            elseif has_closer && (i == 1 || i == length(nodes))
                nested |= nest!(style, n, s; kwargs...)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1

                nested |= nest!(style, n, s; kwargs...)
            end
        end

        if has_closer
            s.line_offset = fst[end].indent + 1
        end
    else
        extra_margin = fst.extra_margin
        has_closer && (extra_margin += 1)
        nested |= nest!(style, nodes, s, fst.indent; kwargs..., extra_margin = extra_margin)
    end

    s.line_offset = start_line_offset
    walk(unnest!(style; dedent = false), fst, s)
    s.line_offset = start_line_offset
    walk(increment_line_offset!, fst, s)

    return nested
end

for f in [
    :n_tuple!,
    :n_call!,
    :n_curly!,
    :n_macrocall!,
    :n_ref!,
    :n_braces!,
    :n_parameters!,
    :n_invisbrackets!,
    :n_bracescat!,
]
    @eval function $f(ss::SciMLStyle, fst::FST, s::State; @nospecialize(kwargs...))
        if s.opts.yas_style_nesting
            $f(YASStyle(getstyle(ss)), fst, s; kwargs...)
        else
            _n_tuple!(getstyle(ss), fst, s; kwargs...)
        end
    end
end

function n_vect!(ss::SciMLStyle, fst::FST, s::State; @nospecialize(kwargs...))
    if s.opts.yas_style_nesting
        # Allow a line break after the opening brackets without aligning
        n_vect!(DefaultStyle(getstyle(ss)), fst, s; kwargs...)
    else
        _n_tuple!(getstyle(ss), fst, s; kwargs...)
    end
end

for f in [:n_chainopcall!, :n_comparison!, :n_for!]
    @eval function $f(ss::SciMLStyle, fst::FST, s::State; @nospecialize(kwargs...))
        if s.opts.yas_style_nesting
            $f(YASStyle(getstyle(ss)), fst, s; kwargs...)
        else
            $f(DefaultStyle(getstyle(ss)), fst, s; kwargs...)
        end
    end
end
