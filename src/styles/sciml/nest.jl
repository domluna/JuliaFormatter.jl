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
    :n_flatten!,
]
    @eval function $f(ss::SciMLStyle, fst::FST, s::State)
        style = getstyle(ss)
        $f(YASStyle(style), fst, s)
    end
end

function n_binaryopcall!(ss::SciMLStyle, fst::FST, s::State; indent::Int = -1)
    style = getstyle(ss)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    if line_margin > s.opts.margin &&
       fst.ref !== nothing &&
       CSTParser.defines_function(fst.ref[])
        transformed = short_to_long_function_def!(fst, s)
        transformed && nest!(style, fst, s)
    end

    if findfirst(n -> n.typ === PLACEHOLDER, fst.nodes) !== nothing
        n_binaryopcall!(DefaultStyle(style), fst, s; indent = indent)
        return
    end

    start_line_offset = s.line_offset
    walk(increment_line_offset!, (fst.nodes::Vector)[1:end-1], s, fst.indent)
    nest!(style, fst[end], s)
end

function n_functiondef!(ss::SciMLStyle, fst::FST, s::State)
    style = getstyle(ss)
    if s.opts.yas_style_nesting
        nest!(
            YASStyle(style),
            fst.nodes::Vector,
            s,
            fst.indent,
            extra_margin = fst.extra_margin,
        )
    else
        nest!(
            DefaultStyle(style),
            fst.nodes::Vector,
            s,
            fst.indent,
            extra_margin = fst.extra_margin,
        )

        # base_indent = fst.indent
        # closers = FST[]
        # f = (fst::FST, s::State) -> begin
        #     if is_closer(fst) && fst.indent == base_indent
        #         push!(closers, fst)
        #     end
        #     fst.indent += s.opts.indent
        #     return nothing
        # end
        # lo = s.line_offset
        # walk(f, fst[3], s)
        # s.line_offset = lo
        # for c in closers
        #     c.indent -= s.opts.indent
        # end
    end
end

function n_macro!(ss::SciMLStyle, fst::FST, s::State)
    n_functiondef!(ss, fst, s)
end

function _n_tuple!(ss::SciMLStyle, fst::FST, s::State)
    style = getstyle(ss)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    nodes = fst.nodes::Vector
    idx = findlast(n -> n.typ === PLACEHOLDER, nodes)
    has_closer = is_closer(fst[end])
    start_line_offset = s.line_offset

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

    optimal_placeholders = find_optimal_placeholders_nest(fst, start_line_offset, s.opts.margin)

    for i in optimal_placeholders
        fst[i] = Newline(length = fst[i].len)
    end

    placeholder_inds = findall(n -> n.typ === PLACEHOLDER, fst.nodes)
    for (i, ph) in enumerate(placeholder_inds)
        if i == 1 || i == length(placeholder_inds)
            continue
        end
        fst[ph] = Whitespace(fst[ph].len)
    end
    placeholder_inds = findall(n -> n.typ === PLACEHOLDER, fst.nodes)
    @info "nesting placeholders" placeholder_inds

    if idx !== nothing && (line_margin > s.opts.margin || must_nest(fst) || src_diff_line)
        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif n.typ === PLACEHOLDER
                si = findnext(n -> n.typ === PLACEHOLDER || n.typ === NEWLINE, nodes, i + 1)
                nested = nest_if_over_margin!(style, fst, s, i; stop_idx = si)
                if has_closer && !nested && n.startline == fst[end].startline
                    # trailing types are automatically converted, undo this if
                    # there is no nest and the closer is on the same in the
                    # original source.
                    if fst[i-1].typ === TRAILINGCOMMA || fst[i-1].typ === TRAILINGSEMICOLON
                        fst[i-1].val = ""
                        fst[i-1].len = 0
                    end
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

    walk(unnest!(style), fst, s)

    s.line_offset = start_line_offset
    walk(increment_line_offset!, fst, s)

    return
end

for f in [
    :n_tuple!,
    :n_call!,
    :n_curly!,
    :n_macrocall!,
    :n_ref!,
    :n_vect!,
    :n_braces!,
    :n_parameters!,
    :n_invisbrackets!,
    :n_bracescat!,
]
    @eval function $f(ss::SciMLStyle, fst::FST, s::State)
        style = getstyle(ss)
        if s.opts.yas_style_nesting
            $f(YASStyle(style), fst, s)
        else
            _n_tuple!(style, fst, s)
        end
    end
end

for f in [
    :n_chainopcall!,
    :n_comparison!,
    :n_for!,
]
    @eval function $f(ss::SciMLStyle, fst::FST, s::State)
        style = getstyle(ss)
        if s.opts.yas_style_nesting
            $f(YASStyle(style), fst, s)
        else
            $f(DefaultStyle(style), fst, s)
        end
    end
end
