function n_tuple!(bs::BlueStyle, fst::FST, s::State)
    style = getstyle(bs)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    nodes = fst.nodes::Vector
    lidx = findlast(n -> n.typ === PLACEHOLDER, nodes)
    fidx = findfirst(n -> n.typ === PLACEHOLDER, nodes)
    multiline_arg = findfirst(is_block, nodes) !== nothing
    multiline_arg && (fst.nest_behavior = AlwaysNest)
    has_closer = is_closer(fst[end])

    # "foo(a, b, c)" is true if "foo" and "c" are on different lines
    src_diff_line = if s.opts.join_lines_based_on_source
        last_arg_idx = findlast(is_iterable_arg, nodes)
        last_arg = last_arg_idx === nothing ? fst[end] : fst[last_arg_idx]
        fst[1].endline != last_arg.startline
    else
        false
    end

    if lidx !== nothing && (line_margin > s.opts.margin || must_nest(fst) || src_diff_line)
        fidx = fidx::Int
        args_range = fidx+1:lidx-1
        args_margin = sum(length.(fst[args_range]))

        nest_to_oneline =
            if can_nest(fst) && (fst.indent + s.opts.indent + args_margin <= s.opts.margin)
                !contains_comment(nodes[args_range])
            else
                false
            end

        # nesting is initiated by source code
        src_nest = src_diff_line && line_margin <= s.opts.margin

        args_diff_line = if src_nest
            # first arg
            first_arg_idx = if fst.typ === TupleN && !has_closer
                1
            elseif is_opener(fst[1])
                # (...)
                findfirst(is_iterable_arg, nodes)
            else
                # f(...)
                findnext(is_iterable_arg, nodes, 2)
            end
            first_arg = first_arg_idx === nothing ? fst[1] : fst[first_arg_idx]
            # last arg
            last_arg_idx = findlast(is_iterable_arg, nodes)
            last_arg = last_arg_idx === nothing ? fst[end] : fst[last_arg_idx]

            first_arg.endline != last_arg.startline
        else
            false
        end

        line_offset = s.line_offset
        if has_closer
            fst[end].indent = fst.indent
        end
        if fst.typ !== TupleN || has_closer
            fst.indent += s.opts.indent
        end

        add_trailing_comma = (src_nest && args_diff_line) || (!src_nest && !nest_to_oneline)

        for (i, n) in enumerate(nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif (i == fidx || i == lidx) && !src_diff_line && nest_to_oneline
                fst[i] = Newline(length = n.len)
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
                elseif !nest_to_oneline
                    fst[i] = Newline(length = n.len)
                    s.line_offset = fst.indent
                else
                    nest!(style, n, s)
                end
            elseif n.typ === TRAILINGCOMMA
                if add_trailing_comma ||
                   nodes[i-1].typ === OPERATOR &&
                   nodes[i-1].metadata.op_kind === Tokens.COLON
                    # if add_trailing_comma  || nodes[i-1].typ === OPERATOR && nodes[i-1].val == ":"
                    n.val = ","
                    n.len = 1
                end
                nest!(style, n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ";"
                n.len = 1
                nest!(style, n, s)
            elseif n.typ === INVERSETRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(style, n, s)
            elseif has_closer && (i == 1 || i == length(nodes))
                nest!(style, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin =
                    !nest_to_oneline ? 1 : i < length(nodes) ? length(fst[i+1]) : 0
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
end
n_call!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)
n_curly!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)
n_macrocall!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)
n_ref!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)
n_braces!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)
n_vect!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)
n_parameters!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)
n_invisbrackets!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)
n_bracescat!(bs::BlueStyle, fst::FST, s::State) = n_tuple!(bs, fst, s)

function n_conditionalopcall!(bs::BlueStyle, fst::FST, s::State)
    style = getstyle(bs)
    if fst[end].typ === Conditional
        conditional_to_if_block!(fst, s)
        nest!(style, fst, s)
    else
        n_conditionalopcall!(DefaultStyle(style), fst, s)
    end
    return
end

function n_binaryopcall!(bs::BlueStyle, fst::FST, s::State)
    style = getstyle(bs)
    if fst.ref !== nothing && parent_is(fst.ref[], n -> is_if(n) || n.head === :macrocall)
        n_binaryopcall!(YASStyle(style), fst, s; indent = fst.indent + s.opts.indent)
    else
        n_binaryopcall!(YASStyle(style), fst, s)
    end
    return
end

function n_chainopcall!(bs::BlueStyle, fst::FST, s::State)
    style = getstyle(bs)
    if fst.ref !== nothing && parent_is(fst.ref[], n -> is_if(n) || n.head === :macrocall)
        n_block!(DefaultStyle(style), fst, s; indent = fst.indent + s.opts.indent)
    else
        n_block!(DefaultStyle(style), fst, s; indent = s.line_offset)
    end
    return
end

n_comparison!(bs::BlueStyle, fst::FST, s::State) = n_chainopcall!(bs, fst, s)
