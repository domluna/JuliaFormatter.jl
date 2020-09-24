function n_tupleh!(bs::BlueStyle, fst::FST, s::State)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    lidx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes)
    fidx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
    opener = findfirst(is_opener, fst.nodes) !== nothing

    if lidx !== nothing && (line_margin > s.opts.margin || must_nest(fst))
        fidx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)
        args_range = fidx+1:lidx-1
        args_margin = sum(length.(fst[args_range]))

        nest_to_oneline =
            (fst.indent + s.opts.indent + args_margin <= s.opts.margin) &&
            !contains_comment(fst.nodes[args_range])

        # @info "" nest_to_oneline fst.indent fst.indent + s.opts.indent + args_margin args_margin

        line_offset = s.line_offset
        if opener
            fst[end].indent = fst.indent
        end
        if fst.typ !== CSTParser.TupleH || opener
            fst.indent += s.opts.indent
        end

        for (i, n) in enumerate(fst.nodes)
            if n.typ === NEWLINE
                s.line_offset = fst.indent
            elseif nest_to_oneline && (i == fidx || i == lidx)
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif !nest_to_oneline && n.typ === PLACEHOLDER
                fst[i] = Newline(length = n.len)
                s.line_offset = fst.indent
            elseif n.typ === TRAILINGCOMMA
                if !nest_to_oneline
                    n.val = ","
                    n.len = 1
                end
                nest!(bs, n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ";"
                n.len = 1
                nest!(bs, n, s)
            elseif n.typ === INVERSETRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(bs, n, s)
            elseif opener && (i == 1 || i == length(fst.nodes))
                nest!(bs, n, s)
            else
                diff = fst.indent - fst[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(bs, n, s)
            end
        end

        if opener
            s.line_offset = fst[end].indent + 1
        end
    else
        extra_margin = fst.extra_margin
        opener && (extra_margin += 1)
        nest!(bs, fst.nodes, s, fst.indent, extra_margin = extra_margin)
    end
end
@inline n_call!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
@inline n_curly!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
@inline n_macrocall!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
@inline n_ref!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
@inline n_braces!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
@inline n_vect!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
@inline n_parameters!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
@inline n_invisbrackets!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
@inline n_bracescat!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
# @inline n_vcat!(bs::BlueStyle, fst::FST, s::State) = n_tupleh!(bs, fst, s)
