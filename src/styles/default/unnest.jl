function dedent!(ds::DefaultStyle, fst::FST, s::State)
    fst.typ === CSTParser.StringH && return
    fst.indent -= s.opts.indent
end

function unnest!(
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
            unnest!(style, n, s)
        end
    end
end
unnest!(
    style::S,
    nodes::Vector{FST},
    s::State,
    indent::Int;
    extra_margin = 0,
) where {S<:AbstractStyle} =
    unnest!(DefaultStyle(style), nodes, s, indent, extra_margin = extra_margin)

function unnest!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)

    if is_leaf(fst)
        s.line_offset += length(fst)
        if is_closer(fst) || fst.typ === NOTCODE
            fst.indent -= s.opts.indent
        end
        return
    end

    # only unnest if it's allowed
    # can_nest(fst) || return
    if !can_nest(fst)
        walk(increment_line_offset!, fst, s)
        return
    end

    if fst.typ === CSTParser.Import
        un_import!(style, fst, s)
    elseif fst.typ === CSTParser.Export
        un_export!(style, fst, s)
    elseif fst.typ === CSTParser.Using
        un_using!(style, fst, s)
    elseif fst.typ === CSTParser.WhereOpCall
        un_whereopcall!(style, fst, s)
    elseif fst.typ === CSTParser.ConditionalOpCall
        line_margin = s.line_offset + length(fst) + fst.extra_margin
        if s.opts.conditional_to_if && line_margin > s.opts.margin
            conditional_to_if_block!(fst, s)
            unnest!(style, fst, s)
        else
            un_conditionalopcall!(style, fst, s)
        end
    elseif fst.typ === CSTParser.BinaryOpCall
        line_margin = s.line_offset + length(fst) + fst.extra_margin
        if s.opts.short_to_long_function_def &&
           line_margin > s.opts.margin &&
           fst.ref !== nothing &&
           CSTParser.defines_function(fst.ref[])
            short_to_long_function_def!(fst, s)
        end
        if fst.typ === CSTParser.BinaryOpCall
            un_binaryopcall!(style, fst, s)
        else
            unnest!(style, fst, s)
        end
    elseif fst.typ === CSTParser.Curly
        un_curly!(style, fst, s)
    elseif fst.typ === CSTParser.Call
        un_call!(style, fst, s)
    elseif fst.typ === CSTParser.MacroCall
        un_macrocall!(style, fst, s)
    elseif fst.typ === CSTParser.Ref
        un_ref!(style, fst, s)
    elseif fst.typ === CSTParser.TypedVcat
        un_typedvcat!(style, fst, s)
    elseif fst.typ === CSTParser.TupleH
        un_tupleh!(style, fst, s)
    elseif fst.typ === CSTParser.Vect
        un_vect!(style, fst, s)
    elseif fst.typ === CSTParser.Vcat
        un_vcat!(style, fst, s)
    elseif fst.typ === CSTParser.Braces
        un_braces!(style, fst, s)
    elseif fst.typ === CSTParser.BracesCat
        un_bracescat!(style, fst, s)
    elseif fst.typ === CSTParser.InvisBrackets
        un_invisbrackets!(style, fst, s)
    elseif fst.typ === CSTParser.Comprehension
        un_comprehension!(style, fst, s)
    elseif fst.typ === CSTParser.TypedComprehension
        un_typedcomprehension!(style, fst, s)
    elseif fst.typ === CSTParser.Do
        un_do!(style, fst, s)
    elseif fst.typ === CSTParser.Generator
        un_generator!(style, fst, s)
    elseif fst.typ === CSTParser.Filter
        un_filter!(style, fst, s)
    elseif fst.typ === CSTParser.Flatten
        un_flatten!(style, fst, s)
    elseif fst.typ === CSTParser.Block
        un_block!(style, fst, s)
    elseif fst.typ === CSTParser.ChainOpCall
        un_chainopcall!(style, fst, s)
    elseif fst.typ === CSTParser.Comparison
        un_comparison!(style, fst, s)
    elseif fst.typ === CSTParser.For
        un_for!(style, fst, s)
    elseif fst.typ === CSTParser.Let
        un_let!(style, fst, s)
    elseif fst.typ === CSTParser.UnaryOpCall && fst[2].typ === CSTParser.OPERATOR
        un_unaryopcall!(style, fst, s)
    else
        unnest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
    end

    dedent!(style, fst, s)
end

unnest!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    unnest!(DefaultStyle(style), fst, s)

function un_binaryopcall!(ds::DefaultStyle, fst::FST, s::State)
    style = getstyle(ds)
    rhs = fst[end]
    rhs.typ === CSTParser.Block && (rhs = rhs[1])
    idxs = findall(n -> n.typ === PLACEHOLDER, fst.nodes)
    i1 = idxs[1]
    i2 = idxs[2]

    # Undo nest if possible
    if can_nest(fst) && !no_unnest(rhs)
        cst = rhs.ref[]
        line_margin = s.line_offset

        if (rhs.typ === CSTParser.BinaryOpCall && cst[2].kind !== Tokens.IN) ||
           rhs.typ === CSTParser.UnaryOpCall ||
           rhs.typ === CSTParser.ChainOpCall ||
           rhs.typ === CSTParser.Comparison ||
           rhs.typ === CSTParser.ConditionalOpCall
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

        if line_margin + fst.extra_margin <= s.opts.margin
            fst[i1] = Whitespace(1)
            if indent_nest
                fst[i2] = Whitespace(0)
                # walk(dedent!, rhs, s)
                unnest!(style, rhs, s)
            end
        end
    end

    s.line_offset = line_offset
    walk(increment_line_offset!, fst, s)
end
un_binaryopcall!(style::S, fst::FST, s::State) where {S<:AbstractStyle} =
    un_binaryopcall!(DefaultStyle(style), fst, s)
