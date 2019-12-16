# Nest
#
# If the line exceeds the print width it will be nested.
#
# This is done by replacing `PLACEHOLDER` nodes with `NEWLINE`
# nodes and updating the PTree's indent.
#
# `extra_margin` provides additional width to consider from
# the top-level node.
#
# Example:
#
#     arg1 op arg2
#
# the length of " op" will be considered when nesting arg1

# unnest, converts newlines to whitespace
unnest!(pt::PTree, ind::Int) = pt[ind] = Whitespace(pt[ind].len)
function unnest!(pt::PTree, nl_inds::Vector{Int})
    for (i, ind) in enumerate(nl_inds)
        unnest!(pt, ind)
        i == length(nl_inds) || continue
        ind2 = ind - 1
        if pt[ind2].typ === TRAILINGCOMMA
            pt[ind2].val = ""
            pt[ind2].len = 0
        elseif pt[ind-1].typ === TRAILINGSEMICOLON
            pt[ind2].val = ";"
            pt[ind2].len = 1
        end
    end
end

function skip_indent(pt::PTree)
    if pt.typ === CSTParser.LITERAL && pt.val == ""
        return true
    elseif pt.typ === NEWLINE || pt.typ === NOTCODE
        return true
    end
    false
end

function walk(f, pt::PTree, s::State)
    f(pt, s)
    is_leaf(pt) && return
    for (i, n) in enumerate(pt.nodes)
        if n.typ === NEWLINE && i < length(pt.nodes)
            if is_closer(pt[i+1])
                s.line_offset = pt[i+1].indent
            elseif !skip_indent(pt[i+1])
                s.line_offset = pt.indent
            end
        else
            walk(f, n, s)
        end
    end
end

function reset_line_offset!(x::PTree, s::State)
    is_leaf(x) || return
    s.line_offset += length(x)
end

function add_indent!(x::PTree, s::State, indent)
    indent == 0 && return
    lo = s.line_offset
    f = (x::PTree, s::State) -> x.indent += indent
    walk(f, x, s)
    s.line_offset = lo
end

function dedent!(x::PTree, s::State)
    if is_leaf(x)
        s.line_offset += length(x)
        is_closer(x) && (x.indent -= s.indent_size)
        return
    end
    x.typ === CSTParser.ConditionalOpCall && return
    x.typ === CSTParser.StringH && return

    # dedent
    x.indent -= s.indent_size
    x.force_nest && return

    nl_inds = findall(n -> n.typ === NEWLINE && !n.force_nest, x.nodes)
    length(nl_inds) > 0 || return
    margin = s.line_offset + x.extra_margin + length(x)
    # @info "" x.typ margin s.line_offset x.extra_margin length(x) length(nl_inds)
    margin <= s.margin || return
    unnest!(x, nl_inds)
end

function nest!(nodes::Vector{PTree}, s::State, indent::Int; extra_margin = 0)
    for (i, n) in enumerate(nodes)
        if n.typ === NEWLINE && nodes[i+1].typ === CSTParser.Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NOTCODE && nodes[i+1].typ === CSTParser.Block
            s.line_offset = nodes[i+1].indent
        elseif n.typ === NEWLINE
            s.line_offset = indent
        else
            n.extra_margin = extra_margin
            nest!(n, s)
        end
    end
end

function nest!(x::PTree, s::State)
    if is_leaf(x)
        s.line_offset += length(x)
        return
    end

    if x.typ === CSTParser.Import
        n_import!(x, s)
    elseif x.typ === CSTParser.Export
        n_import!(x, s)
    elseif x.typ === CSTParser.Using
        n_import!(x, s)
    elseif x.typ === CSTParser.WhereOpCall
        n_wherecall!(x, s)
    elseif x.typ === CSTParser.ConditionalOpCall
        n_condcall!(x, s)
    elseif x.typ === CSTParser.BinaryOpCall
        n_binarycall!(x, s)
    elseif x.typ === CSTParser.Curly
        n_call!(x, s)
    elseif x.typ === CSTParser.Call
        n_call!(x, s)
    elseif x.typ === CSTParser.MacroCall
        n_call!(x, s)
    elseif x.typ === CSTParser.Ref
        n_call!(x, s)
    elseif x.typ === CSTParser.TypedVcat
        n_call!(x, s)
    elseif x.typ === CSTParser.TupleH
        n_tuple!(x, s)
    elseif x.typ === CSTParser.Vect
        n_tuple!(x, s)
    elseif x.typ === CSTParser.Vcat
        n_tuple!(x, s)
    elseif x.typ === CSTParser.Parameters
        n_tuple!(x, s)
    elseif x.typ === CSTParser.Braces
        n_tuple!(x, s)
    elseif x.typ === CSTParser.InvisBrackets
        n_tuple!(x, s)
    elseif x.typ === CSTParser.Comprehension
        n_tuple!(x, s)
    elseif x.typ === CSTParser.Do
        n_do!(x, s)
    elseif x.typ === CSTParser.Generator
        n_gen!(x, s)
    elseif x.typ === CSTParser.Filter
        n_gen!(x, s)
    elseif x.typ === CSTParser.Block
        n_block!(x, s)
    elseif x.typ === CSTParser.ChainOpCall
        n_block!(x, s)
    elseif x.typ === CSTParser.Comparison
        n_block!(x, s)
    elseif x.typ === CSTParser.For
        n_for!(x, s)
    elseif x.typ === CSTParser.Let
        n_for!(x, s)
    elseif x.typ === CSTParser.UnaryOpCall && x[2].typ === CSTParser.OPERATOR
        x[1].extra_margin = x.extra_margin + length(x[2])
        nest!(x[1], s)
        nest!(x[2], s)
    else
        nest!(x.nodes, s, x.indent, extra_margin = x.extra_margin)
    end
end

function n_do!(x, s)
    extra_margin = sum(length.(x[2:3]))
    # make sure there are nodes after "do"
    if x[4].typ === WHITESPACE
        extra_margin += length(x[4])
        extra_margin += length(x[5])
    end
    x[1].extra_margin = x.extra_margin + extra_margin
    nest!(x[1], s)
    nest!(x[2:end], s, x.indent, extra_margin = x.extra_margin)
end


# Import,Using,Export,ImportAll
function n_import!(x, s)
    line_margin = s.line_offset + length(x) + x.extra_margin
    idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
    if idx !== nothing && (line_margin > s.margin || x.force_nest)
        x.indent += s.indent_size

        if !x.force_nest
            if x.indent + sum(length.(x[idx+1:end])) <= s.margin
                x[idx] = Newline(length = x[idx].len)
                walk(reset_line_offset!, x, s)
                return
            end
        end

        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x[i] = Newline(length = n.len)
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end
    else
        nest!(x.nodes, s, x.indent, extra_margin = x.extra_margin)
    end
end

function n_tuple!(x, s)
    line_margin = s.line_offset + length(x) + x.extra_margin
    idx = findlast(n -> n.typ === PLACEHOLDER, x.nodes)
    # @info "ENTERING" idx x.typ s.line_offset length(x) x.extra_margin
    opener = length(x.nodes) > 0 ? is_opener(x[1]) : false
    if idx !== nothing && (line_margin > s.margin || x.force_nest)
        line_offset = s.line_offset
        if opener
            x[end].indent = x.indent
        end
        if x.typ !== CSTParser.Parameters && !(x.typ === CSTParser.TupleH && !opener)
            x.indent += s.indent_size
        end

        # @info "DURING" x.typ x.indent s.line_offset
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x[i] = Newline(length = n.len)
                s.line_offset = x.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(n, s)
            elseif opener && (i == 1 || i == length(x.nodes))
                nest!(n, s)
            else
                diff = x.indent - x[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(n, s)
            end
        end

        if opener
            s.line_offset = x[end].indent + 1
        end
        # @info "EXITING" x.typ s.line_offset x.indent x[end].indent
    else
        extra_margin = x.extra_margin
        opener && (extra_margin += 1)
        nest!(x.nodes, s, x.indent, extra_margin = extra_margin)
    end
end


function n_call!(x, s)
    line_margin = s.line_offset + length(x) + x.extra_margin
    idx = findlast(n -> n.typ === PLACEHOLDER, x.nodes)
    # @info "ENTERING" x.typ s.line_offset length(x) x.extra_margin s.margin x[1].val
    if idx !== nothing && (line_margin > s.margin || x.force_nest)
        line_offset = s.line_offset
        x[end].indent = x.indent
        x.indent += s.indent_size

        # @info "DURING" x.typ x.indent s.line_offset
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x[i] = Newline(length = n.len)
                s.line_offset = x.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(n, s)
            elseif i == 1 || i == length(x.nodes)
                n.typ === CSTParser.Parameters && (n.force_nest = true)
                n.extra_margin = 1
                nest!(n, s)
            else
                diff = x.indent - x[i].indent
                add_indent!(n, s, diff)
                n.typ === CSTParser.Parameters && (n.force_nest = true)
                n.extra_margin = 1
                nest!(n, s)
            end
        end

        s.line_offset = x[end].indent + 1
    else
        extra_margin = x.extra_margin
        is_closer(x[end]) && (extra_margin += 1)
        nest!(x.nodes, s, x.indent, extra_margin = extra_margin)
    end
end

# "A where B"
function n_wherecall!(x, s)
    line_margin = s.line_offset + length(x) + x.extra_margin
    # @info "" s.line_offset x.typ line_margin x.extra_margin length(x) x.force_nest line_margin > s.margin
    if line_margin > s.margin || x.force_nest
        line_offset = s.line_offset
        # after "A where "
        idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
        Blen = sum(length.(x[idx+1:end]))

        # A, +7 is the length of " where "
        x[1].extra_margin = Blen + 7 + x.extra_margin
        nest!(x[1], s)

        for (i, n) in enumerate(x[2:idx-1])
            nest!(n, s)
        end

        # "B"

        has_braces = is_closer(x[end])
        if has_braces
            x[end].indent = x.indent
        end

        over = (s.line_offset + Blen + x.extra_margin > s.margin) || x.force_nest
        x.indent += s.indent_size

        for (i, n) in enumerate(x[idx+1:end])
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif is_opener(n)
                if x.indent - s.line_offset > 1
                    x.indent = s.line_offset + 1
                    x[end].indent = s.line_offset
                end
                nest!(n, s)
            elseif n.typ === PLACEHOLDER && over
                x[i+idx] = Newline(length = n.len)
                s.line_offset = x.indent
            elseif n.typ === TRAILINGCOMMA && over
                n.val = ","
                n.len = 1
                nest!(n, s)
            elseif has_braces
                n.extra_margin = 1 + x.extra_margin
                nest!(n, s)
            else
                n.extra_margin = x.extra_margin
                nest!(n, s)
            end
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, x, s)
    else
        nest!(x.nodes, s, x.indent, extra_margin = x.extra_margin)
    end
end

function n_condcall!(x, s)
    line_margin = s.line_offset + length(x) + x.extra_margin
    # @info "ENTERING" x.typ s.line_offset line_margin x.force_nest
    if line_margin > s.margin || x.force_nest
        line_offset = s.line_offset
        x.indent = s.line_offset
        phs = reverse(findall(n -> n.typ === PLACEHOLDER, x.nodes))
        for (i, idx) in enumerate(phs)
            if i == 1
                x[idx] = Newline(length = x[idx].len)
            else
                nidx = phs[i-1]
                l1 = sum(length.(x[1:idx-1]))
                l2 = sum(length.(x[idx:nidx-1]))
                width = line_offset + l1 + l2
                if x.force_nest || width > s.margin
                    x[idx] = Newline(length = x[idx].len)
                end
            end
        end

        for (i, n) in enumerate(x.nodes)
            if i == length(x.nodes)
                n.extra_margin = x.extra_margin
                nest!(n, s)
            elseif x[i+1].typ === WHITESPACE
                n.extra_margin = length(x[i+1]) + length(x[i+2])
                nest!(n, s)
            elseif n.typ === NEWLINE
                s.line_offset = x.indent
            else
                nest!(n, s)
            end
        end

        s.line_offset = line_offset
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE && !is_comment(x[i+1]) && !is_comment(x[i-1])
                # +1 for newline to whitespace conversion
                width = s.line_offset + 1
                if i == length(x.nodes) - 1
                    width += sum(length.(x[i+1:end])) + x.extra_margin
                else
                    width += sum(length.(x[i+1:i+3]))
                end
                # @debug "" s.line_offset l  s.margin
                if width <= s.margin
                    x[i] = Whitespace(1)
                else
                    s.line_offset = x.indent
                end
            end
            s.line_offset += length(x[i])
        end

        s.line_offset = line_offset
        walk(reset_line_offset!, x, s)
    else
        nest!(x.nodes, s, x.indent, extra_margin = x.extra_margin)
    end
end



# arg1 op arg2
#
# nest in order of
#
# arg1 op
# arg2
function n_binarycall!(x, s)
    # If there's no placeholder the binary call is not nestable
    idxs = findall(n -> n.typ === PLACEHOLDER, x.nodes)
    line_margin = s.line_offset + length(x) + x.extra_margin
    # @info "ENTERING" x.typ x.extra_margin s.line_offset length(x) idxs x.ref[][2]
    if length(idxs) == 2 && (line_margin > s.margin || x.force_nest)
        line_offset = s.line_offset
        i1 = idxs[1]
        i2 = idxs[2]
        x[i1] = Newline(length = x[i1].len)
        cst = x.ref[]

        has_eq = CSTParser.defines_function(cst) || nest_assignment(cst)
        has_arrow = cst[2].kind === Tokens.PAIR_ARROW || cst[2].kind === Tokens.ANON_FUNC
        indent_nest = has_eq || has_arrow

        if indent_nest
            s.line_offset = x.indent + s.indent_size
            x[i2] = Whitespace(s.indent_size)
            add_indent!(x[end], s, s.indent_size)
        else
            x.indent = s.line_offset
        end

        # arg2
        x[end].extra_margin = x.extra_margin
        nest!(x[end], s)

        # "arg1 op" arg2
        s.line_offset = line_offset

        # extra margin for " op"
        x[1].extra_margin = length(x[2]) + length(x[3])
        nest!(x[1], s)
        for n in x[2:i1]
            nest!(n, s)
        end

        # Undo nest if possible
        if !x.force_nest
            arg2 = x[end]
            arg2.typ === CSTParser.Block && (arg2 = arg2[1])
            cst = arg2.ref[]

            line_margin = s.line_offset
            if (
                arg2.typ === CSTParser.BinaryOpCall &&
                (!(is_lazy_op(cst) && !indent_nest) && cst[2].kind !== Tokens.IN)
            ) || arg2.typ === CSTParser.UnaryOpCall ||
               arg2.typ === CSTParser.ChainOpCall || arg2.typ === CSTParser.Comparison
                line_margin += length(x[end])
            elseif arg2.typ === CSTParser.Do && is_iterable(arg2[1])
                rw, _ = length_to(x, [NEWLINE], start = i2 + 1)
                line_margin += rw
            elseif is_block(cst)
                idx = findfirst(n -> n.typ === NEWLINE, arg2.nodes)
                if idx === nothing
                    line_margin += length(x[end])
                else
                    line_margin += sum(length.(arg2[1:idx-1]))
                end
            else
                rw, _ = length_to(x, [NEWLINE], start = i2 + 1)
                line_margin += rw
            end

            # @info "" arg2.typ indent_nest s.line_offset line_margin x.extra_margin length(x[end])
            if line_margin + x.extra_margin <= s.margin
                x[i1] = Whitespace(1)
                if indent_nest
                    x[i2] = Whitespace(0)
                    walk(dedent!, arg2, s)
                end
            end
        end

        # @info "before reset" line_offset s.line_offset
        s.line_offset = line_offset
        walk(reset_line_offset!, x, s)
        # @info "after reset" line_offset s.line_offset
    else
        # Handles the case of a function def defined
        # as "foo(a)::R where {A,B} = body".
        #
        # In this case instead of it being parsed as:
        #
        # CSTParser.BinaryOpCall
        #  - CSTParser.WhereOpCall
        #  - OP
        #  - arg2
        #
        # It's parsed as:
        #
        # CSTParser.BinaryOpCall
        #  - CSTParser.BinaryOpCall
        #   - arg1
        #   - OP
        #   - CSTParser.WhereOpCall
        #    - R
        #    - ...
        #  - OP
        #  - arg2
        #
        # The result being extra width is trickier to deal with.
        idx = findfirst(n -> n.typ === CSTParser.WhereOpCall, x.nodes)
        return_width = 0
        if idx !== nothing && idx > 1
            return_width = length(x[idx]) + length(x[2])
        elseif idx === nothing
            return_width, _ = length_to(x, [NEWLINE], start = 2)
        end

        # @info "" idx map(n -> n.typ, x.nodes)
        # @info "" s.line_offset length(x[1]) return_width x.extra_margin

        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif i == 1
                n.extra_margin = return_width + x.extra_margin
                nest!(n, s)
            elseif i == idx
                n.extra_margin = x.extra_margin
                nest!(n, s)
            else
                n.extra_margin = x.extra_margin
                nest!(n, s)
            end
        end
    end
end

function n_for!(x, s)
    block_idx = findfirst(n -> !is_leaf(n), x.nodes)
    if block_idx === nothing
        nest!(x.nodes, s, x.indent, extra_margin = x.extra_margin)
        return
    end
    ph_idx = findfirst(n -> n.typ === PLACEHOLDER, x[block_idx].nodes)
    nest!(x.nodes, s, x.indent, extra_margin = x.extra_margin)

    # return if the argument block was nested
    ph_idx !== nothing && x[3][ph_idx].typ === NEWLINE && return

    idx = 5
    n = x[idx]
    if n.typ === NOTCODE && n.startline == n.endline
        res = get(s.doc.comments, n.startline, (0, ""))
        res == (0, "") && deleteat!(x.nodes, idx - 1)
    end
end

function n_block!(x, s; custom_indent = 0)
    line_margin = s.line_offset + length(x) + x.extra_margin
    idx = findfirst(n -> n.typ === PLACEHOLDER, x.nodes)
    # @info "ENTERING" idx x.typ s.line_offset length(x) x.extra_margin
    if idx !== nothing && (line_margin > s.margin || x.force_nest)
        line_offset = s.line_offset

        x.indent = custom_indent > 0 ? custom_indent : s.line_offset

        # @info "DURING" x.indent s.line_offset x.typ
        for (i, n) in enumerate(x.nodes)
            if n.typ === NEWLINE
                s.line_offset = x.indent
            elseif n.typ === PLACEHOLDER
                x[i] = Newline(length = n.len)
                s.line_offset = x.indent
            elseif n.typ === TRAILINGCOMMA
                n.val = ","
                n.len = 1
                nest!(n, s)
            elseif n.typ === TRAILINGSEMICOLON
                n.val = ""
                n.len = 0
                nest!(n, s)
            else
                diff = x.indent - x[i].indent
                add_indent!(n, s, diff)
                n.extra_margin = 1
                nest!(n, s)
            end
        end

        # @info "EXITING" x.typ s.line_offset x.indent x[end].indent
    else
        nest!(x.nodes, s, x.indent, extra_margin = x.extra_margin)
    end
end

n_gen!(x, s) = n_block!(x, s, custom_indent = x.indent)
