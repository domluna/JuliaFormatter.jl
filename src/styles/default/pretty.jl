function pretty(
    ds::DefaultStyle,
    t::JuliaSyntax.GreenNode,
    s::State;
    lineage = Tuple{JuliaSyntax.Kind,Bool,Bool}[],
    kwargs...,
)
    k = kind(t)
    style = getstyle(ds)

    push!(lineage, (k, is_iterable(t), is_assignment(t)))
    kwargs = (; kwargs..., lineage = lineage)

    ret = if k == K"Identifier" && !haschildren(t)
        p_identifier(style, t, s; kwargs...)
    elseif JuliaSyntax.is_operator(t) && !haschildren(t)
        p_operator(style, t, s; kwargs...)
    elseif kind(t) == K"Comment"
        p_comment(style, t, s; kwargs...)
    elseif JuliaSyntax.is_whitespace(t)
        p_whitespace(style, t, s; kwargs...)
    elseif kind(t) == K";"
        p_semicolon(style, t, s; kwargs...)
    elseif is_punc(t) && !haschildren(t)
        p_punctuation(style, t, s; kwargs...)
    elseif JuliaSyntax.is_keyword(t) && !haschildren(t)
        p_keyword(style, t, s; kwargs...)
    elseif k == K"as"
        p_as(style, t, s; kwargs...)
    elseif k in KSet"string cmdstring char"
        p_stringh(style, t, s; kwargs...)
    elseif JuliaSyntax.is_literal(t) || k in KSet"\" \"\"\" ` ```"
        p_literal(style, t, s; kwargs...)
    elseif k === K"." && haschildren(t)
        p_accessor(style, t, s; kwargs...)
    elseif k === K"block" && length(children(t)) > 1 && kind(t[1]) === K"begin"
        p_begin(style, t, s; kwargs...)
    elseif k === K"block"
        p_block(style, t, s; kwargs...)
    elseif k === K"module"
        p_module(style, t, s; kwargs...)
    elseif k === K"baremodule"
        p_baremodule(style, t, s; kwargs...)
    elseif k === K"function"
        p_functiondef(style, t, s; kwargs...)
    elseif k in KSet"MacroName StringMacroName CmdMacroName core_@cmd"
        p_macroname(style, t, s; kwargs...)
    elseif k === K"macro"
        p_macro(style, t, s; kwargs...)
    elseif k === K"struct" && !JuliaSyntax.has_flags(t, JuliaSyntax.MUTABLE_FLAG)
        p_struct(style, t, s; kwargs...)
    elseif k === K"struct" && JuliaSyntax.has_flags(t, JuliaSyntax.MUTABLE_FLAG)
        p_mutable(style, t, s; kwargs...)
    elseif k === K"abstract"
        p_abstract(style, t, s; kwargs...)
    elseif k === K"primitive"
        p_primitive(style, t, s; kwargs...)
    elseif k === K"for"
        p_for(style, t, s; kwargs...)
    elseif k === K"while"
        p_while(style, t, s; kwargs...)
    elseif k === K"do"
        p_do(style, t, s; kwargs...)
    elseif k === K"var"
        p_var(style, t, s; kwargs...)
    elseif is_if(t)
        p_if(style, t, s; kwargs...)
    elseif is_try(t)
        p_try(style, t, s; kwargs...)
    elseif k === K"toplevel"
        p_toplevel(style, t, s; kwargs...)
    elseif k === K"quote" && haschildren(t) && kind(t[1]) === K":"
        p_quotenode(style, t, s; kwargs...)
    elseif k === K"quote" && haschildren(t)
        p_quote(style, t, s; kwargs...)
    elseif k === K"let"
        p_let(style, t, s; kwargs...)
    elseif k === K"vect"
        p_vect(style, t, s; kwargs...)
    elseif k === K"comprehension"
        p_comprehension(style, t, s; kwargs...)
    elseif k === K"typed_comprehension"
        p_typedcomprehension(style, t, s; kwargs...)
    elseif k === K"braces"
        p_braces(style, t, s; kwargs...)
    elseif k === K"bracescat"
        p_bracescat(style, t, s; kwargs...)
    elseif k === K"tuple"
        p_tuple(style, t, s; kwargs...)
    elseif k === K"cartesian_iterator"
        p_cartesian_iterator(style, t, s; kwargs...)
    elseif k === K"parens"
        p_invisbrackets(style, t, s; kwargs...)
    elseif k === K"curly"
        p_curly(style, t, s; kwargs...)
    elseif is_macrostr(t)
        p_macrostr(style, t, s; kwargs...)
    elseif k === K"doc"
        p_globalrefdoc(style, t, s; kwargs...)
    elseif k === K"macrocall"
        p_macrocall(style, t, s; kwargs...)
    elseif k === K"where"
        p_whereopcall(style, t, s; kwargs...)
    elseif k === K"?" && haschildren(t)
        p_conditionalopcall(style, t, s; kwargs...)
    elseif is_binary(t)
        p_binaryopcall(style, t, s; kwargs...)
    elseif is_chain(t)
        p_chainopcall(style, t, s; kwargs...)
    elseif is_unary(t)
        p_unaryopcall(style, t, s; kwargs...)
    elseif is_func_call(t)
        p_call(style, t, s; kwargs...)
    elseif k === K"comparison"
        p_comparison(style, t, s; kwargs...)
    elseif JuliaSyntax.is_operator(t) && haschildren(t)
        p_binaryopcall(style, t, s; kwargs...)
    elseif kind(t) in KSet"dotcall call"
        p_binaryopcall(style, t, s; kwargs...)
    elseif k === K"parameters"
        p_parameters(style, t, s; kwargs...)
    elseif k === K"local"
        p_local(style, t, s; kwargs...)
    elseif k === K"global"
        p_global(style, t, s; kwargs...)
    elseif k === K"const"
        p_const(style, t, s; kwargs...)
    elseif k === K"return"
        p_return(style, t, s; kwargs...)
    elseif k === K"outer"
        p_outer(style, t, s; kwargs...)
    elseif k === K"import"
        p_import(style, t, s; kwargs...)
    elseif k === K"export"
        p_export(style, t, s; kwargs...)
    elseif k === K"using"
        p_using(style, t, s; kwargs...)
    elseif k === K"importpath"
        p_importpath(style, t, s; kwargs...)
    elseif k === K"row"
        p_row(style, t, s; kwargs...)
    elseif k === K"nrow"
        p_nrow(style, t, s; kwargs...)
    elseif k === K"ncat"
        p_ncat(style, t, s; kwargs...)
    elseif k === K"typed_ncat"
        p_typedncat(style, t, s; kwargs...)
    elseif k === K"vcat"
        p_vcat(style, t, s; kwargs...)
    elseif k === K"typed_vcat"
        p_typedvcat(style, t, s; kwargs...)
    elseif k === K"hcat"
        p_hcat(style, t, s; kwargs...)
    elseif k === K"typed_hcat"
        p_typedhcat(style, t, s; kwargs...)
    elseif k === K"ref"
        p_ref(style, t, s; kwargs...)
    elseif k === K"generator"
        p_generator(style, t, s; kwargs...)
    elseif k === K"filter"
        p_filter(style, t, s; kwargs...)
    elseif k === K"juxtapose"
        p_juxtapose(style, t, s; kwargs...)
    elseif k === K"break"
        p_break(style, t, s; kwargs...)
    elseif k === K"continue"
        p_continue(style, t, s; kwargs...)
    elseif k === K"inert"
        p_inert(style, t, s; kwargs...)
    else
        @warn "unknown node" kind(t) t cursor_loc(s)
        if is_leaf(t)
            s.offset += span(t)
            FST(NONE, 0, 0, 0, "")
        else
            tt = FST(Unknown, t, nspaces(s))
            for a in children(t)
                add_node!(tt, pretty(style, a, s; kwargs...), s, join_lines = true)
            end
            tt
        end
    end

    pop!(kwargs[:lineage])

    return ret
end
pretty(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    pretty(DefaultStyle(style), cst, s; kwargs...)

function p_identifier(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)
    s.offset += span(cst)
    FST(IDENTIFIER, loc[2], loc[1], loc[1], val)
end
p_identifier(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_identifier(DefaultStyle(style), cst, s; kwargs...)

function p_whitespace(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)
    s.offset += span(cst)
    FST(NONE, loc[2], loc[1], loc[1], val)
end
p_whitespace(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_whitespace(DefaultStyle(style), cst, s; kwargs...)

function p_comment(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)
    if startswith(val, "#=") && endswith(val, "=#")
        s.offset += span(cst)
        return FST(NONE, loc[2], loc[1], loc[1], val)
    end
    s.offset += span(cst)
    FST(NONE, loc[2], loc[1], loc[1], "")
end
p_comment(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_comment(DefaultStyle(style), cst, s; kwargs...)

function p_semicolon(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    s.offset += span(cst)
    FST(SEMICOLON, loc[2], loc[1], loc[1], ";")
end
p_semicolon(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_semicolon(DefaultStyle(style), cst, s; kwargs...)

function p_macroname(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)
    s.offset += span(cst)
    FST(MACRONAME, loc[2], loc[1], loc[1], val)
end
p_macroname(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_macroname(DefaultStyle(style), cst, s; kwargs...)

function p_operator(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)
    s.offset += span(cst)
    t = FST(OPERATOR, loc[2], loc[1], loc[1], val)
    t.metadata = Metadata(kind(cst), JuliaSyntax.is_dotted(cst))
    return t
end
p_operator(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_operator(DefaultStyle(style), cst, s; kwargs...)

function p_keyword(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)
    s.offset += span(cst)
    FST(KEYWORD, loc[2], loc[1], loc[1], val)
end
p_keyword(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_keyword(DefaultStyle(style), cst, s; kwargs...)

function p_punctuation(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)
    s.offset += span(cst)
    FST(PUNCTUATION, loc[2], loc[1], loc[1], val)
end
p_punctuation(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_punctuation(DefaultStyle(style), cst, s; kwargs...)

function p_juxtapose(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Juxtapose, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
    end

    return t
end
p_juxtapose(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_juxtapose(DefaultStyle(style), cst, s; kwargs...)

function p_continue(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Continue, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
    end

    return t
end
p_continue(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_continue(DefaultStyle(style), cst, s; kwargs...)

function p_break(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Break, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
    end

    return t
end
p_break(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_break(DefaultStyle(style), cst, s; kwargs...)

# $
function p_inert(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Inert, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
    end

    return t
end
p_inert(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_inert(DefaultStyle(style), cst, s; kwargs...)

function p_macrostr(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(MacroStr, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
    end

    return t
end
p_macrostr(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_macrostr(DefaultStyle(style), cst, s; kwargs...)

# what mean
#
# julia> t = parseall(JuliaSyntax.GreenNode, """r"hello"x""")
#      1:9      │[toplevel]
#      1:9      │  [macrocall]
#      1:1      │    StringMacroName      ✔
#      2:8      │    [string]
#      2:2      │      "
#      3:7      │      String             ✔
#      8:8      │      "
#      9:9      │    String               ✔
function p_literal(
    ::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_docstring = false,
    kwargs...,
)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)

    if !is_str_or_cmd(cst)
        if kind(cst) in KSet"Float Float32"
            float_suffix = if (fidx = findlast(==('f'), val)) === nothing
                ""
            else
                fs = val[fidx:end]
                val = val[1:(fidx-1)]
                fs
            end
            if findfirst(c -> c == 'e' || c == 'E', val) === nothing
                if (dotidx = findlast(==('.'), val)) === nothing
                    val *= s.opts.trailing_zero ? ".0" : ""  # append a trailing zero prior to the suffix
                elseif dotidx == length(val)
                    val *= s.opts.trailing_zero ? "0" : ""  # if a float literal ends in `.`, add trailing zero.
                elseif dotidx == 1
                    val = '0' * val  # leading zero
                elseif dotidx == 2 && (val[1] == '-' || val[1] == '+')
                    val = val[1] * '0' * val[2:end]  # leading zero on signed numbers
                end
            end
            val *= float_suffix
        end
    end

    s.offset += span(cst)
    return FST(LITERAL, loc[2], loc[1], loc[1], val)
    # if from_docstring && s.opts.format_docstrings
    #     str = format_docstring(ds, s, str)
    # end
end
p_literal(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_docstring = false,
    kwargs...,
) where {S<:AbstractStyle} =
    p_literal(DefaultStyle(style), cst, s; from_docstring = from_docstring, kwargs...)

function p_accessor(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Accessor, cst, nspaces(s))

    for (i, c) in enumerate(children(cst))
        add_node!(t, pretty(style, c, s; kwargs...), s; join_lines = true)
    end
    t
end
p_accessor(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_accessor(DefaultStyle(style), cst, s; kwargs...)

# StringH
function p_stringh(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_docstring::Bool = false,
    kwargs...,
)
    style = getstyle(ds)
    loc = cursor_loc(s)

    val = ""
    startline = -1
    endline = -1

    for a in children(cst)
        n = pretty(style, a, s; from_docstring = from_docstring, kwargs...)
        val *= gettreeval(n)

        if startline == -1
            startline = n.startline
        end

        if n.endline > endline
            endline = n.endline
        end
    end

    if from_docstring && s.opts.format_docstrings
        val = format_docstring(ds, s, val)
    end

    lines = split(val, "\n")

    if length(lines) == 1
        t = FST(LITERAL, loc[2], startline, startline, lines[1])
        return t
    end

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    t = FST(StringN, cst, loc[2] - 1)
    t.line_offset = loc[2]
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        n = FST(
            LITERAL,
            ln,
            ln,
            sidx - 1,
            length(l),
            l,
            nothing,
            nothing,
            AllowNest,
            0,
            -1,
            nothing,
        )
        add_node!(t, n, s)
    end

    # we need to maintain the start and endlines of the original source
    t.startline = startline
    t.endline = endline

    t
end
p_stringh(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_stringh(DefaultStyle(style), cst, s; kwargs...)

# GlobalRefDoc (docstring)
function p_globalrefdoc(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(GlobalRefDoc, cst, nspaces(s))

    args = children(cst)
    for (i, c) in enumerate(args)
        if i == 1
            add_node!(t, p_stringh(style, c, s; from_docstring = true), s, max_padding = 0)
        elseif i == length(args)
            add_node!(t, pretty(style, c, s; kwargs...), s, max_padding = 0)
        else
            add_node!(t, pretty(style, c, s; kwargs...), s)
        end
    end

    return t
end
p_globalrefdoc(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_globalrefdoc(DefaultStyle(style), cst, s; kwargs...)

# MacroCall
function p_macrocall(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(MacroCall, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )
    has_closer = is_closer(cst[end])
    is_macroblock = !has_closer

    if is_macroblock
        t.typ = MacroBlock
    end

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs..., can_separate_kwargs = false)
        if JuliaSyntax.is_macro_name(a)
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"("
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(a) === K")"
            nest && add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K","
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        elseif kind(a) === K"parameters"
            if n_args(cst) == n_args(a)
                # There are no arguments prior to params
                # so we can remove the initial placeholder.
                idx = findfirst(n -> n.typ === PLACEHOLDER, t.nodes)
                idx !== nothing && (t[idx] = Whitespace(0))
            end
            add_node!(t, n, s; join_lines = true)
        elseif JuliaSyntax.is_whitespace(a)
            add_node!(t, n, s, join_lines = true)
        elseif is_macroblock
            if n.typ === MacroBlock && t[end].typ === WHITESPACE
                t[end] = Placeholder(length(t[end].val))
            end

            max_padding = is_block(n) ? 0 : -1
            join_lines = t.endline == n.startline

            if join_lines && (i > 1 && kind(childs[i-1]) in KSet"NewlineWs Whitespace") ||
               next_node_is(childs[i], nn -> kind(nn) in KSet"NewlineWs Whitespace")
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s; join_lines, max_padding)
        else
            if has_closer
                add_node!(t, n, s, join_lines = true)
            else
                padding = is_block(n) ? 0 : -1
                add_node!(t, n, s, join_lines = true, max_padding = padding)
            end
        end
    end

    # move placement of @ to the end
    #
    # @Module.macro -> Module.@macro
    t[1] = move_at_sign_to_the_end(t[1], s)
    t
end
p_macrocall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_macrocall(DefaultStyle(style), cst, s; kwargs...)

# Block
# length Block is the length of the longest expr
function p_block(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    ignore_single_line = false,
    from_quote = false,
    join_body = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Block, cst, nspaces(s))

    single_line =
        ignore_single_line ? false : on_same_line(s, s.offset, s.offset + span(cst))
    before_first_arg = true

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs...)

        if from_quote && !single_line
            if kind(a) in KSet"; ) ("
                add_node!(t, n, s, join_lines = true)
            elseif kind(a) === K","
                add_node!(t, n, s, join_lines = true)
                if needs_placeholder(childs, i + 1, K")")
                    add_node!(t, Whitespace(1), s)
                end
            elseif JuliaSyntax.is_whitespace(a)
                add_node!(t, n, s, join_lines = true)
            elseif before_first_arg
                add_node!(t, n, s, join_lines = true)
                before_first_arg = false
            else
                add_node!(t, n, s, max_padding = 0)
            end
        elseif single_line
            if kind(a) in KSet", ;"
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, n, s, join_lines = true)
            end
        else
            if kind(a) === K","
                add_node!(t, n, s, join_lines = true)
                if join_body && needs_placeholder(childs, i + 1, K")")
                    add_node!(t, Placeholder(1), s)
                end
            elseif kind(a) === K";"
                continue
            elseif join_body
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, n, s, max_padding = 0)
            end
        end
    end

    t
end
p_block(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_block(DefaultStyle(style), cst, s; kwargs...)

function p_block(
    ds::DefaultStyle,
    nodes::Vector{JuliaSyntax.GreenNode{T}},
    s::State;
    kwargs...,
) where {T}
    style = getstyle(ds)
    t = FST(Block, nspaces(s))

    for (i, a) in enumerate(nodes)
        n = pretty(style, a, s; kwargs...)
        if i < length(nodes) && kind(a) === K"," && is_punc(nodes[i+1])
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"," && i != length(nodes)
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K";"
            continue
        else
            add_node!(t, n, s, max_padding = 0)
        end
    end
    t
end
function p_block(
    style::S,
    nodes::Vector{JuliaSyntax.GreenNode{T}},
    s::State;
    kwargs...,
) where {S<:AbstractStyle,T}
    p_block(DefaultStyle(style), nodes, s; kwargs...)
end

# Abstract
function p_abstract(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Abstract, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        if !JuliaSyntax.is_whitespace(c) && kind(c) !== K"end"
            add_node!(t, Whitespace(1), s)
        end
    end
    t
end
p_abstract(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_abstract(DefaultStyle(style), cst, s; kwargs...)

# Primitive
function p_primitive(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Primitive, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        if !JuliaSyntax.is_whitespace(c) && kind(c) !== K"end"
            add_node!(t, Whitespace(1), s)
        end
    end
    t
end
p_primitive(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_primitive(DefaultStyle(style), cst, s; kwargs...)

function p_var(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(NonStdIdentifier, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
    end
    t
end
p_var(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_var(DefaultStyle(style), cst, s; kwargs...)

# function/macro
function p_functiondef(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(FunctionN, cst, nspaces(s))

    block_has_contents = false
    childs = children(cst)
    for (i, c) in enumerate(childs)
        if i == 1
            n = pretty(style, c, s; kwargs...)
            add_node!(t, n, s)
            add_node!(t, Whitespace(1), s)
        elseif kind(c) === K"end"
            n = pretty(style, c, s; kwargs...)
            if s.opts.join_lines_based_on_source && !block_has_contents
                join_lines = t.endline == n.startline
                join_lines && (add_node!(t, Whitespace(1), s))
                add_node!(t, n, s, join_lines = join_lines)
            elseif block_has_contents
                add_node!(t, n, s)
            else
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            end
        elseif kind(c) === K"block" && haschildren(c)
            block_has_contents =
                length(filter(cc -> !JuliaSyntax.is_whitespace(cc), children(c))) > 0

            s.indent += s.opts.indent
            n = pretty(style, c, s; kwargs..., ignore_single_line = true)
            if s.opts.always_use_return
                prepend_return!(n, s)
            end
            add_node!(t, n, s, max_padding = s.opts.indent)
            s.indent -= s.opts.indent
        elseif kind(c) === K"call"
            add_node!(
                t,
                pretty(style, c, s; kwargs..., can_separate_kwargs = false),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        end
    end
    t.metadata = Metadata(kind(cst), false, false, false, false, true)
    t
end
p_functiondef(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_functiondef(DefaultStyle(style), cst, s; kwargs...)

function p_macro(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_functiondef(ds, cst, s; kwargs...)
    t.typ = Macro
    t
end
p_macro(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_macro(DefaultStyle(style), cst, s; kwargs...)

# struct
function p_struct(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Struct, cst, nspaces(s))

    block_has_contents = false
    childs = children(cst)
    for (i, c) in enumerate(childs)
        if i == 1
            n = pretty(style, c, s; kwargs...)
            add_node!(t, n, s)
            add_node!(t, Whitespace(1), s)
        elseif kind(c) === K"end"
            n = pretty(style, c, s; kwargs...)
            if s.opts.join_lines_based_on_source && !block_has_contents
                join_lines = t.endline == n.startline
                join_lines && (add_node!(t, Whitespace(1), s))
                add_node!(t, n, s, join_lines = join_lines)
            elseif block_has_contents
                add_node!(t, n, s)
            else
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            end
        elseif kind(c) === K"block" && haschildren(c)
            block_has_contents =
                length(filter(cc -> !JuliaSyntax.is_whitespace(cc), children(c))) > 0
            s.indent += s.opts.indent
            n = pretty(style, c, s; kwargs..., ignore_single_line = true)
            if s.opts.annotate_untyped_fields_with_any
                annotate_typefields_with_any!(n, s)
            end
            add_node!(t, n, s, max_padding = s.opts.indent)
            s.indent -= s.opts.indent
        else
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        end
    end
    t
end
p_struct(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_struct(DefaultStyle(style), cst, s; kwargs...)

# mutable
function p_mutable(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Mutable, cst, nspaces(s))

    block_has_contents = false
    childs = children(cst)
    for c in childs
        if kind(c) in KSet"struct mutable"
            n = pretty(style, c, s; kwargs...)
            add_node!(t, n, s; join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif kind(c) === K"end"
            n = pretty(style, c, s; kwargs...)
            if s.opts.join_lines_based_on_source && !block_has_contents
                join_lines = t.endline == n.startline
                join_lines && (add_node!(t, Whitespace(1), s))
                add_node!(t, n, s, join_lines = join_lines)
            elseif block_has_contents
                add_node!(t, n, s)
            else
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            end
        elseif kind(c) === K"block" && haschildren(c)
            block_has_contents =
                length(filter(cc -> !JuliaSyntax.is_whitespace(cc), children(c))) > 0
            s.indent += s.opts.indent
            n = pretty(style, c, s; kwargs..., ignore_single_line = true)
            if s.opts.annotate_untyped_fields_with_any
                annotate_typefields_with_any!(n, s)
            end
            add_node!(t, n, s, max_padding = s.opts.indent)
            s.indent -= s.opts.indent
        else
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        end
    end
    t
end
p_mutable(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_mutable(DefaultStyle(style), cst, s; kwargs...)

# module/baremodule
function p_module(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_module::Bool = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(ModuleN, cst, nspaces(s))

    block_has_contents = false
    childs = children(cst)
    indent_module = s.opts.indent_submodule && from_module

    for c in childs
        if kind(c) in KSet"module baremodule" && !haschildren(c)
            n = pretty(style, c, s; kwargs...)
            add_node!(t, n, s; join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif kind(c) === K"end"
            n = pretty(style, c, s)
            if s.opts.join_lines_based_on_source && !block_has_contents
                join_lines = t.endline == n.startline
                join_lines && (add_node!(t, Whitespace(1), s))
                add_node!(t, n, s, join_lines = join_lines)
            elseif block_has_contents
                add_node!(t, n, s)
            else
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            end
        elseif kind(c) === K"block" && haschildren(c)
            block_has_contents =
                length(filter(cc -> !JuliaSyntax.is_whitespace(cc), children(c))) > 0

            if indent_module
                s.indent += s.opts.indent
            end
            n = pretty(
                style,
                c,
                s;
                kwargs...,
                from_module = true,
                ignore_single_line = true,
            )
            if indent_module
                add_node!(t, n, s; max_padding = s.opts.indent)
                s.indent -= s.opts.indent
            else
                add_node!(t, n, s, max_padding = 0)
            end
        else
            add_node!(t, pretty(style, c, s; from_module = true), s, join_lines = true)
        end
    end
    t
end
p_module(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_module(DefaultStyle(style), cst, s; kwargs...)

function p_baremodule(style::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_module(style, cst, s; kwargs...)
    t.typ = BareModule
    t
end
p_baremodule(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_baremodule(DefaultStyle(style), cst, s; kwargs...)

function p_return(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_const(ds, cst, s; kwargs...)
    t.typ = Return
    t
end
p_return(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_return(DefaultStyle(style), cst, s; kwargs...)

# const/local/global/outer/return
function p_const(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Const, cst, nspaces(s))
    for c in children(cst)
        if kind(c) === K","
        elseif !JuliaSyntax.is_whitespace(c) && !JuliaSyntax.is_keyword(c)
            add_node!(t, Whitespace(1), s)
        elseif !JuliaSyntax.is_whitespace(c) && JuliaSyntax.is_keyword(c) && haschildren(c)
            add_node!(t, Whitespace(1), s)
        end
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
    end
    t
end
p_const(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_const(DefaultStyle(style), cst, s; kwargs...)

function p_local(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_const(ds, cst, s; kwargs...)
    t.typ = Local
    t
end
p_local(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_local(DefaultStyle(style), cst, s; kwargs...)

function p_global(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_const(ds, cst, s; kwargs...)
    t.typ = Global
    t
end
p_global(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_global(DefaultStyle(style), cst, s; kwargs...)

function p_outer(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_const(ds, cst, s; kwargs...)
    t.typ = Outer
    t
end
p_outer(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_outer(DefaultStyle(style), cst, s; kwargs...)

function p_toplevel(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(TopLevel, cst, nspaces(s))
    for a in children(cst)
        n = pretty(style, a, s; kwargs...)
        if kind(a) === K";"
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, max_padding = 0)
        end
    end
    t
end
p_toplevel(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_toplevel(DefaultStyle(style), cst, s; kwargs...)

function p_begin(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Begin, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s; kwargs...), s)

    childs = children(cst)
    empty_body = length(filter(n -> !JuliaSyntax.is_whitespace(n), childs)) == 2

    if empty_body
        for c in childs[2:(end-1)]
            pretty(style, c, s; kwargs...)
        end
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[end], s), s, join_lines = true)
    else
        s.indent += s.opts.indent
        add_node!(
            t,
            p_block(style, childs[2:(end-1)], s; kwargs...),
            s,
            max_padding = s.opts.indent,
        )
        s.indent -= s.opts.indent
        add_node!(t, pretty(style, cst[end], s), s)
    end
    t
end
p_begin(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_begin(DefaultStyle(style), cst, s; kwargs...)

function p_quote(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)

    t = FST(Quote, cst, nspaces(s))
    childs = children(cst)
    if kind(childs[1]) === K"block"
        add_node!(t, p_begin(style, childs[1], s), s, join_lines = true)
        for i in 2:length(childs)
            add_node!(t, pretty(style, childs[i], s; kwargs...), s, join_lines = true)
        end
    else
        for c in childs
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        end
    end

    return t
end
p_quote(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_quote(DefaultStyle(style), cst, s; kwargs...)

function p_quotenode(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Quotenode, cst, nspaces(s))
    for a in children(cst)
        add_node!(
            t,
            pretty(style, a, s; kwargs..., from_quote = true),
            s,
            join_lines = true,
        )
    end
    t
end
p_quotenode(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_quotenode(DefaultStyle(style), cst, s; kwargs...)

# Let
#
# two forms:
#
# let var1 = value1, var2
#     body
# end
#
# y, back = let
#     body
# end
# #
#
# let
# [block]
# ...
# [block]
# end
function p_let(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Let, cst, nspaces(s))
    block_id = 1

    has_let_args = false

    childs = children(cst)
    for (i, c) in enumerate(childs)
        if kind(c) === K"block"
            s.indent += s.opts.indent
            if block_id == 1
                has_let_args =
                    haschildren(c) &&
                    any(n -> kind(n) === K"," || is_iterable(n), children(c))
                add_node!(
                    t,
                    pretty(style, c, s; join_body = true, from_let = true),
                    s,
                    join_lines = true,
                )
            else
                add_node!(
                    t,
                    pretty(
                        style,
                        c,
                        s;
                        kwargs...,
                        ignore_single_line = true,
                        from_let = true,
                    ),
                    s,
                    max_padding = s.opts.indent,
                )
                if has_let_args && t.nodes[end-2].typ !== NOTCODE
                    insert!(t, length(t.nodes) - 1, Placeholder(0))
                end
            end
            s.indent -= s.opts.indent
            block_id += 1
        elseif kind(c) === K"let"
            add_node!(t, pretty(style, c, s; kwargs...), s)
            if block_id == 1 &&
               kind(childs[i+1]) === K"block" &&
               length(children(childs[i+1])) > 0
                add_node!(t, Whitespace(1), s)
            end
        elseif kind(c) === K"end"
            add_node!(t, pretty(style, c, s; kwargs...), s)
        else
            add_node!(
                t,
                pretty(style, c, s; kwargs..., from_let = true),
                s,
                join_lines = true,
            )
        end
    end
    t
end
p_let(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_let(DefaultStyle(style), cst, s; kwargs...)

# For/While
function p_for(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(For, cst, nspaces(s))

    ends_in_iterable = false

    for c in children(cst)
        if kind(c) in KSet"for while" && !haschildren(c)
            add_node!(t, pretty(style, c, s), s)
        elseif kind(c) === K"end"
            add_node!(t, pretty(style, c, s), s)
        elseif kind(c) === K"block"
            s.indent += s.opts.indent
            n = pretty(style, c, s; kwargs..., ignore_single_line = true)
            add_node!(t, n, s; max_padding = s.opts.indent)
            s.indent -= s.opts.indent

            if !ends_in_iterable && t.nodes[end-2].typ !== NOTCODE
                insert!(t, length(t.nodes) - 1, Placeholder(0))
            end
        elseif JuliaSyntax.is_whitespace(c)
            add_node!(t, pretty(style, c, s; kwargs...), s)
        else
            add_node!(t, Whitespace(1), s)
            n = if kind(c) === K"cartesian_iterator"
                s.indent += s.opts.indent
                n = pretty(style, c, s; kwargs..., from_for = true)
                s.indent -= s.opts.indent
                n
            else
                n = pretty(style, c, s; kwargs..., from_for = true)
                if !is_leaf(n) && length(n.nodes) > 1 && is_iterable(n[end])
                    ends_in_iterable = true
                end
                n
            end
            if kind(cst) === K"for"
                eq_to_in_normalization!(n, s.opts.always_for_in, s.opts.for_in_replacement)
            end
            add_node!(t, n, s, join_lines = true)
        end
    end

    t
end
p_for(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_for(DefaultStyle(style), cst, s; kwargs...)

function p_cartesian_iterator(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_for = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(CartesianIterator, cst, nspaces(s))

    childs = children(cst)
    for (i, c) in enumerate(childs)
        n = pretty(style, c, s; kwargs...)
        if kind(c) === K","
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        elseif !JuliaSyntax.is_whitespace(c)
            if from_for
                eq_to_in_normalization!(n, s.opts.always_for_in, s.opts.for_in_replacement)
            end
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end

    t
end
p_cartesian_iterator(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_cartesian_iterator(DefaultStyle(style), cst, s; kwargs...)

function p_while(style::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_for(style, cst, s; kwargs...)
    t.typ = While
    t
end
p_while(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_while(DefaultStyle(style), cst, s; kwargs...)

# Do
# node [nodes] do [nodes] node node end
function p_do(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Do, cst, nspaces(s))

    for (i, c) in enumerate(children(cst))
        if kind(c) === K"do" && !haschildren(c)
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
            if !next_node_is(cst[i+1], K"NewlineWs")
                add_node!(t, Whitespace(1), s)
            end
        elseif kind(c) === K"end"
            add_node!(t, pretty(style, c, s; kwargs...), s)
        elseif kind(c) === K"block"
            s.indent += s.opts.indent
            n = pretty(style, c, s; kwargs..., ignore_single_line = true)
            if s.opts.always_use_return
                prepend_return!(n, s)
            end
            add_node!(t, n, s, max_padding = s.opts.indent)
            s.indent -= s.opts.indent
        else
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        end
    end
    t
end
p_do(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_do(DefaultStyle(style), cst, s; kwargs...)

# Try
function p_try(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Try, cst, nspaces(s))

    # in JuliaSyntax this is now a tree structure instead of being linear
    # since we're still picking up comments in add_node! if the comment is at
    # the end of block it will be added as a comment in the parent node and hence
    # have a lower indentation than the rest of the block. To counteract that we reduce
    # the indent when we encounter "catch finally end" keywords.

    childs = children(cst)
    for c in childs
        if kind(c) in KSet"try catch finally"
            if !haschildren(c)
                if kind(c) in KSet"catch finally"
                    s.indent -= s.opts.indent
                end
                add_node!(t, pretty(style, c, s; kwargs...), s, max_padding = 0)
            else
                len = length(t)
                n = pretty(style, c, s; kwargs...)
                add_node!(t, n, s; max_padding = 0)
                t.len = max(len, length(n))
            end
        elseif kind(c) === K"end"
            s.indent -= s.opts.indent
            add_node!(t, pretty(style, c, s; kwargs...), s)
        elseif kind(c) === K"block"
            s.indent += s.opts.indent
            add_node!(
                t,
                pretty(style, c, s; ignore_single_line = true),
                s,
                max_padding = s.opts.indent,
            )
        elseif !JuliaSyntax.is_whitespace(c)
            # "catch" vs "catch ..."
            if !(kind(cst) === K"catch" && any(n -> kind(n) === K"false", childs))
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        else
            add_node!(t, pretty(style, c, s; kwargs...), s)
        end
    end
    t
end
p_try(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_try(DefaultStyle(style), cst, s; kwargs...)

# If
function p_if(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(If, cst, nspaces(s))

    for c in children(cst)
        if kind(c) in KSet"if elseif else"
            if !haschildren(c)
                add_node!(t, pretty(style, c, s; kwargs...), s, max_padding = 0)
            else
                len = length(t)
                n = pretty(style, c, s; kwargs...)
                add_node!(t, n, s)
                t.len = max(len, length(n))
            end
        elseif kind(c) === K"end"
            add_node!(t, pretty(style, c, s; kwargs...), s)
        elseif kind(c) === K"block"
            s.indent += s.opts.indent
            add_node!(
                t,
                pretty(style, c, s; kwargs..., ignore_single_line = true),
                s,
                max_padding = s.opts.indent,
            )
            s.indent -= s.opts.indent
        elseif !JuliaSyntax.is_whitespace(c)
            add_node!(t, Whitespace(1), s)
            add_node!(
                t,
                pretty(style, c, s; kwargs..., standalone_binary_circuit = false),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, c, s; kwargs...), s)
        end
    end

    # @info "length" t.len
    return t
end
p_if(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_if(DefaultStyle(style), cst, s; kwargs...)

# Chain/Comparison
function p_chainopcall(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_binaryopcall(ds, cst, s; kwargs...)
    t.typ = Chain
    t
end
p_chainopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_chainopcall(DefaultStyle(style), cst, s; kwargs...)

function p_comparison(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_chainopcall(ds, cst, s; kwargs...)
    t.typ = Comparison
    t
end
p_comparison(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_comparison(DefaultStyle(style), cst, s; kwargs...)

# Kw
# this is only called on it's own so we need to add the lineage
function p_kw(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State;
    lineage = Tuple{JuliaSyntax.Kind,Bool,Bool}[],
              kwargs...)
    style = getstyle(ds)
    t = FST(Kw, cst, nspaces(s))

    push!(lineage, (kind(cst), false, true))

    for c in children(cst)
        if kind(c) === K"=" && s.opts.whitespace_in_kwargs
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            n = pretty(style, c, s; kwargs...)
            if !s.opts.whitespace_in_kwargs &&
               ((n.typ === IDENTIFIER && endswith(n.val, "!")) || (is_prefix_op_call(c)))
                add_node!(
                    t,
                    FST(PUNCTUATION, -1, n.startline, n.startline, "("),
                    s,
                    join_lines = true,
                )
                add_node!(t, n, s, join_lines = true)
                add_node!(
                    t,
                    FST(PUNCTUATION, -1, n.startline, n.startline, ")"),
                    s,
                    join_lines = true,
                )
            else
                add_node!(t, n, s, join_lines = true)
            end
        end
    end

    pop!(lineage)

    t
end
p_kw(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_kw(DefaultStyle(style), cst, s; kwargs...)

function p_binaryopcall(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nospace = false,
    nonest = false,
    from_typedef = false,
    standalone_binary_circuit = true,
    from_let = false,
    from_ref = false,
    from_colon = false,
    lineage = Tuple{JuliaSyntax.Kind,Bool,Bool}[],
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Binary, cst, nspaces(s))
    opkind = op_kind(cst)

    nonest = nonest || opkind === K":"

    nrhs = nest_rhs(cst)
    nrhs && (t.nest_behavior = AlwaysNest)
    nest = (is_binaryop_nestable(style, cst) && !nonest) || nrhs

    is_short_form_function = defines_function(cst) && !from_let
    op_dotted = kind(cst) === K"dotcall"
    can_separate_kwargs = !is_function_or_macro_def(cst)

    lazy_op = is_lazy_op(opkind)
    # check if expression is a lazy circuit
    if lazy_op && standalone_binary_circuit
        for i in (length(lineage)-1):-1:1
            tt, _, is_assign = lineage[i]
            if tt in KSet"parens macrocall return if elseif else" || is_assign
                standalone_binary_circuit = false
                break
            elseif tt in KSet"block"
                break
            end
        end
    end

    t.metadata = Metadata(
        opkind,
        op_dotted,
        lazy_op && standalone_binary_circuit,
        is_short_form_function,
        is_assignment(cst),
        false,
    )

    has_ws = false
    childs = children(cst)
    for (i, c) in enumerate(childs)
        if i > 1 && kind(c) in KSet"Whitespace NewlineWs"
            has_ws = true
            break
        end
    end

    if opkind === K":"
        nospace = true
        from_colon = true
    elseif opkind in KSet"in ∈ isa ."
        nospace = false
    elseif from_typedef && opkind in KSet"<: >:"
        if s.opts.whitespace_typedefs
            nospace = false
            has_ws = true
        else
            nospace = true
            has_ws = false
        end
    elseif from_ref
        if s.opts.whitespace_ops_in_indices
            nospace = false
            has_ws = true
        else
            nospace = true
            has_ws = false
        end
    elseif from_colon
        nospace = true
    end
    nws = !nospace && has_ws ? 1 : 0

    if kind(cst) === K"dotcall"
        nospace = false
        nws = 1
    end

    nlws_count = 0
    after_op = false

    op_indices = extract_operator_indices(childs)

    for (i, c) in enumerate(childs)
        n = pretty(
            style,
            c,
            s;
            nonest,
            from_typedef,
            from_ref,
            from_colon,
            kwargs...,
            standalone_binary_circuit = standalone_binary_circuit &&
                !(is_lazy_op(c) && kind(c) !== opkind),
            can_separate_kwargs = can_separate_kwargs,
        )

        is_dot = kind(c) === K"."
        if is_dot && haschildren(c) && length(children(c)) == 2
            # [.]
            #   .
            #   <=
            ns = is_dot ? 1 : nws

            # Add whitespace before the operator, unless it's a dot in a dotted operator
            if ns > 0
                add_node!(t, Whitespace(ns), s)
            end
            add_node!(t, n, s, join_lines = true)
            # Add whitespace after the operator
            if ns > 0
                if nest
                    add_node!(t, Placeholder(ns), s)
                else
                    add_node!(t, Whitespace(ns), s)
                end
            end
            after_op = true
            # elseif (kind(c) === opkind || kind(c) === K".") && !haschildren(c)
        elseif JuliaSyntax.is_operator(c) && !haschildren(c) && i in op_indices
            # there are some weird cases where we can assign an operator a value so that
            # the arguments are operators as well.
            #
            # a .* %
            ns = is_dot ? 1 : nws

            # Add whitespace before the operator, unless it's a dot in a dotted operator
            if ns > 0
                if i > 1 && !(kind(childs[i-1]) === K".")  # Don't add space if previous was a dot
                    add_node!(t, Whitespace(ns), s)
                end
            end

            add_node!(t, n, s, join_lines = true)

            # Add whitespace after the operator
            if !is_dot && ns > 0
                if nest
                    add_node!(t, Placeholder(ns), s)
                else
                    add_node!(t, Whitespace(ns), s)
                end
            end

            after_op = true
        elseif JuliaSyntax.is_whitespace(c)
            add_node!(t, n, s; join_lines = true)
        else
            if opkind === K":" &&
               # !s.whitespace_ops_in_indices &&
               is_opcall(c) &&
               kind(c) !== K"parens"
                add_node!(
                    t,
                    FST(PUNCTUATION, -1, n.startline, n.startline, "("),
                    s;
                    join_lines = true,
                )
                if after_op
                    add_node!(
                        t,
                        n,
                        s,
                        join_lines = true,
                        override_join_lines_based_on_source = !nest,
                    )
                else
                    add_node!(t, n, s; join_lines = true)
                end
                add_node!(
                    t,
                    FST(PUNCTUATION, -1, n.startline, n.startline, ")"),
                    s;
                    join_lines = true,
                )
            else
                if after_op
                    add_node!(
                        t,
                        n,
                        s;
                        join_lines = true,
                        override_join_lines_based_on_source = !nest,
                    )
                else
                    add_node!(t, n, s; join_lines = true)
                end
            end
        end

        if kind(c) === K"NewlineWs"
            nlws_count += 1
        end
    end

    if nest && is_binary(cst)
        # for indent, will be converted to `indent` if needed
        insert!(t.nodes::Vector{FST}, length(t.nodes::Vector{FST}), Placeholder(0))
    end

    t
end
p_binaryopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_binaryopcall(DefaultStyle(style), cst, s; kwargs...)

function p_whereopcall(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_typedef::Bool = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Where, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    childs = children(cst)
    where_idx = findfirst(c -> kind(c) === K"where" && !haschildren(c), childs)
    curly_ctx = if where_idx === nothing
        from_typedef
    else
        from_typedef ||
            any(c -> kind(c) in KSet"curly bracescat braces", childs[(where_idx+1):end])
    end
    add_braces = s.opts.surround_whereop_typeparameters && !curly_ctx

    nws = s.opts.whitespace_typedefs ? 1 : 0

    after_where = false
    for (i, a) in enumerate(childs)
        if kind(a) === K"where" && !haschildren(a)
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
            after_where = true
        elseif kind(a) === K"{" && nest
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
            s.indent += s.opts.indent
        elseif kind(a) === K"}" && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            s.indent -= s.opts.indent
        elseif kind(a) === K","
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            if needs_placeholder(childs, i + 1, K"}")
                add_node!(t, Placeholder(nws), s)
            end
        elseif JuliaSyntax.is_whitespace(a)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        else
            n = pretty(style, a, s; kwargs..., from_typedef = after_where)

            if after_where && add_braces
                brace = FST(PUNCTUATION, -1, n.endline, n.endline, "{")
                add_node!(t, brace, s, join_lines = true)
            end

            add_node!(t, n, s, join_lines = true)

            if after_where && add_braces
                brace = FST(PUNCTUATION, -1, n.endline, n.endline, "}")
                add_node!(t, brace, s, join_lines = true)
            end
        end
    end

    t
end
p_whereopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_whereopcall(DefaultStyle(style), cst, s; kwargs...)

function p_conditionalopcall(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Conditional, cst, nspaces(s))

    for c in children(cst)
        if kind(c) in KSet"? :" && !haschildren(c)
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        end
    end

    t
end
p_conditionalopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_conditionalopcall(DefaultStyle(style), cst, s; kwargs...)

function p_unaryopcall(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Unary, cst, nspaces(s))

    opkind = op_kind(cst)
    op_dotted = kind(cst) === K"dotcall"

    t.metadata = Metadata(opkind, op_dotted)

    for (i, c) in enumerate(children(cst))
        if i > 1 && kind(c) in KSet"Whitespace"
            add_node!(t, Whitespace(1), s)
        end
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
    end
    t
end
p_unaryopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_unaryopcall(DefaultStyle(style), cst, s; kwargs...)

function p_curly(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Curly, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    nws = s.opts.whitespace_typedefs ? 1 : 0

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs..., from_typedef = true)

        if kind(a) === K"{"
            add_node!(t, n, s, join_lines = true)
            if nest
                add_node!(t, Placeholder(0), s)
            end
        elseif kind(a) === K"}"
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K","
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K"}")
                add_node!(t, Placeholder(nws), s)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_curly(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_curly(DefaultStyle(style), cst, s; kwargs...)

function p_call(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    can_separate_kwargs::Bool = true,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Call, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    childs = children(cst)
    for (i, a) in enumerate(childs)
        k = kind(a)
        n = if k == K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        if k === K"("
            add_node!(t, n, s, join_lines = true)
            if nest
                add_node!(t, Placeholder(0), s)
            end
        elseif k === K")"
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif k === K","
            add_node!(t, n, s, join_lines = true)

            # figure out if we need to put a placeholder
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        elseif k === K"parameters"
            if n_args(cst) == n_args(a)
                # There are no arguments prior to params
                # so we can remove the initial placeholder.
                idx = findfirst(n -> n.typ === PLACEHOLDER, t.nodes)
                idx !== nothing && (t[idx] = Whitespace(0))
            end
            add_node!(t, n, s; join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end

    if s.opts.separate_kwargs_with_semicolon && can_separate_kwargs
        separate_kwargs_with_semicolon!(t)
    end

    t
end
p_call(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_call(DefaultStyle(style), cst, s; kwargs...)

function p_invisbrackets(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest::Bool = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Brackets, cst, nspaces(s))
    args = get_args(cst)
    nest = if length(args) > 0
        arg = args[1]
        if is_block(arg) || (kind(arg) === K"generator" && is_block(arg[1]))
            t.nest_behavior = AlwaysNest
        end
        !nonest && !s.opts.disallow_single_arg_nesting && !is_iterable(arg)
    else
        false
    end

    for c in children(cst)
        if kind(c) === K"("
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(c) === K")"
            nest && add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        elseif kind(c) === K"block"
            add_node!(
                t,
                pretty(style, c, s; kwargs..., from_quote = true),
                s,
                join_lines = true,
            )
        elseif is_opcall(c)
            add_node!(
                t,
                pretty(style, c, s; nonest = nonest, kwargs...),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        end
    end

    t
end
p_invisbrackets(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_invisbrackets(DefaultStyle(style), cst, s; kwargs...)

function p_tuple(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(TupleN, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = if kind(a) === K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        if kind(a) === K"("
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(a) === K")"
            # An odd case but this could occur if there are no keyword arguments.
            # In which case ";," is invalid syntax.
            #
            # no trailing comma since (arg) is semantically different from (arg,) !!!
            if nest
                if t[end].typ !== SEMICOLON && length(args) > 1
                    add_node!(t, TrailingComma(), s)
                end
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K","
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_tuple(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_tuple(DefaultStyle(style), cst, s; kwargs...)

function p_braces(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_typedef = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Braces, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    nws = from_typedef && !s.opts.whitespace_typedefs ? 0 : 1

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; from_typedef, kwargs...)

        if kind(a) === K"{"
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(a) === K"}"
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K","
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K"}")
                add_node!(t, Placeholder(nws), s)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_braces(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_braces(DefaultStyle(style), cst, s; kwargs...)

function p_bracescat(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_typedef = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(BracesCat, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    nws = from_typedef && !s.opts.whitespace_typedefs ? 0 : 1
    childs = children(cst)

    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; from_typedef, kwargs...)

        if kind(a) === K"{"
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(a) === K"}"
            if nest
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K";"
            if needs_placeholder(childs, i + 1, K"}")
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Placeholder(nws), s)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_bracescat(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_bracescat(DefaultStyle(style), cst, s; kwargs...)

function p_vect(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Vect, cst, nspaces(s))

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs...)

        if kind(a) === K"["
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(a) === K"]"
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K","
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K"]")
                add_node!(t, Placeholder(1), s)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_vect(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_vect(DefaultStyle(style), cst, s; kwargs...)

function p_comprehension(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Comprehension, cst, nspaces(s))

    idx = findfirst(
        n -> !JuliaSyntax.is_whitespace(kind(n)) && !(kind(n) in KSet"[ ]"),
        children(cst),
    )
    arg = cst[idx]

    if is_block(arg)
        t.nest_behavior = AlwaysNest
    elseif kind(arg) === K"generator"
        idx = findfirst(n -> !JuliaSyntax.is_whitespace(kind(n)), children(arg))
        if !isnothing(idx) && is_block(arg[idx])
            t.nest_behavior = AlwaysNest
        end
    end

    for c in children(cst)
        n = pretty(style, c, s; kwargs...)
        if kind(c) === K"["
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif kind(c) === K"]"
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end

    t
end
p_comprehension(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_comprehension(DefaultStyle(style), cst, s; kwargs...)

function p_typedcomprehension(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
)
    t = p_comprehension(ds, cst, s; kwargs...)
    t.typ = TypedComprehension
    t
end
p_typedcomprehension(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_typedcomprehension(DefaultStyle(style), cst, s; kwargs...)

function p_parameters(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_typedef = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Parameters, cst, nspaces(s))

    nws = from_typedef && !s.opts.whitespace_typedefs ? 0 : 1

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = if kind(a) === K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        if kind(a) in KSet", ;"
            add_node!(t, n, s; join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(nws), s)
            end
        else
            add_node!(t, n, s; join_lines = true)
        end
    end
    t
end
p_parameters(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_parameters(DefaultStyle(style), cst, s; kwargs...)

function p_import(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Import, cst, nspaces(s))

    for a in children(cst)
        if kind(a) in KSet"import export using"
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif kind(a) === K":"
            nodes = children(a)
            for n in nodes
                add_node!(t, pretty(style, n, s; kwargs...), s, join_lines = true)
                if kind(n) in KSet"import export using"
                    add_node!(t, Whitespace(1), s)
                elseif kind(n) in KSet", :"
                    add_node!(t, Placeholder(1), s)
                end
            end
        elseif kind(a) in KSet", :"
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
        end
    end
    t
end
p_import(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_import(DefaultStyle(style), cst, s; kwargs...)

function p_export(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_import(ds, cst, s; kwargs...)
    t.typ = Export
    t
end
p_export(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_export(DefaultStyle(style), cst, s; kwargs...)

function p_using(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_import(ds, cst, s; kwargs...)
    t.typ = Using
    t
end
p_using(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_using(DefaultStyle(style), cst, s; kwargs...)

function p_importpath(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(ImportPath, cst, nspaces(s))

    for a in children(cst)
        n = pretty(style, a, s; kwargs...)
        add_node!(t, n, s, join_lines = true)
    end
    t
end
p_importpath(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_importpath(DefaultStyle(style), cst, s; kwargs...)

function p_as(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(As, cst, nspaces(s))

    for c in children(cst)
        n = pretty(style, c, s; kwargs...)
        if kind(c) === K"as"
            add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end

    t
end
p_as(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_as(DefaultStyle(style), cst, s; kwargs...)

function p_ref(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(RefN, cst, nspaces(s))
    args = get_args(cst)
    nest =
        length(args) > 1 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    childs = children(cst)
    for (i, a) in enumerate(childs)
        if kind(a) === K"]"
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
        elseif kind(a) === K"["
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(a) === K","
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            if needs_placeholder(childs, i + 1, K"]")
                add_node!(t, Placeholder(1), s)
            end
        elseif is_opcall(a)
            n = pretty(style, a, s; kwargs..., from_ref = true, nonest = true)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
        end
    end
    t
end
p_ref(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_ref(DefaultStyle(style), cst, s; kwargs...)

function p_vcat(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Vcat, cst, nspaces(s))
    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )
    st = kind(cst) in KSet"vcat ncat" ? 1 : 2

    childs = children(cst)
    first_arg_idx = findnext(n -> !JuliaSyntax.is_whitespace(n), childs, st + 1)

    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs..., from_matrix = true)
        diff_line = t.endline != t.startline
        # If arguments are on different lines then always nest
        diff_line && (t.nest_behavior = AlwaysNest)

        if kind(a) === K"["
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(a) === K"]"
            nest && add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif JuliaSyntax.is_whitespace(a)
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K";"
            add_node!(t, n, s, join_lines = true)
        else
            # TODO: maybe we need to do something here?
            # [a b c d e f] is semantically different from [a b c; d e f]
            # child_has_semicolon = any(c -> kind(c) === K";", children(a))
            # if !child_has_semicolon
            #     add_node!(t, n, s, join_lines = false)
            # else
            #     add_node!(t, n, s, join_lines = true)
            # end
            if i > first_arg_idx
                add_node!(t, Placeholder(1), s)
            end

            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_vcat(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_vcat(DefaultStyle(style), cst, s; kwargs...)

function p_typedvcat(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_vcat(ds, cst, s; kwargs...)
    t.typ = TypedVcat
    t
end
p_typedvcat(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_typedvcat(DefaultStyle(style), cst, s; kwargs...)

function p_hcat(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Hcat, cst, nspaces(s))
    st = kind(cst) === K"hcat" ? 1 : 2
    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs...)
        if JuliaSyntax.is_whitespace(a)
            add_node!(t, n, s; join_lines = true)
        elseif i > st
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K"]")
                add_node!(t, Whitespace(1), s)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_hcat(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_hcat(DefaultStyle(style), cst, s; kwargs...)

function p_typedhcat(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_hcat(ds, cst, s; kwargs...)
    t.typ = TypedHcat
    t
end
p_typedhcat(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_typedhcat(DefaultStyle(style), cst, s; kwargs...)

function p_ncat(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_vcat(ds, cst, s; kwargs...)
    t.typ = Ncat
    return t
end
p_ncat(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_ncat(DefaultStyle(style), cst, s; kwargs...)

function p_typedncat(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_ncat(ds, cst, s; kwargs...)
    t.typ = TypedNcat
    t
end
p_typedncat(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_typedncat(DefaultStyle(style), cst, s; kwargs...)

function p_row(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Row, cst, nspaces(s))

    childs = children(cst)
    first_arg_idx = findfirst(n -> !JuliaSyntax.is_whitespace(n), childs)

    for (i, a) in enumerate(childs)
        n = if is_opcall(a)
            pretty(style, a, s; nonest = true, kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        if kind(a) === K";"
            add_node!(t, n, s; join_lines = true)
        elseif JuliaSyntax.is_whitespace(a)
            add_node!(t, n, s; join_lines = true)
        else
            if i > first_arg_idx
                add_node!(t, Whitespace(1), s; join_lines = true)
            end
            add_node!(t, n, s; join_lines = true)
        end
    end
    t.nest_behavior = NeverNest
    t
end
p_row(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_row(DefaultStyle(style), cst, s; kwargs...)

function p_nrow(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_row(ds, cst, s; kwargs...)
    t.typ = NRow
    t
end

p_nrow(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_nrow(DefaultStyle(style), cst, s; kwargs...)

function p_generator(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    lineage = Tuple{JuliaSyntax.Kind,Bool,Bool}[],
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Generator, cst, nspaces(s))
    has_for_kw = false

    from_iterable = false
    for (kind, is_itr, _) in Iterators.reverse(lineage)
        if kind in KSet"parens generator filter"
            continue
        elseif is_itr
            from_iterable = true
            break
        end
    end

    childs = children(cst)

    has_for_kw = findfirst(n -> kind(n) === K"for", childs) !== nothing

    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs..., from_for = has_for_kw)
        if JuliaSyntax.is_keyword(a) && !haschildren(a)
            # for keyword can only be on the following line
            # if this expression is within an iterable expression
            if kind(a) === K"for" && from_iterable
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end

            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif kind(a) === K","
            add_node!(t, n, s, join_lines = true)
            if needs_placeholder(childs, i + 1, K")")
                add_node!(t, Placeholder(1), s)
            end
        else
            add_node!(t, n, s, join_lines = true)
        end

        has_for_kw &&
            eq_to_in_normalization!(n, s.opts.always_for_in, s.opts.for_in_replacement)
    end
    t
end
p_generator(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_generator(DefaultStyle(style), cst, s; kwargs...)

function p_filter(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_generator(ds, cst, s; kwargs...)
    t.typ = Filter
    t
end
p_filter(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_filter(DefaultStyle(style), cst, s; kwargs...)
