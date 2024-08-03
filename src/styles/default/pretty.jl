function pretty(ds::DefaultStyle, t::JuliaSyntax.GreenNode, s::State; kwargs...)
    k = kind(t)
    style = getstyle(ds)

    if !(haskey(kwargs, :from_iterable) && kwargs[:from_iterable])
        kwargs = (; kwargs..., from_iterable = is_iterable(t))
    end

    if k == K"Identifier" && !haschildren(t)
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
    elseif k === K"string"
        p_stringh(style, t, s; kwargs...)
    elseif JuliaSyntax.is_literal(t) || k in KSet"\" \"\"\" ` ```"
        p_literal(style, t, s; kwargs...)
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
    elseif is_if(t)
        p_if(style, t, s; kwargs...)
    elseif k in KSet"try catch finally" && haschildren(t)
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
    elseif k === K"parens"
        p_invisbrackets(style, t, s; kwargs...)
    elseif k === K"curly"
        p_curly(style, t, s; kwargs...)
        # elseif is_macrostr(t)
        #     p_macrostr(style, t, s; kwargs...)
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
    elseif is_unary(t)
        p_unaryopcall(style, t, s; kwargs...)
    elseif is_chain(t)
        p_chainopcall(style, t, s; kwargs...)
    elseif is_func_call(t)
        p_call(style, t, s; kwargs...)
    elseif k === K"comparison"
        p_comparison(style, t, s; kwargs...)
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
    else
        @warn "unknown node" kind(t) t s.offset span(t)
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
    s.offset += span(cst)
    FST(NONE, 0, 0, 0, "")
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

function p_operator(::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    loc = cursor_loc(s)
    val = getsrcval(s.doc, s.offset:s.offset+span(cst)-1)
    s.offset += span(cst)
    t = FST(OPERATOR, loc[2], loc[1], loc[1], val)
    # t.metadata = Metadata(tokenize(cst.val::AbstractString), CSTParser.isdotted(cst))
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
                val = val[1:fidx-1]
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
    sidx = loc[2]

    t = FST(StringN, cst, loc[2] - 1)
    t.line_offset = loc[2]

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; from_docstring = from_docstring, kwargs...)

        if i > 1 && n.val !== nothing
            fc = findfirst(c -> !isspace(c), n.val)
            if fc !== nothing
                sidx = min(sidx, fc)
            end
        end

        add_node!(t, n, s)
    end

    for (i, n) in enumerate(t)
        n.indent = sidx - 1
        if i > 1
            n.val = n.val[sidx:end]
            n.len = length(n.val)
        end
    end
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

    !has_closer && (t.typ = MacroBlock)

    for (i, a) in enumerate(children(cst))
        n = pretty(
            style,
            a,
            s;
            kwargs...,
            can_separate_kwargs = false,
            standalone_binary_circuit = false,
        )
        if JuliaSyntax.is_macro_name(a)
            add_node!(t, n, s, join_lines = true)
            if JuliaSyntax.is_whitespace(kind(cst[i+1]))
                add_node!(t, Whitespace(1), s)
            end
        elseif is_opener(n)
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif is_closer(n)
            nest && add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif t.typ === MacroBlock
            if n.typ === MacroBlock && t[end].typ === WHITESPACE
                t[end] = Placeholder(length(t[end].val))
            end
            if has_closer
                add_node!(t, n, s, join_lines = true)
                if i < length(cst) - 1 && kind(cst[i+1]) !== K"parameters"
                    add_node!(t, Whitespace(1), s)
                end
            else
                padding = is_block(n) ? 0 : -1
                add_node!(t, n, s, join_lines = true, max_padding = padding)
                i < length(cst) && add_node!(t, Whitespace(1), s)
            end
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

function p_macroname(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Macroname, cst, nspaces(s))
    for a in children(cst)
        add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
    end
    t
end
p_macroname(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_macroname(DefaultStyle(style), cst, s; kwargs...)

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

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs..., standalone_binary_circuit = false)

        if from_quote && !single_line
            if i == 1 || kind(a) in KSet", ;"
                add_node!(t, n, s, join_lines = true)
            elseif kind(childs[i-1]) === K","
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
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
            if i < length(childs) && kind(a) === K"," && is_punc(childs[i+1])
                add_node!(t, n, s, join_lines = true)
            elseif kind(a) === K"," && i != length(childs)
                add_node!(t, n, s, join_lines = true)
                join_body && add_node!(t, Placeholder(1), s)
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
    ignore_single_line = false,
    from_quote = false,
    join_body = false,
    kwargs...,
) where {S<:AbstractStyle} = p_block(
    DefaultStyle(style),
    cst,
    s,
    ignore_single_line = ignore_single_line,
    from_quote = from_quote,
    join_body = join_body,
    kwargs...,
)

function p_block(
    ds::DefaultStyle,
    nodes::Vector{JuliaSyntax.GreenNode{T}},
    s::State;
    kwargs...,
) where {T}
    style = getstyle(ds)
    t = FST(Block, nspaces(s))

    for (i, a) in enumerate(nodes)
        n = pretty(style, a, s; kwargs..., standalone_binary_circuit = false)
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
p_block(
    style::S,
    nodes::Vector{JuliaSyntax.GreenNode},
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_block(DefaultStyle(style), nodes, s; kwargs...)

# Abstract
function p_abstract(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Abstract, cst, nspaces(s))

    for c in children(cst)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        if !JuliaSyntax.is_whitespace(c) || kind(c) === K"end"
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
        if !JuliaSyntax.is_whitespace(c) || kind(c) === K"end"
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
            n = pretty(style, c, s, kwargs..., ignore_single_line = true)
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

            indent_module && (s.indent += s.opts.indent)
            n = pretty(
                style,
                c,
                s,
                kwargs...,
                from_module = true,
                ignore_single_line = true,
            )
            add_node!(t, n, s, max_padding = s.opts.indent)
            indent_module && (s.indent -= s.opts.indent)
            # elseif kind(c) in KSet"module baremodule" && indent_module
            #     @info "here"
            #     s.indent += s.opts.indent
            #     add_node!(t, pretty(style, c, s; from_module = true), s, join_lines = true)
            #     s.indent -= s.opts.indent
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
    t = p_const(ds, cst, s; kwargs..., standalone_binary_circuit = false)
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
        if !JuliaSyntax.is_whitespace(c) && !JuliaSyntax.is_keyword(c)
            add_node!(t, Whitespace(1), s)
        end
        add_node!(t, pretty(style, c, s), s, join_lines = true)
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
        add_node!(t, pretty(style, a, s; kwargs...), s, max_padding = s.opts.indent)
    end
    t
end
p_toplevel(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_toplevel(DefaultStyle(style), cst, s; kwargs...)

# TODO: might not work with comments?
function p_begin(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Begin, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s; kwargs...), s)

    childs = children(cst)
    empty_body = length(filter(n -> !JuliaSyntax.is_whitespace(n), childs)) == 2

    if empty_body
        for c in childs[2:end-1]
            pretty(style, c, s; kwargs...)
        end
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[end], s), s, join_lines = true)
    else
        s.indent += s.opts.indent
        add_node!(t, p_block(style, childs[2:end-1], s), s, max_padding = s.opts.indent)
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
            add_node!(t, pretty(style, childs[i], s, kwargs...), s, join_lines = true)
        end
    else
        for c in childs
            add_node!(t, pretty(style, c, s, kwargs...), s, join_lines = true)
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
        add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
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
    block_count = 0

    for c in children(cst)
        if kind(c) === K"block"
            s.indent += s.opts.indent
            if block_count == 0
                add_node!(t, p_block(style, c, s, join_body = true), s, join_lines = true)
            else
                add_node!(
                    t,
                    pretty(style, c, s; kwargs..., ignore_single_line = true),
                    s,
                    max_padding = s.opts.indent,
                )
            end
            s.indent -= s.opts.indent
            block_count += 1
        elseif kind(c) === K"let"
            add_node!(t, pretty(style, c, s; kwargs...), s)
            add_node!(t, Whitespace(1), s)
        elseif kind(c) === K"end"
            add_node!(t, pretty(style, c, s; kwargs...), s)
        else
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
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
    block_count = 0

    for c in children(cst)
        if kind(c) in KSet"for while" && !haschildren(c)
            add_node!(t, pretty(style, c, s), s)
        elseif kind(c) === K"end"
            add_node!(t, pretty(style, c, s), s)
        elseif kind(c) === K"block"
            s.indent += s.opts.indent
            add_node!(
                t,
                pretty(style, c, s, kwargs..., ignore_single_line = true),
                s,
                max_padding = s.opts.indent,
            )
            s.indent -= s.opts.indent
        elseif !JuliaSyntax.is_whitespace(c)
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, c, s), s, join_lines = true)
        else
            add_node!(t, pretty(style, c, s), s)
        end
    end

    t
end
p_for(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_for(DefaultStyle(style), cst, s; kwargs...)

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

    for c in children(cst)
        if kind(c) === K"do" && !haschildren(c)
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif kind(c) === K"end"
            add_node!(t, pretty(style, c, s; kwargs...), s)
        elseif kind(c) === K"block"
            s.indent += s.opts.indent
            n = p_block(style, c, s, kwargs..., ignore_single_line = true)
            if s.opts.always_use_return
                prepend_return!(n, s)
            end
            add_node!(t, n, s, max_padding = s.opts.indent)
            s.indent -= s.opts.indent
            # elseif JuliaSyntax.is_whitespace(c)
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

    for c in children(cst)
        if kind(c) in KSet"try catch finally"
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
                p_block(style, c, s, ignore_single_line = true),
                s,
                max_padding = s.opts.indent,
            )
            s.indent -= s.opts.indent
        elseif !JuliaSyntax.is_whitespace(c)
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        else
            add_node!(t, pretty(style, c, s; kwargs...), s)
        end
    end

    # @info "length" t.len
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
                p_block(style, c, s; kwargs..., ignore_single_line = true),
                s,
                max_padding = s.opts.indent,
            )
            s.indent -= s.opts.indent
        elseif !JuliaSyntax.is_whitespace(c)
            add_node!(t, Whitespace(1), s)
            @info "if" kind(c)
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
function p_chainopcall(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest = false,
    nospace = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Chain, cst, nspaces(s))

    nws = nospace ? 0 : 1
    childs = children(cst)
    for (i, a) in enumerate(childs)
        if JuliaSyntax.is_operator(a)
            add_node!(t, Whitespace(nws), s)
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            if nonest
                add_node!(t, Whitespace(nws), s)
            else
                add_node!(t, Placeholder(nws), s)
            end
        elseif is_opcall(a)
            add_node!(
                t,
                pretty(style, a, s; nospace = nospace, nonest = nonest, kwargs...),
                s,
                join_lines = true,
            )
        elseif i == length(cst) - 1 && is_punc(a) && is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s, kwargs...), s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s, kwargs...), s, join_lines = true)
        end
    end
    t
end
p_chainopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest = false,
    nospace = false,
    kwargs...,
) where {S<:AbstractStyle} = p_chainopcall(
    DefaultStyle(style),
    cst,
    s;
    nonest = nonest,
    nospace = nospace,
    kwargs...,
)

function p_comparison(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest = false,
    nospace = false,
    kwargs...,
)
    t = p_chainopcall(ds, cst, s; nonest = nonest, nospace = nospace, kwargs...)
    t.typ = Comparison
    t
end
p_comparison(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest = false,
    nospace = false,
    kwargs...,
) where {S<:AbstractStyle} =
    p_comparison(DefaultStyle(style), cst, s; nonest = nonest, nospace = nospace, kwargs...)

# Kw
function p_kw(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Kw, cst, nspaces(s))

    for c in children(cst)
        if kind(c) === K"=" && s.opts.whitespace_in_kwargs
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            n = pretty(style, c, s; kwargs...)
            if !s.opts.whitespace_in_kwargs && (
                (n.typ === IDENTIFIER && endswith(n.val, "!")) ||
                (n.typ === Call && n[1].typ === OPERATOR)
            )
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

    t
end
p_kw(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_kw(DefaultStyle(style), cst, s; kwargs...)

function p_binaryopcall(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest = false,
    nospace = false,
    from_curly = false,
    standalone_binary_circuit = true,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Binary, cst, nspaces(s))
    opkind = op_kind(cst)

    # TODO: add some metadata flag(s) to show this is a standalone_binary_circuit

    nonest = nonest || opkind === K":"

    if (from_curly && opkind in KSet"<: >:" && !s.opts.whitespace_typedefs) ||
       opkind === K":"
        nospace = true
    end
    can_separate_kwargs = !is_function_or_macro_def(cst)

    # standalone_binary_circuit =
    # @info "" opkind JuliaSyntax.is_prec_assignment(cst) is_function_or_macro_def(cst) kind(cst) can_separate_kwargs

    nodes = map(children(cst)) do c
        pretty(
            style,
            c,
            s;
            nonest = nonest,
            nospace = s.opts.whitespace_ops_in_indices ? false : nospace,
            from_curly = from_curly,
            kwargs...,
            can_separate_kwargs = can_separate_kwargs,
            standalone_binary_circuit = false,
        )
    end
    nodes = filter(n -> n.typ !== NONE, nodes)

    op = nodes[findfirst(n -> n.typ === OPERATOR, nodes)]

    if opkind === K":" &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(nodes[1]) &&
       !is_iterable(nodes[1])
        paren = FST(PUNCTUATION, -1, nodes[1].startline, nodes[1].startline, "(")
        add_node!(t, paren, s)
        add_node!(t, nodes[1], s, join_lines = true)
        paren = FST(PUNCTUATION, -1, nodes[1].startline, nodes[1].startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, nodes[1], s)
    end

    nrhs = nest_rhs(cst)
    nrhs && (t.nest_behavior = AlwaysNest)
    nest = (is_binaryop_nestable(style, cst) && !nonest) || nrhs

    if opkind === K"$"
        add_node!(t, op, s, join_lines = true)
    elseif (
        (JuliaSyntax.is_number(cst[1]) || opkind === K"^") && kind(cst) === K"dotcall"
    ) ||
           # 1 .. -2 (can be ., .., ..., etc)
           (
        JuliaSyntax.is_number(cst[end]) &&
        startswith(nodes[end].val, "-") &&
        opkind in KSet".."
    )
        add_node!(t, Whitespace(1), s)
        add_node!(t, op, s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif !(opkind in KSet"in isa ∈") &&
           (nospace || (opkind !== K"->" && opkind in KSet"⥔ :: ."))
        add_node!(t, op, s, join_lines = true)
    elseif JuliaSyntax.is_radical_op(opkind)
        add_node!(t, op, s, join_lines = true)
    else
        add_node!(t, Whitespace(1), s)
        add_node!(t, op, s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    end

    if opkind === K":" &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[end]) &&
       !is_iterable(cst[end])
        paren = FST(PUNCTUATION, -1, nodes[end].startline, nodes[end].startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(
            t,
            nodes[end],
            s,
            join_lines = true,
            override_join_lines_based_on_source = !nest,
        )
        paren = FST(PUNCTUATION, -1, nodes[end].startline, nodes[end].startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(
            t,
            nodes[end],
            s,
            join_lines = true,
            override_join_lines_based_on_source = !nest,
        )
    end

    if nest
        # for indent, will be converted to `indent` if needed
        insert!(t.nodes::Vector{FST}, length(t.nodes::Vector{FST}), Placeholder(0))
    end

    t
end
p_binaryopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest = false,
    nospace = false,
    kwargs...,
) where {S<:AbstractStyle} = p_binaryopcall(
    DefaultStyle(style),
    cst,
    s,
    nonest = nonest,
    nospace = nospace;
    kwargs...,
)

function p_whereopcall(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_curly::Bool = false,
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

    add_node!(t, pretty(style, cst[1], s, kwargs...), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s, kwargs...), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    curly_ctx = from_curly || kind(cst[3]) in KSet"curly bracescat parameters"

    add_braces =
        s.opts.surround_whereop_typeparameters && !curly_ctx && !kind(cst[3]) === K"{"

    bc = curly_ctx ? t : FST(BracesCat, nspaces(s))

    brace = FST(PUNCTUATION, -1, t.endline, t.endline, "{")
    add_braces && add_node!(bc, brace, s, join_lines = true)

    nws = s.opts.whitespace_typedefs ? 1 : 0

    for i in 3:length(children(cst))
        a = cst[i]
        if is_opener(a) && nest
            add_node!(bc, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(bc, Placeholder(0), s)
            s.indent += s.opts.indent
        elseif is_closer(a) && nest
            add_node!(bc, TrailingComma(), s)
            add_node!(bc, Placeholder(0), s)
            add_node!(bc, pretty(style, a, s), s, join_lines = true)
            s.indent -= s.opts.indent
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
            add_node!(bc, pretty(style, a, s), s, join_lines = true)
            add_node!(bc, Placeholder(nws), s)
        elseif is_binary(a)
            add_node!(
                bc,
                pretty(style, a, s, nospace = !s.opts.whitespace_typedefs),
                s,
                join_lines = true,
            )
        else
            n = pretty(style, a, s)
            add_node!(bc, n, s, join_lines = true)
        end
    end

    brace = FST(PUNCTUATION, -1, bc.endline, bc.endline, "}")
    add_braces && add_node!(bc, brace, s, join_lines = true)

    !curly_ctx && add_node!(t, bc, s, join_lines = true)

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
            add_node!(t, pretty(style, c, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, c, s), s, join_lines = true)
        end
    end

    t
end
p_conditionalopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State,
    kwargs...,
) where {S<:AbstractStyle} = p_conditionalopcall(DefaultStyle(style), cst, s; kwargs...)

function p_unaryopcall(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Unary, cst, nspaces(s))
    for c in children(cst)
        if kind(c) === K"Whitespace"
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, c, s), s, join_lines = true)
        end
    end
    t
end
p_unaryopcall(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_unaryopcall(DefaultStyle(style), cst, s, kwargs...)

function p_curly(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Curly, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    nws = s.opts.whitespace_typedefs ? 1 : 0
    if nest
        add_node!(t, Placeholder(0), s)
    end

    childs = children(cst)

    for i in 3:length(childs)
        a = childs[i]
        pretty(style, a, s; from_curly = true)

        if i == length(childs)
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"," && i < length(childs) && !is_punc(childs[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
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
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    if nest
        add_node!(t, Placeholder(0), s)
    end

    childs = children(cst)
    for i in 3:length(childs)
        a = childs[i]
        n = if kind(a) == K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        if i == length(childs)
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"," && i < length(childs) && !is_punc(childs[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
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
    arg = get_args(cst)[1]
    nest = !is_iterable(arg) && !nonest && !s.opts.disallow_single_arg_nesting

    if is_block(arg) || (kind(arg) === K"generator" && is_block(arg[1]))
        t.nest_behavior = AlwaysNest
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
                pretty(style, c, s, kwargs..., from_quote = true),
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
            add_node!(t, pretty(style, c, s), s, join_lines = true)
        end
    end

    t
end
p_invisbrackets(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    nonest::Bool = false,
    kwargs...,
) where {S<:AbstractStyle} =
    p_invisbrackets(DefaultStyle(style), cst, s; nonest = nonest, kwargs...)

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
        n = if kind(a) == K"=" && haschildren(a)
            p_kw(style, a, s; kwargs...)
        else
            pretty(style, a, s; kwargs...)
        end

        if is_opener(n)
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif is_closer(n)
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
        elseif kind(a) === K"," && i < length(childs) && !is_punc(childs[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
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

function p_braces(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Braces, cst, nspaces(s))
    nest =
        length(cst) > 2 && !(
            length(cst) == 3 &&
            (unnestable_node(cst[2]) || s.opts.disallow_single_arg_nesting)
        )

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; kwargs...)
        if i == 1
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif i == length(cst)
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
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

function p_bracescat(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(BracesCat, cst, nspaces(s))
    nest =
        length(cst) > 2 && !(
            length(cst) == 3 &&
            (unnestable_node(cst[2]) || s.opts.disallow_single_arg_nesting)
        )

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; kwargs...)
        if i == 1
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif i == length(cst)
            if nest
                add_node!(t, TrailingSemicolon(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
            if i != length(cst) - 1
                add_node!(t, Semicolon(), s)
                add_node!(t, Placeholder(1), s)
            end
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
    nest =
        length(cst) > 2 && !(
            length(cst) == 3 &&
            (unnestable_node(cst[2]) || s.opts.disallow_single_arg_nesting)
        )

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; kwargs...)
        if i == 1
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif i == length(cst)
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, n, s, join_lines = true)
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
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

    if is_block(arg) || (kind(arg) === K"generator" && is_block(arg[1]))
        t.nest_behavior = AlwaysNest
    end

    @info "" kwargs

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

function p_parameters(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Parameters, cst, nspaces(s))

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; kwargs...)
        if kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
            push!(t.nodes::Vector{FST}, n)
            push!(t.nodes::Vector{FST}, Placeholder(1))
        else
            push!(t.nodes::Vector{FST}, n)
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
                if kind(n) in KSet", :"
                    add_node!(t, Placeholder(1), s)
                end
            end
        elseif kind(a) in KSet", :"
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            n = pretty(style, a, s; kwargs...)
            add_node!(t, n, s, join_lines = true)
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

function p_as(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(As, cst, nspaces(s))

    childs = children(cst)
    for (i, c) in enumerate(childs)
        add_node!(t, pretty(style, c, s; kwargs...), s, join_lines = true)
        if !(i == length(childs) || JuliaSyntax.is_whitespace(c))
            add_node!(t, Whitespace(1), s)
        end
    end

    t
end
p_as(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_as(DefaultStyle(style), cst, s; kwargs...)

function p_ref(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(RefN, cst, nspaces(s))
    nest =
        length(cst) > 5 && !(
            length(cst) == 5 &&
            (unnestable_node(cst[3]) || s.opts.disallow_single_arg_nesting)
        )
    nospace = !s.opts.whitespace_ops_in_indices

    for (i, a) in enumerate(children(cst))
        if is_closer(a)
            if nest
                add_node!(t, TrailingComma(), s)
                add_node!(t, Placeholder(0), s)
            end
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
        elseif is_opener(a)
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif is_opcall(a)
            n = pretty(style, a, s, nonest = true, nospace = nospace; kwargs...)
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
    st = kind(cst) === K"vcat" ? 1 : 2
    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )

    childs = children(cst)
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs...)
        diff_line = t.endline != t.startline
        # If arguments are on different always nest
        diff_line && (t.nest_behavior = AlwaysNest)

        if is_opener(a)
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif is_closer(a)
            nest && add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif JuliaSyntax.is_whitespace(a)
        else
            # [a b c d e f] is semantically different from [a b c; d e f]
            # child_has_semicolon = any(aa -> kind(aa) === K";", children(a))
            # if !child_has_semicolon
            #     add_node!(t, n, s, join_lines = false)
            # else
            #     add_node!(t, n, s, join_lines = true)
            # end
            add_node!(t, n, s, join_lines = true)

            j = i + 1
            is_last_arg = false
            while j <= length(childs) && !is_last_arg
                k = kind(childs[j])
                if !JuliaSyntax.is_whitespace(k)
                    k !== K"row" && (is_last_arg = true)
                    break
                end
                j += 1
            end
            if !is_last_arg
                add_node!(t, Placeholder(1), s)
            end
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
    for (i, a) in enumerate(children(cst))
        if i > st && i < length(cst) - 1
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
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
    style = getstyle(ds)
    t = FST(Ncat, cst, nspaces(s))
    st = kind(cst) === K"ncat" ? 2 : 3
    args = get_args(cst)
    nest =
        length(args) > 0 && !(
            length(args) == 1 &&
            (unnestable_node(args[1]) || s.opts.disallow_single_arg_nesting)
        )
    childs = children(cst)
    last_was_semicolon = false
    for (i, a) in enumerate(childs)
        n = pretty(style, a, s; kwargs...)
        diff_line = t.endline != t.startline
        diff_line && (t.nest_behavior = AlwaysNest)

        if is_opener(a)
            add_node!(t, n, s, join_lines = true)
            nest && add_node!(t, Placeholder(0), s)
        elseif is_closer(a)
            nest && add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        else
            if kind(a) === K";"
                add_node!(t, n, s)
                last_was_semicolon = true
            elseif JuliaSyntax.is_whitespace(a)
            else
                if last_was_semicolon
                    add_node!(t, Placeholder(1), s)
                    last_was_semicolon = false
                end
                add_node!(t, n, s, join_lines = true)
            end
        end
    end
    t
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
    for (i, a) in enumerate(childs)
        if is_opcall(a)
            add_node!(
                t,
                pretty(style, a, s; nospace = true, nonest = true, kwargs...),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
        end

        if kind(a) === K";"
        elseif i < length(childs) &&
               !JuliaSyntax.is_whitespace(a) &&
               JuliaSyntax.is_whitespace(childs[i+1])
            # check if there's a semicolon in the future
            j = i + 2
            future_has_semicolon = false
            while j <= length(childs) && !future_has_semicolon
                k = kind(childs[j])
                if !JuliaSyntax.is_whitespace(k)
                    k === K";" && (future_has_semicolon = true)
                    break
                end
                j += 1
            end
            if !future_has_semicolon
                add_node!(t, Whitespace(1), s)
            end
        end
    end
    t.nest_behavior = NeverNest
    t
end
p_row(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_row(DefaultStyle(style), cst, s; kwargs...)

function p_nrow(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(NRow, cst, nspaces(s))
    childs = children(cst)
    last_was_semicolon = false

    for (i, a) in enumerate(childs)
        if kind(a) === K";"
            add_node!(t, pretty(style, a, s; kwargs...), s)
            last_was_semicolon = true
        elseif JuliaSyntax.is_whitespace(a)
            pretty(style, a, s; kwargs...)
        else
            if last_was_semicolon
                if i < length(childs) - 1
                    add_node!(t, Whitespace(1), s)
                end
                last_was_semicolon = false
            end

            if is_opcall(a)
                add_node!(
                    t,
                    pretty(style, a, s; kwargs..., nospace = true, nonest = true),
                    s,
                    join_lines = true,
                )
            else
                add_node!(t, pretty(style, a, s; kwargs...), s, join_lines = true)
            end
        end
    end

    t.nest_behavior = NeverNest
    t
end
p_nrow(style::S, cst::JuliaSyntax.GreenNode, s::State; kwargs...) where {S<:AbstractStyle} =
    p_nrow(DefaultStyle(style), cst, s; kwargs...)

function p_generator(
    ds::DefaultStyle,
    cst::JuliaSyntax.GreenNode,
    s::State;
    from_iterable::Bool = false,
    kwargs...,
)
    style = getstyle(ds)
    t = FST(Generator, cst, nspaces(s))
    has_for_kw = false

    for (i, a) in enumerate(children(cst))
        n = pretty(style, a, s; kwargs...)
        if JuliaSyntax.is_keyword(a)
            if kind(a) === K"for"
                has_for_kw = true
            end

            # for keyword can only be on the following line
            # if this expression is within an iterable expression
            if kind(a) === K"for" && from_iterable
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end

            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif kind(a) === K"," && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
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

function p_flatten(ds::DefaultStyle, cst::JuliaSyntax.GreenNode, s::State; kwargs...)
    t = p_generator(ds, cst, s; kwargs...)
    t.typ = Flatten
    t
end
p_flatten(
    style::S,
    cst::JuliaSyntax.GreenNode,
    s::State;
    kwargs...,
) where {S<:AbstractStyle} = p_flatten(DefaultStyle(style), cst, s; kwargs...)
