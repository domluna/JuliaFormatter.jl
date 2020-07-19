"""
    DocumentFormatStyle()

Formatting style based on DocumentFormat.jl
"""
struct DocumentFormatStyle <: AbstractStyle
    indentstyle::AbstractStyle
end
@inline getstyle(s::DocumentFormatStyle) = s

function nestable(::DocumentFormatStyle, cst::CSTParser.EXPR)
    (CSTParser.defines_function(cst) || nest_assignment(cst)) && return false
    (cst[2].kind === Tokens.PAIR_ARROW || cst[2].kind === Tokens.ANON_FUNC) && return false
    return true
end

function p_import(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(df, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for (i, a) in enumerate(cst.args[2:end])
        if CSTParser.is_comma(a)
            add_node!(t, pretty(df, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif CSTParser.is_colon(a)
            add_node!(t, pretty(df, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            n = pretty(df, a, s)
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end
@inline p_using(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_import(df, cst, s)
@inline p_export(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_import(df, cst, s)

function p_curly(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(df, a, s)
        if CSTParser.is_comma(a) && i == length(cst) - 1
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    t
end
@inline p_braces(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_curly(df, cst, s)

function p_tupleh(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            n = pretty(df, a, s)
            if n_args(cst) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(cst[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(df, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif a.typ === CSTParser.BinaryOpCall && a[2].kind === Tokens.EQ
            n = p_kw(y, a, s)
            jl = n.startline == t.endline
            add_node!(t, n, s, join_lines = jl)
        else
            n = pretty(df, a, s)
            jl = n.startline == t.endline
            add_node!(t, n, s, join_lines = jl)
        end
    end
    remove_superflous_whitespace!(t)
    t
end

function p_tupleh(df::DocumentFormatStyle, nodes::Vector{CSTParser.EXPR}, s::State)
    t = FST(CSTParser.TupleH, nspaces(s))
    for (i, a) in enumerate(nodes)
        n = pretty(df, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(nodes)
            if n_args(nodes) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(nodes[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(nodes) && !is_punc(nodes[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end

# InvisBrackets
function p_invisbrackets(
    df::DocumentFormatStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    t = p_invisbrackets(DefaultStyle(df), cst, s, nonest = nonest, nospace = nospace)
    for (i, n) in enumerate(t.nodes)
        if n.typ === PLACEHOLDER
            t[i] = Whitespace(length(n))
        end
    end
    t
end

function p_call(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(df, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end
@inline p_vect(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) = p_call(df, cst, s)

function p_ref(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices

    for (i, a) in enumerate(cst)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(df, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif a.typ === CSTParser.BinaryOpCall ||
               a.typ === CSTParser.InvisBrackets ||
               a.typ === CSTParser.ChainOpCall ||
               a.typ === CSTParser.Comparison

            n = pretty(df, a, s, nonest = true, nospace = nospace)
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        else
            n = pretty(df, a, s)
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end

function p_comprehension(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    if is_block(cst[2])
        t.force_nest = true
    elseif cst[2].typ === CSTParser.Generator && is_block(cst[2][1])
        t.force_nest = true
    end

    add_node!(t, pretty(df, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(df, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(df, cst[3], s), s, join_lines = true)
    t
end

function p_typedcomprehension(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    if is_block(cst[3])
        t.force_nest = true
    elseif cst[3].typ === CSTParser.Generator && is_block(cst[3][1])
        t.force_nest = true
    end

    add_node!(t, pretty(df, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(df, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(df, cst[3], s), s, join_lines = true)
    add_node!(t, pretty(df, cst[4], s), s, join_lines = true)
    t
end

function p_macrocall(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    has_closer = is_closer(cst.args[end])

    !has_closer && (t.typ = MacroBlock)

    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(cst)
        n = pretty(df, a, s)
        if a.typ === CSTParser.MacroName
            if a.fullspan - a.span > 0 && length(cst) > 1
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            else
                # assumes the next argument is a brace of some sort
                add_node!(t, n, s, join_lines = true)
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif a.fullspan - a.span > 0
            if has_closer
                add_node!(t, n, s, join_lines = n.startline == t.endline)
                if i < length(cst) - 1 && cst[i+1].typ !== CSTParser.Parameters
                    add_node!(t, Whitespace(1), s)
                end
            else
                padding = is_block(n) ? 0 : -1
                add_node!(t, n, s, join_lines = true, max_padding = padding)
                i < length(cst) && add_node!(t, Whitespace(1), s)
            end
        else
            if has_closer
                add_node!(t, n, s, join_lines = n.startline == t.endline)
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

    remove_superflous_whitespace!(t)

    t
end

function p_parameters(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(df, a, s)
        if i == length(cst) && CSTParser.is_comma(a)
            # do nothing
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end

function p_whereopcall(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(df, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(df, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    add_braces =
        !CSTParser.is_lbrace(cst[3]) &&
        cst.parent.typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.BracesCat

    brace = FST(CSTParser.PUNCTUATION, t.endline, t.endline, "{")
    add_braces && add_node!(t, brace, s, join_lines = true)

    for (i, a) in enumerate(cst.args[3:end])
        n = a.typ === CSTParser.BinaryOpCall ? pretty(df, a, s, nospace = true) :
            pretty(df, a, s)

        if CSTParser.is_comma(a) && i + 2 == length(cst) - 1
            continue
        elseif CSTParser.is_comma(a) && i + 2 < length(cst) && !is_punc(cst[i+3])
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = t.endline == n.startline)
        end
    end

    brace = FST(CSTParser.PUNCTUATION, t.endline, t.endline, "}")
    add_braces && add_node!(t, brace, s, join_lines = true)
    t
end

function p_generator(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    for (i, a) in enumerate(cst)
        n = pretty(df, a, s)
        if a.typ === CSTParser.KEYWORD
            incomp = parent_is(
                a,
                is_iterable,
                ignore = n -> is_gen(n) || n.typ === CSTParser.InvisBrackets,
            )

            if is_block(cst[i-1])
                add_node!(t, Newline(), s)
            elseif incomp
                add_node!(t, Whitespace(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s, join_lines = t.endline == n.startline)
            add_node!(t, Whitespace(1), s)

            if a.kind === Tokens.FOR
                for j = i+1:length(cst)
                    eq_to_in_normalization!(cst[j], s.opts.always_for_in)
                end
            end

            if !is_gen(cst.args[i+1])
                tup = p_tupleh(df, cst.args[i+1:length(cst)], s)
                add_node!(t, tup, s, join_lines = t.endline == n.startline)
                return t
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, n, s, join_lines = t.endline == n.startline)
        end
    end

    t
end
@inline p_filter(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_generator(df, cst, s)
@inline p_flatten(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_generator(df, cst, s)

function p_binaryopcall(
    df::DocumentFormatStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(df)
    t = FST(cst, nspaces(s))
    op = cst[2]
    nonest = nonest || op.kind === Tokens.COLON
    if cst.parent.typ === CSTParser.Curly &&
       op.kind in (Tokens.ISSUBTYPE, Tokens.ISSUPERTYPE) &&
       !s.opts.whitespace_typedefs
        nospace = true
    elseif op.kind === Tokens.COLON
        nospace = true
    end
    nospace_args = s.opts.whitespace_ops_in_indices ? false : nospace

    if is_opcall(cst[1])
        n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[1], s)
    end

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[1]) &&
       !is_iterable(cst[1])

        paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, "(")
        add_node!(t, paren, s)
        add_node!(t, n, s, join_lines = true)
        paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s)
    end

    if op.fullspan == 0
        # Do nothing - represents a binary op with no textual representation.
        # For example: `2a`, which is equivalent to `2 * a`.
    elseif op.kind === Tokens.EX_OR
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif op.kind === Tokens.CIRCUMFLEX_ACCENT && op.dot
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        add_node!(t, Whitespace(1), s)
    elseif (
        nospace ||
        (CSTParser.precedence(op) in (8, 13, 14, 16) && op.kind !== Tokens.ANON_FUNC)
    ) && op.kind !== Tokens.IN
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    else
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        add_node!(t, Whitespace(1), s)
    end

    if is_opcall(cst[3])
        n = pretty(style, cst[3], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[3], s)
    end

    join_lines = t.endline == n.startline

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[3]) &&
       !is_iterable(cst[3])
        paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(t, n, s, join_lines = join_lines)
        paren = FST(CSTParser.PUNCTUATION, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s, join_lines = join_lines)
    end

    if !join_lines
        # for indent, will be converted to `indent_size` if needed
        insert!(t.nodes, length(t.nodes), Placeholder(0))
    end

    remove_superflous_whitespace!(t)
    t
end

# Conditional
function p_conditionalopcall(df::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(df, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(df, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    n = pretty(df, cst[3], s)
    add_node!(t, n, s, join_lines = n.startline == t.endline)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(df, cst[4], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    n = pretty(df, cst[5], s)
    add_node!(t, n, s, join_lines = n.startline == t.endline)

    remove_superflous_whitespace!(t)
    t
end

# ChainOpCall/Comparison
function p_chainopcall(
    df::DocumentFormatStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    t = FST(cst, nspaces(s))
    nws = nospace ? 0 : 1
    for (i, a) in enumerate(cst)
        n = pretty(df, a, s)
        if a.typ === CSTParser.OPERATOR
            !nospace && add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(nws), s)
        elseif i == length(cst) - 1 && is_punc(a) && is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end
