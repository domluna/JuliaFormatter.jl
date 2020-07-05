"""
    DocumentFormatStyle()

Formatting style based on DocumentFormat.jl
"""
struct DocumentFormatStyle <: AbstractStyle end
@inline getstyle(s::DocumentFormatStyle) = s

function nestable(::DocumentFormatStyle, cst::CSTParser.EXPR)
    (CSTParser.defines_function(cst) || nest_assignment(cst)) && return false
    (cst[2].kind === Tokens.PAIR_ARROW || cst[2].kind === Tokens.ANON_FUNC) && return false
    return true
end

function p_import(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(ds, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for (i, a) in enumerate(cst.args[2:end])
        if CSTParser.is_comma(a)
            add_node!(t, pretty(ds, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif CSTParser.is_colon(a)
            add_node!(t, pretty(ds, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            n = pretty(ds, a, s)
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end
@inline p_using(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_import(ds, cst, s)
@inline p_export(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_import(ds, cst, s)

function p_curly(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(ds, a, s)
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
@inline p_braces(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_curly(ds, cst, s)

function p_tupleh(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            n = pretty(ds, a, s)
            if n_args(cst) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(cst[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(ds, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif is_closer(a)
            add_node!(t, pretty(ds, a, s), s, join_lines = true)
        elseif a.typ === CSTParser.BinaryOpCall && a[2].kind === Tokens.EQ
            n = p_kw(y, a, s)
            jl = n.startline == t.endline
            add_node!(t, n, s, join_lines = jl)
        else
            n = pretty(ds, a, s)
            jl = n.startline == t.endline
            add_node!(t, n, s, join_lines = jl)
        end
    end
    remove_superflous_whitespace!(t)
    t
end

function p_tupleh(ds::DocumentFormatStyle, nodes::Vector{CSTParser.EXPR}, s::State)
    t = FST(CSTParser.TupleH, nspaces(s))
    for (i, a) in enumerate(nodes)
        n = pretty(ds, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(nodes)
            if n_args(nodes) == 1
                add_node!(t, n, s, join_lines = true)
            elseif !is_closer(nodes[i+1])
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            end
        elseif is_closer(a)
            add_node!(t, n, s, join_lines = true)
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
    ds::DocumentFormatStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    t = p_invisbrackets(DefaultStyle(ds), cst, s, nonest = nonest, nospace = nospace)
    for (i, n) in enumerate(t.nodes)
        if n.typ === PLACEHOLDER
            t[i] = Whitespace(length(n))
        end
    end
    t
end

function p_call(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(ds, a, s)
        if CSTParser.is_comma(a) && i + 1 == length(cst)
            continue
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif is_closer(a)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end
@inline p_vect(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) = p_call(ds, cst, s)

function p_ref(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices

    for (i, a) in enumerate(cst)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(ds, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        elseif a.typ === CSTParser.BinaryOpCall ||
               a.typ === CSTParser.InvisBrackets ||
               a.typ === CSTParser.ChainOpCall ||
               a.typ === CSTParser.Comparison

            n = pretty(ds, a, s, nonest = true, nospace = nospace)
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        else
            n = pretty(ds, a, s)
            add_node!(t, n, s, join_lines = n.startline == t.endline)
        end
    end
    remove_superflous_whitespace!(t)
    t
end

function p_comprehension(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    if is_block(cst[2])
        t.force_nest = true
    elseif cst[2].typ === CSTParser.Generator && is_block(cst[2][1])
        t.force_nest = true
    end

    add_node!(t, pretty(ds, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(ds, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(ds, cst[3], s), s, join_lines = true)
    t
end

function p_typedcomprehension(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    if is_block(cst[3])
        t.force_nest = true
    elseif cst[3].typ === CSTParser.Generator && is_block(cst[3][1])
        t.force_nest = true
    end

    add_node!(t, pretty(ds, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(ds, cst[2], s), s, join_lines = true)
    add_node!(t, pretty(ds, cst[3], s), s, join_lines = true)
    add_node!(t, pretty(ds, cst[4], s), s, join_lines = true)
    t
end

function p_macrocall(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_arg(args[1]))
    has_closer = is_closer(cst.args[end])

    !has_closer && (t.typ = MacroBlock)

    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(cst)
        n = pretty(ds, a, s)
        if a.typ === CSTParser.MacroName
            if a.fullspan - a.span > 0 && length(cst) > 1
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            else
                # assumes the next argument is a brace of some sort
                add_node!(t, n, s, join_lines = true)
            end
        elseif is_opener(n) && nest
            add_node!(t, n, s, join_lines = true)
        elseif is_closer(n) && nest
            add_node!(t, n, s, join_lines = true)
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

function p_parameters(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(ds, a, s)
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

function p_whereopcall(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(ds, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(ds, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    add_braces =
        !CSTParser.is_lbrace(cst[3]) &&
        cst.parent.typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.BracesCat

    brace = FST(CSTParser.PUNCTUATION, t.endline, t.endline, "{")
    add_braces && add_node!(t, brace, s, join_lines = true)

    for (i, a) in enumerate(cst.args[3:end])
        n = a.typ === CSTParser.BinaryOpCall ? pretty(ds, a, s, nospace = true) :
            pretty(ds, a, s)

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

function p_generator(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))

    for (i, a) in enumerate(cst)
        n = pretty(ds, a, s)
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
                    eq_to_in_normalization!(cst[j], s.opts.alwads_for_in)
                end
            end

            if !is_gen(cst.args[i+1])
                tup = p_tupleh(ds, cst.args[i+1:length(cst)], s)
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
@inline p_filter(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_generator(ds, cst, s)
@inline p_flatten(ds::DocumentFormatStyle, cst::CSTParser.EXPR, s::State) =
    p_generator(ds, cst, s)

#
# Nesting
#

function n_call!(ds::DocumentFormatStyle, fst::FST, s::State)
    fst.indent = s.line_offset + sum(length.(fst[1:2]))

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === TRAILINGSEMICOLON
            n.val = ""
            n.len = 0
            nest!(ds, n, s)
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(ds, n, s)
        else
            diff = fst.indent - fst[i].indent
            add_indent!(n, s, diff)
            n.extra_margin = 1
            nest!(ds, n, s)
        end
    end
end
@inline n_curly!(ds::DocumentFormatStyle, fst::FST, s::State) = n_call!(ds, fst, s)
@inline n_ref!(ds::DocumentFormatStyle, fst::FST, s::State) = n_call!(ds, fst, s)
@inline n_macrocall!(ds::DocumentFormatStyle, fst::FST, s::State) = n_call!(ds, fst, s)
@inline n_typedcomprehension!(ds::DocumentFormatStyle, fst::FST, s::State) =
    n_call!(ds, fst, s)

function n_tupleh!(ds::DocumentFormatStyle, fst::FST, s::State)
    fst.indent = s.line_offset
    length(fst.nodes) > 0 && is_opener(fst[1]) && (fst.indent += 1)

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif n.typ === TRAILINGSEMICOLON
            n.val = ""
            n.len = 0
            nest!(ds, n, s)
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(ds, n, s)
        else
            diff = fst.indent - fst[i].indent
            add_indent!(n, s, diff)
            n.extra_margin = 1
            nest!(ds, n, s)
        end
    end
end
@inline n_braces!(ds::DocumentFormatStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
@inline n_vect!(ds::DocumentFormatStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
@inline n_parameters!(ds::DocumentFormatStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
@inline n_invisbrackets!(ds::DocumentFormatStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)
@inline n_comprehension!(ds::DocumentFormatStyle, fst::FST, s::State) = n_tupleh!(ds, fst, s)

function n_generator!(ds::DocumentFormatStyle, fst::FST, s::State)
    diff = s.line_offset - fst[1].indent

    # if the first argument is not a leaf
    # aligns it to be inside the generator
    # expression
    add_indent!(fst[1], s, diff)

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif is_gen(n)
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(ds, n, s)
        else
            n.extra_margin = 1
            nest!(ds, n, s)
        end
    end
end
@inline n_filter!(ds::DocumentFormatStyle, fst::FST, s::State) = n_generator!(ds, fst, s)
@inline n_flatten!(ds::DocumentFormatStyle, fst::FST, s::State) = n_generator!(ds, fst, s)

function n_whereopcall!(ds::DocumentFormatStyle, fst::FST, s::State)
    fst.indent = s.line_offset
    # after "A where "
    Blen = sum(length.(fst[2:end]))
    fst[1].extra_margin = Blen + fst.extra_margin

    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        elseif is_opener(n) && n.val == "{"
            fst.indent = s.line_offset + 1
            nest!(ds, n, s)
        elseif i == 1
            nest!(ds, n, s)
        else
            n.extra_margin = 1
            nest!(ds, n, s)
        end
    end
end

function n_using!(ds::DocumentFormatStyle, fst::FST, s::State)
    idx = findfirst(n -> n.val == ":", fst.nodes)
    fst.indent = s.line_offset
    if idx === nothing
        fst.indent += sum(length.(fst[1:2]))
    else
        fst.indent += sum(length.(fst[1:idx+1]))
    end
    for (i, n) in enumerate(fst.nodes)
        if n.typ === NEWLINE
            s.line_offset = fst.indent
        else
            nest!(ds, n, s)
        end
    end
end
@inline n_export!(ds::DocumentFormatStyle, fst::FST, s::State) = n_using!(ds, fst, s)
@inline n_import!(ds::DocumentFormatStyle, fst::FST, s::State) = n_using!(ds, fst, s)

n_chainopcall!(ds::DocumentFormatStyle, fst::FST, s::State) =
    n_block!(DefaultStyle(ds), fst, s, indent = s.line_offset)
n_comparison!(ds::DocumentFormatStyle, fst::FST, s::State) =
    n_block!(DefaultStyle(ds), fst, s, indent = s.line_offset)

function n_binaryopcall!(ds::DocumentFormatStyle, fst::FST, s::State)
    idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)

    if idx !== nothing
        n_binaryopcall!(DefaultStyle(ds), fst, s)
        return
    end

    walk(reset_line_offset!, fst.nodes[1:end-1], s, fst.indent)
    nest!(ds, fst[end], s)
end
