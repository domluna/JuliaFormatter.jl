# YAS style !!!

struct YASStyle <: AbstractStyle end
@inline getstyle(s::YASStyle) = s

yasformat(s::AbstractString; kwargs...) = format_text(
    s;
    kwargs...,
    always_for_in = true,
    whitespace_ops_in_indices = true,
    whitespace_typedefs = false,
    style = YASStyle(),
)

pretty(style::YASStyle, cst::CSTParser.EXPR, s::State; kwargs...) =
    pretty(style, nodetype(cst), cst, s; kwargs...)
pretty(
    style::YASStyle,
    node::T,
    cst::CSTParser.EXPR,
    s::State;
    kwargs...,
) where {T<:AbstractFormatNode} = pretty(DefaultStyle(style), node, cst, s; kwargs...)

function nestable(::YASStyle, cst::CSTParser.EXPR)
    # CSTParser.defines_function(cst) && cst[1].typ !== CSTParser.UnaryOpCall && return true
    return false
    # nest_assignment(cst) && return false
    # true
end

function pretty(style::YASStyle, ::Kw, cst::CSTParser.EXPR, s::State)
    t = FST{Kw}(cst, nspaces(s))
    for a in cst
        add_node!(t, pretty(style, a, s), s, join_lines = true)
    end
    t
end

function pretty(
    style::YASStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Curly,Braces}}
    t = FST{T}(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i == length(cst) - 1
            # remove trailing comma
        elseif is_closer(n) || is_opener(n)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = t.endline == n.startline)
        end
    end
    t
end

function pretty(
    style::YASStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Call,RefFN,TupleFN,Vect,Parameters,Comprehension}}
    t = FST{T}(cst, nspaces(s))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i == length(cst) - 1
            # remove trailing comma
        elseif is_closer(n) || is_opener(n)
            add_node!(t, n, s, join_lines = true)
        elseif n isa FST{Parameters}
            join_lines = t.endline == n.startline
            add_node!(t, n, s, join_lines = join_lines)
        else
            join_lines = t.endline == n.startline
            if join_lines && !is_opener(t[end])
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, n, s, join_lines = join_lines)
        end
    end
    t
end

function pretty(style::YASStyle, ::Whereopcall, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST{Whereopcall}(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    add_braces =
        !CSTParser.is_lbrace(cst[3]) && cst.parent.typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.Curly && cst[3].typ !== CSTParser.BracesCat

    brace = FST{Punctuation}(t.endline, t.endline, "{")
    add_braces && add_node!(t, brace, s, join_lines = true)

    for (i, a) in enumerate(cst.args[3:end])
        n = a.typ === CSTParser.BinaryOpCall ? pretty(style, a, s, nospace = true) :
            pretty(style, a, s)
        if is_closer(n) || is_opener(n)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && !is_punc(cst[i+3])
            add_node!(t, n, s, join_lines = true)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(t, n, s, join_lines = t.endline == n.startline)
        else
            add_node!(t, n, s, join_lines = t.endline == n.startline)
        end
    end
    brace = FST{Punctuation}(t.endline, t.endline, "}")
    add_braces && add_node!(t, brace, s, join_lines = true)
    t
end

function pretty(
    style::YASStyle,
    ::T,
    cst::CSTParser.EXPR,
    s::State,
) where {T<:Union{Generator,Filter}}
    t = FST{T}(cst, nspaces(s))
    for (i, a) in enumerate(cst)
        n = a.typ === CSTParser.BinaryOpCall ? pretty(style, a, s, nonest = true) :
            pretty(style, a, s)
        if a.typ === CSTParser.KEYWORD
            add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
            # add_node!(t, Whitespace(1), s)
            if a.kind === Tokens.FOR
                for j = i+1:length(cst)
                    eq_to_in_normalization!(cst[j], s.opts.always_for_in)
                end
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
        else
            join_lines = t.endline == n.startline
            join_lines && add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = join_lines)
        end
    end
    t
end

#
# Nesting
#

nest!(style::YASStyle, fst::FST{T}, s::State) where {T<:AbstractFormatNode} =
    nest!(DefaultStyle(style), fst, s)
nest!(style::YASStyle, nodes::Vector{FST}, s::State, indent::Int; kwargs...) =
    nest!(DefaultStyle(style), nodes, s, indent; kwargs...)

function nest_if_over_margin!(style, fst::FST, s::State, i::Int)
    margin = s.line_offset
    idx = findnext(is_placeholder, fst.nodes, i + 1)
    if idx === nothing
        margin += sum(length.(fst[i+1:end])) + fst.extra_margin
    else
        margin += sum(length.(fst[i+1:idx]))
    end
    if margin > s.margin || is_comment(fst[i+1])
        fst[i] = Newline(length = fst[i].len)
        s.line_offset = fst.indent
    else
        nest!(style, fst[i], s)
    end
end

function nest!(style::YASStyle, fst::FST{T}, s::State) where {T<:Union{Call,Curly,RefFN}}
    line_offset = s.line_offset
    fst.indent = line_offset + sum(length.(fst[1:2]))

    for (i, n) in enumerate(fst.nodes)
        if n isa FST{NEWLINE}
            s.line_offset = fst.indent
        elseif n isa FST{TRAILINGSEMICOLON}
            n.val = ""
            n.len = 0
            nest!(style, n, s)
        elseif n isa FST{Parameters}
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(style, n, s)
        elseif n isa FST{Generator}
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(style, n, s)
        else
            n.extra_margin = 1
            nest!(style, n, s)
        end
    end
end

function nest!(
    style::YASStyle,
    fst::FST{T},
    s::State,
) where {T<:Union{TupleFN,Braces,Vect,Parameters,Invisbrackets,Comprehension}}
    line_offset = s.line_offset
    !(fst isa FST{Parameters}) && (fst.indent = line_offset)
    length(fst.nodes) > 0 && is_opener(fst[1]) && (fst.indent += 1)

    for (i, n) in enumerate(fst.nodes)
        if n isa FST{NEWLINE}
            s.line_offset = fst.indent
        elseif n isa FST{TRAILINGSEMICOLON}
            n.val = ""
            n.len = 0
            nest!(style, n, s)
        elseif n isa FST{Generator}
            n.indent = fst.indent
            n.extra_margin = 1
            nest!(style, n, s)
        else
            n.extra_margin = 1
            nest!(style, n, s)
        end
    end
end

function nest!(style::YASStyle, fst::FST{T}, s::State) where {T<:Union{Import,Export,Using}}
    fst.indent = s.line_offset + sum(length.(fst[1:2]))
    for (i, n) in enumerate(fst.nodes)
        if n isa FST{PLACEHOLDER}
            nest_if_over_margin!(style, fst, s, i)
        elseif n isa FST{NEWLINE}
            s.line_offset = fst.indent
        else
            nest!(style, n, s)
        end
    end
end

nest!(style::YASStyle, fst::FST{Whereopcall}, s::State) =
    nest!(style, fst.nodes, s, fst.indent, extra_margin = fst.extra_margin)
nest!(style::YASStyle, fst::FST{Chainopcall}, s::State) =
    n_block!(DefaultStyle(style), fst, s, custom_indent = s.line_offset)
