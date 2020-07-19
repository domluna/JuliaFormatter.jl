"""
`FNode` is a node used for formatting which does not have a `CSTParser` equivalent.
"""
@enum(
    FNode,

    # leaf nodes
    NEWLINE,
    SEMICOLON,
    WHITESPACE,
    PLACEHOLDER,
    NOTCODE,
    INLINECOMMENT,
    TRAILINGCOMMA,
    TRAILINGSEMICOLON,
    INVERSETRAILINGSEMICOLON,

    # no equivalent in CSTParser
    MacroBlock,
)

# nesting
# DEFAULT
# NEST
# UNNEST
# @enum()

"""
Formatted Syntax Tree
"""
mutable struct FST
    typ::Union{CSTParser.Head,FNode}

    # Start and end lines of the node
    # in the original source file.
    startline::Int
    endline::Int
    indent::Int

    len::Int
    val::Union{Nothing,AbstractString}
    nodes::Union{Nothing,Vector{FST}}
    ref::Union{Nothing,Ref{CSTParser.EXPR}}
    force_nest::Bool

    # Extra margin caused by parent nodes.
    # i.e. `(f(arg))`
    #
    # `f(arg)` would have `extra_margin` = 1
    # due to `)` after `f(arg)`.
    extra_margin::Int
end

FST(cst::CSTParser.EXPR, indent::Integer) =
    FST(cst.typ, -1, -1, indent, 0, nothing, FST[], Ref(cst), false, 0)

function FST(cst::CSTParser.EXPR, startline::Integer, endline::Integer, val::AbstractString)
    FST(cst.typ, startline, endline, 0, length(val), val, nothing, Ref(cst), false, 0)
end

function FST(typ::CSTParser.Head, startline::Integer, endline::Integer, val::AbstractString)
    FST(typ, startline, endline, 0, length(val), val, nothing, nothing, false, 0)
end

FST(typ::CSTParser.Head, indent::Integer) =
    FST(typ, -1, -1, indent, 0, nothing, FST[], nothing, false, 0)

@inline function Base.setindex!(fst::FST, node::FST, ind::Int)
    fst.len -= fst.nodes[ind].len
    fst.nodes[ind] = node
    fst.len += node.len
end
@inline Base.getindex(fst::FST, inds...) = fst.nodes[inds...]
@inline Base.lastindex(fst::FST) = length(fst.nodes)

@inline Newline(; length = 0, force_nest = false) =
    FST(NEWLINE, -1, -1, 0, length, "\n", nothing, nothing, force_nest, 0)
@inline Semicolon() = FST(SEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, false, 0)
@inline TrailingComma() = FST(TRAILINGCOMMA, -1, -1, 0, 0, "", nothing, nothing, false, 0)
@inline TrailingSemicolon() =
    FST(TRAILINGSEMICOLON, -1, -1, 0, 1, "", nothing, nothing, false, 0)
@inline InverseTrailingSemicolon() =
    FST(INVERSETRAILINGSEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, false, 0)
@inline Whitespace(n) = FST(WHITESPACE, -1, -1, 0, n, " "^n, nothing, nothing, false, 0)
@inline Placeholder(n) = FST(PLACEHOLDER, -1, -1, 0, n, " "^n, nothing, nothing, false, 0)
@inline Notcode(startline, endline) =
    FST(NOTCODE, startline, endline, 0, 0, "", nothing, nothing, false, 0)
@inline InlineComment(line) =
    FST(INLINECOMMENT, line, line, 0, 0, "", nothing, nothing, false, 0)

@inline Base.length(fst::FST) = fst.len

@inline is_leaf(cst::CSTParser.EXPR) = cst.args === nothing
@inline is_leaf(fst::FST) = fst.nodes === nothing

@inline is_punc(cst::CSTParser.EXPR) = CSTParser.ispunctuation(cst)
@inline is_punc(fst::FST) = fst.typ === CSTParser.PUNCTUATION
@inline is_end(x) = x.typ === CSTParser.KEYWORD && x.val == "end"
@inline is_colon(x) = x.typ === CSTParser.OPERATOR && x.val == ":"
@inline is_comma(fst::FST) =
    (fst.typ === CSTParser.PUNCTUATION && fst.val == ",") || fst.typ === TRAILINGCOMMA
@inline is_comment(fst::FST) = fst.typ === INLINECOMMENT || fst.typ === NOTCODE

@inline is_colon_op(cst::CSTParser.EXPR) =
    (cst.typ === CSTParser.BinaryOpCall && cst[2].kind === Tokens.COLON) ||
    cst.typ === CSTParser.ColonOpCall

function is_multiline(fst::FST)
    fst.typ === CSTParser.StringH && return true
    if fst.typ === CSTParser.x_Str && fst[2].typ === CSTParser.StringH
        return true
    elseif fst.typ === CSTParser.x_Cmd && fst[2].typ === CSTParser.StringH
        return true
    elseif fst.typ === CSTParser.Vcat && fst.endline > fst.startline
        return true
    elseif fst.typ === CSTParser.TypedVcat && fst.endline > fst.startline
        return true
    end
    false
end

function is_importer_exporter(fst::FST)
    fst.typ === CSTParser.Import && return true
    fst.typ === CSTParser.Export && return true
    fst.typ === CSTParser.Using && return true
    return false
end

function is_macrodoc(cst::CSTParser.EXPR)
    return cst.typ === CSTParser.MacroCall &&
           length(cst) == 3 &&
           cst[1].typ === CSTParser.MacroName &&
           cst[1][2].val == "doc" &&
           is_str(cst[2])
end

# f a function which returns a bool
function parent_is(cst::CSTParser.EXPR, valid; ignore = (n) -> false)
    p = cst.parent
    p === nothing && return false
    while p !== nothing && ignore(p)
        p = p.parent
    end
    valid(p)
end

function contains_comment(fst::FST)
    is_leaf(fst) && return false
    findfirst(is_comment, fst.nodes) !== nothing
end

# TODO: Remove once this is fixed in CSTParser.
# https://github.com/julia-vscode/CSTParser.jl/issues/108
function get_args(cst::CSTParser.EXPR)
    if cst.typ === CSTParser.MacroCall ||
       cst.typ === CSTParser.TypedVcat ||
       cst.typ === CSTParser.Ref ||
       cst.typ === CSTParser.Curly ||
       cst.typ === CSTParser.Call
        return get_args(cst.args[2:end])
    elseif cst.typ === CSTParser.WhereOpCall
        # get the arguments in B of `A where B`
        return get_args(cst.args[3:end])
    elseif cst.typ === CSTParser.Braces ||
           cst.typ === CSTParser.Vcat ||
           cst.typ === CSTParser.BracesCat ||
           cst.typ === CSTParser.TupleH ||
           cst.typ === CSTParser.Vect ||
           cst.typ === CSTParser.InvisBrackets ||
           cst.typ === CSTParser.Parameters
        return get_args(cst.args)
    end
    CSTParser.get_args(cst)
end

function get_args(args::Vector{CSTParser.EXPR})
    args0 = CSTParser.EXPR[]
    for a in args
        CSTParser.ispunctuation(a) && continue
        if CSTParser.typof(a) === CSTParser.Parameters
            for j = 1:length(a.args)
                parg = a[j]
                CSTParser.ispunctuation(parg) && continue
                push!(args0, parg)
            end
        else
            push!(args0, a)
        end
    end
    args0
end
@inline n_args(x) = length(get_args(x))

function add_node!(t::FST, n::FST, s::State; join_lines = false, max_padding = -1)
    if n.typ === SEMICOLON
        join_lines = true
        loc = s.offset > length(s.doc.text) && t.typ === CSTParser.TopLevel ?
            cursor_loc(s, s.offset - 1) : cursor_loc(s)
        for l = t.endline:loc[1]
            if has_semicolon(s.doc, l)
                n.startline = l
                n.endline = l
                break
            end
        end

        # If there's no semicolon, treat it
        # as a FNode
        if n.startline == -1
            t.len += length(n)
            n.startline = t.endline
            n.endline = t.endline
            push!(t.nodes, n)
            return
        end
    elseif n.typ === TRAILINGCOMMA
        en = t.nodes[end]
        if en.typ === CSTParser.Generator ||
           en.typ === CSTParser.Filter ||
           en.typ === CSTParser.Flatten ||
           en.typ === CSTParser.MacroCall ||
           en.typ === MacroBlock ||
           (is_comma(en) && t.typ === CSTParser.TupleH && n_args(t.ref[]) == 1)
            # don't insert trailing comma in these cases
        elseif is_comma(en)
            t.nodes[end] = n
            t.len -= 1
        else
            t.len += length(n)
            n.startline = t.startline
            n.endline = t.endline
            push!(t.nodes, n)
        end
        return
    elseif n.typ === NOTCODE
        n.indent = s.indent
        push!(t.nodes, n)
        return
    elseif n.typ === INLINECOMMENT
        push!(t.nodes, n)
        return
    elseif n.typ isa FNode && is_leaf(n)
        t.len += length(n)
        n.startline = t.startline
        n.endline = t.endline
        push!(t.nodes, n)
        return
    end

    if n.typ === CSTParser.Block && length(n) == 0
        push!(t.nodes, n)
        return
    elseif n.typ === CSTParser.Parameters
        # unpack Parameters arguments into the parent node
        if n_args(t.ref[]) == n_args(n.ref[])
            # There are no arguments prior to params
            # so we can remove the initial placeholder.
            idx = findfirst(n -> n.typ === PLACEHOLDER, t.nodes)
            idx !== nothing && (t[idx] = Whitespace(0))
        end
        add_node!(t, Semicolon(), s)

        if length(n.nodes) > 0
            nws = 1
            if (t.typ === CSTParser.Curly || t.typ === CSTParser.WhereOpCall) &&
               !s.opts.whitespace_typedefs
                nws = 0
            end
            multi_arg = n_args(t.ref[]) > 0
            multi_arg ? add_node!(t, Placeholder(nws), s) : add_node!(t, Whitespace(nws), s)
        end
        for nn in n.nodes
            add_node!(t, nn, s, join_lines = true)
        end
        return
    elseif s.opts.import_to_using && n.typ === CSTParser.Import
        usings = import_to_usings(n, s)
        if length(usings) > 0
            for nn in usings
                add_node!(t, nn, s, join_lines = false, max_padding = 0)
            end
            return
        end
    elseif n.typ === CSTParser.BinaryOpCall &&
           n[1].typ === CSTParser.BinaryOpCall &&
           n[1][end].typ === CSTParser.WhereOpCall
        # normalize FST representation for WhereOpCall
        binaryop_to_whereop!(n, s)
    end

    if length(t.nodes) == 0
        t.startline = n.startline
        t.endline = n.endline
        t.len += length(n)
        push!(t.nodes, n)
        return
    end

    if !is_prev_newline(t.nodes[end])
        current_line = t.endline
        notcode_startline = current_line + 1
        notcode_endline = n.startline - 1
        nt = t.nodes[end].typ

        if notcode_startline <= notcode_endline
            # If there are comments in between node elements
            # nesting is forced in an effort to preserve them.

            if remove_empty_notcode(t)
                nest = false
                for l = notcode_startline:notcode_endline
                    if hascomment(s.doc, l)
                        nest = true
                        break
                    end
                end
                if !nest
                    @goto add_node_end
                end
            end

            t.force_nest = true

            # If the previous node type is WHITESPACE - reset it.
            # This fixes cases similar to the one shown in issue #51.
            nt === WHITESPACE && (t.nodes[end] = Whitespace(0))

            hs = hascomment(s.doc, current_line)
            hs && add_node!(t, InlineComment(current_line), s)
            if nt !== PLACEHOLDER
                add_node!(t, Newline(force_nest = true), s)
            elseif hs && nt === PLACEHOLDER
                # swap PLACEHOLDER (will be NEWLINE) with INLINECOMMENT node
                idx = length(t.nodes)
                t.nodes[idx-1], t.nodes[idx] = t.nodes[idx], t.nodes[idx-1]
            end
            add_node!(t, Notcode(notcode_startline, notcode_endline), s)
            add_node!(t, Newline(force_nest = true), s)
        elseif !join_lines
            if hascomment(s.doc, current_line) && current_line != n.startline
                add_node!(t, InlineComment(current_line), s)
            end
            add_node!(t, Newline(force_nest = true), s)
        elseif nt === PLACEHOLDER &&
               current_line != n.startline &&
               hascomment(s.doc, current_line)
            t.force_nest = true
            add_node!(t, InlineComment(current_line), s)
            # swap PLACEHOLDER (will be NEWLINE) with INLINECOMMENT node
            idx = length(t.nodes)
            t.nodes[idx-1], t.nodes[idx] = t.nodes[idx], t.nodes[idx-1]
        end
    end

    @label add_node_end

    if n.startline < t.startline || t.startline == -1
        t.startline = n.startline
    end
    if n.endline > t.endline || t.endline == -1
        t.endline = n.endline
    end

    if !join_lines && is_end(n)
        # end keyword isn't useful w.r.t margin lengths
    elseif t.typ === CSTParser.StringH
        # The length of this node is the length of
        # the longest string. The length of the string is
        # only considered "in the positive" when it's past
        # the hits the initial """ offset, i.e. `t.indent`.
        t.len = max(t.len, n.indent + length(n) - t.indent)
    elseif is_multiline(n)
        is_iterable(t) && n_args(t.ref[]) > 1 && (t.force_nest = true)
        t.len += length(n)
    elseif max_padding >= 0
        t.len = max(t.len, length(n) + max_padding)
    else
        t.len += length(n)
    end
    push!(t.nodes, n)
    nothing
end

@inline function is_prev_newline(fst::FST)
    if fst.typ === NEWLINE
        return true
    elseif is_leaf(fst) || length(fst.nodes) == 0
        return false
    end
    is_prev_newline(fst[end])
end

"""
    `length_to(x::FST, ntyps; start::Int = 1)`

Returns the length to any node type in `ntyps` based off the `start` index.
"""
@inline function length_to(fst::FST, ntyps; start::Int = 1)
    fst.typ in ntyps && return 0, true
    is_leaf(fst) && return length(fst), false
    len = 0
    for i = start:length(fst.nodes)
        l, found = length_to(fst.nodes[i], ntyps)
        len += l
        found && return len, found
    end
    return len, false
end

@inline is_closer(fst::FST) =
    fst.typ === CSTParser.PUNCTUATION &&
    (fst.val == "}" || fst.val == ")" || fst.val == "]")
@inline is_closer(cst::CSTParser.EXPR) =
    cst.kind === Tokens.RBRACE || cst.kind === Tokens.RPAREN || cst.kind === Tokens.RSQUARE

@inline is_opener(fst::FST) =
    fst.typ === CSTParser.PUNCTUATION &&
    (fst.val == "{" || fst.val == "(" || fst.val == "[")
@inline is_opener(cst::CSTParser.EXPR) =
    cst.kind === Tokens.LBRACE || cst.kind === Tokens.LPAREN || cst.kind === Tokens.LSQUARE

@inline is_str(cst::CSTParser.EXPR) = is_str_or_cmd(cst.kind) || is_str_or_cmd(cst.typ)

function is_iterable(x::Union{CSTParser.EXPR,FST})
    x.typ === CSTParser.TupleH && return true
    x.typ === CSTParser.Vect && return true
    x.typ === CSTParser.Vcat && return true
    x.typ === CSTParser.Braces && return true
    x.typ === CSTParser.Call && return true
    x.typ === CSTParser.Curly && return true
    x.typ === CSTParser.Comprehension && return true
    x.typ === CSTParser.TypedComprehension && return true
    x.typ === CSTParser.MacroCall && return true
    x.typ === CSTParser.InvisBrackets && return true
    x.typ === CSTParser.Ref && return true
    x.typ === CSTParser.TypedVcat && return true
    x.typ === CSTParser.Import && return true
    x.typ === CSTParser.Using && return true
    x.typ === CSTParser.Export && return true
    return false
end

function is_block(x::Union{CSTParser.EXPR,FST})
    x.typ === CSTParser.If && return true
    x.typ === CSTParser.Do && return true
    x.typ === CSTParser.Try && return true
    x.typ === CSTParser.Begin && return true
    x.typ === CSTParser.For && return true
    x.typ === CSTParser.While && return true
    x.typ === CSTParser.Let && return true
    # (cst.typ === CSTParser.Quote && cst[1].kind === Tokens.QUOTE) && return true
    (x.typ === CSTParser.Quote && x[1].val == "quote") && return true
    return false
end

function is_opcall(x::Union{CSTParser.EXPR,FST})
    x.typ === CSTParser.BinaryOpCall && return true
    x.typ === CSTParser.Comparison && return true
    x.typ === CSTParser.ChainOpCall && return true
    # InvisBrackets are often mixed with operators
    # so kwargs are propagated through its related
    # functions
    x.typ === CSTParser.InvisBrackets && return true
    return false
end

# Generator typ
# (x for x in 1:10)
# (x for x in 1:10 if x % 2 == 0)
function is_gen(x::Union{CSTParser.EXPR,FST})
    x.typ === CSTParser.Generator && return true
    x.typ === CSTParser.Filter && return true
    x.typ === CSTParser.Flatten && return true
    # x.typ === CSTParser.InvisBrackets && return true
    return false
end

function nest_block(cst::CSTParser.EXPR)
    cst.typ === CSTParser.If && return true
    cst.typ === CSTParser.Do && return true
    cst.typ === CSTParser.Try && return true
    cst.typ === CSTParser.For && return true
    cst.typ === CSTParser.While && return true
    cst.typ === CSTParser.Let && return true
    return false
end

function remove_empty_notcode(fst::FST)
    is_iterable(fst) && return true
    fst.typ === CSTParser.BinaryOpCall && return true
    fst.typ === CSTParser.ConditionalOpCall && return true
    fst.typ === CSTParser.Comparison && return true
    fst.typ === CSTParser.ChainOpCall && return true
    return false
end

nest_assignment(cst::CSTParser.EXPR) = CSTParser.precedence(cst[2].kind) == 1

function unnestable_arg(cst::CSTParser.EXPR)
    is_iterable(cst) && return true
    is_str(cst) && return true
    cst.typ === CSTParser.LITERAL && return true
    cst.typ === CSTParser.UnaryOpCall && cst[2].kind === Tokens.DDDOT && return true
    cst.typ === CSTParser.BinaryOpCall && cst[2].kind === Tokens.DOT && return true
    return false
end

function nestable(::S, cst::CSTParser.EXPR) where {S<:AbstractStyle}
    CSTParser.defines_function(cst) && cst[1].typ !== CSTParser.UnaryOpCall && return true
    nest_assignment(cst) && return !is_str(cst[3])
    true
end

function nest_rhs(cst::CSTParser.EXPR)::Bool
    if CSTParser.defines_function(cst)
        rhs = cst[3]
        rhs.typ === CSTParser.Block && (rhs = rhs[1])
        return nest_block(rhs)
    end
    false
end

function op_kind(cst::CSTParser.EXPR)::Union{Nothing,Tokens.Kind}
    if cst.typ === CSTParser.BinaryOpCall ||
       cst.typ === CSTParser.Comparison ||
       cst.typ === CSTParser.ChainOpCall
        return cst[2].kind
    elseif cst.typ === CSTParser.UnaryOpCall
        return cst[1].typ === CSTParser.OPERATOR ? cst[1].kind : cst[2].kind
    end
    return nothing
end
op_kind(::Nothing) = nothing
op_kind(fst::FST) = fst.ref === nothing ? nothing : op_kind(fst.ref[])

is_lazy_op(kind) = kind === Tokens.LAZY_AND || kind === Tokens.LAZY_OR

"""
    is_standalone_shortcircuit(cst::CSTParser.EXPR)

Returns `true` if the `cst` is a short-circuit expression (uses `&&`, `||`)
and is _standalone_, meaning it's not directly associated with another statement or
expression.

### Examples

```julia
# this IS a standalone short-circuit
a && b

# this IS NOT a standalone short-circuit
if a && b
end

# this IS NOT a standalone short-circuit
var = a && b

# this IS NOT a standalone short-circuit
@macro a && b

# operation inside parenthesis IS NOT a standalone short-circuit
# operation outside parenthesis IS a standalone short-circuit
(a && b) && c
```
"""
function is_standalone_shortcircuit(cst::CSTParser.EXPR)
    kind = op_kind(cst)
    is_lazy_op(kind) || return false

    function valid(n)
        n === nothing && return true
        n.typ === CSTParser.InvisBrackets && return false
        n.typ === CSTParser.MacroCall && return false
        n.typ === CSTParser.Return && return false
        n.typ === CSTParser.If && return false
        n.typ === CSTParser.Block && nest_assignment(n.parent) && return false
        n.typ === CSTParser.BinaryOpCall && nest_assignment(n) && return false
        return true
    end

    function ignore(n::CSTParser.EXPR)
        n.typ === CSTParser.InvisBrackets && return false
        n.typ === CSTParser.If && return false
        n.typ === CSTParser.Block && return false
        n.typ === CSTParser.MacroCall && return false
        n.typ === CSTParser.Return && return false
        n.typ === CSTParser.BinaryOpCall && nest_assignment(n) && return false
        return true
    end

    return parent_is(cst, valid, ignore = ignore)
end
