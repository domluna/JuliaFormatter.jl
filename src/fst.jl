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

    KEYWORD,
    LITERAL,
    OPERATOR,
    PUNCTUATION,
    IDENTIFIER,

    MacroBlock,
    MacroCall,
    Macroname,
    GlobalRefDoc,

    TupleN,
    RefN,
    ModuleN,

    Unary,
    Binary,
    Chain,
    Comparison,
    Conditional,
    Where,

    Vect,
    Braces,
    Brackets,
    Curly,
    Call,
    Parameters,
    Kw,

    Vcat,
    Hcat,
    TypedVcat,
    TypedHcat,
    Row,

    BracesCat,

    TypedComprehension,
    Comprehension,

    Generator,
    Filter,
    Flatten,

    For,
    While,
    If,
    Begin,
    Try,
    Block,
    Quote,
    Do,
    Let,
    Block,
    BareModule,
    TopLevel,

    StringN,

    FunctionN,
    Struct,
    Mutable,
    Primitive,
    Abstract,

    Return,
    Local,
    Outer,
    Global,
    Const,

    Import,
    Export,
    Using,

    File,

    Quotenode,
    Unknown,
)

@enum(NestBehavior, AllowNest, AlwaysNest, NeverNest, NeverNestNode)


"""
Formatted Syntax Tree
"""
mutable struct FST
    typ::FNode

    # Start and end lines of the node
    # in the original source file.
    startline::Int
    endline::Int

    indent::Int
    len::Int
    val::Union{Nothing,AbstractString}
    nodes::Union{Nothing,Vector{FST}}
    ref::Union{Nothing,Ref{CSTParser.EXPR}}
    nest_behavior::NestBehavior

    # Extra margin caused by parent nodes.
    # i.e. `(f(arg))`
    #
    # `f(arg)` would have `extra_margin` = 1
    # due to `)` after `f(arg)`.
    extra_margin::Int
    line_offset::Int
end

FST(typ::FNode, cst::CSTParser.EXPR, indent::Int) =
    FST(cst.head, -1, -1, indent, 0, nothing, FST[], Ref(cst), AllowNest, 0, -1)

FST(typ::FNode, indent::Int) =
    FST(typ, -1, -1, indent, 0, nothing, FST[], nothing, AllowNest, 0, -1)

function FST(
        typ::FNode,
    cst::CSTParser.EXPR,
    line_offset::Int,
    startline::Int,
    endline::Int,
    val::AbstractString,
)
    FST(
        typ,
        startline,
        endline,
        0,
        length(val),
        val,
        nothing,
        Ref(cst),
        AllowNest,
        0,
        line_offset,
    )
end

function FST(
    typ::FNode,
    line_offset::Int,
    startline::Int,
    endline::Int,
    val::AbstractString,
)
    FST(
        head,
        startline,
        endline,
        0,
        length(val),
        val,
        nothing,
        nothing,
        AllowNest,
        0,
        line_offset,
    )
end

@inline function Base.setindex!(fst::FST, node::FST, ind::Int)
    fst.len -= fst.nodes[ind].len
    fst.nodes[ind] = node
    fst.len += node.len
end
@inline Base.getindex(fst::FST, inds...) = fst.nodes[inds...]
@inline Base.lastindex(fst::FST) = length(fst.nodes)
@inline Base.firstindex(fst::FST) = 1
@inline Base.length(fst::FST) = fst.len
@inline function Base.iterate(fst::FST, state = 1)
    if state > length(fst.nodes)
        return nothing
    end
    return fst.nodes[state], state + 1
end

@inline function Base.insert!(fst::FST, ind::Int, node::FST)
    insert!(fst.nodes, ind, node)
    fst.len += node.len
    return
end

@inline Newline(; length = 0, nest_behavior = AllowNest) =
    FST(NEWLINE, -1, -1, 0, length, "\n", nothing, nothing, nest_behavior, 0, -1)
@inline Semicolon() = FST(SEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, AllowNest, 0, -1)
@inline TrailingComma() =
    FST(TRAILINGCOMMA, -1, -1, 0, 0, "", nothing, nothing, AllowNest, 0, -1)
@inline TrailingSemicolon() =
    FST(TRAILINGSEMICOLON, -1, -1, 0, 0, "", nothing, nothing, AllowNest, 0, -1)
@inline InverseTrailingSemicolon() =
    FST(INVERSETRAILINGSEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, AllowNest, 0, -1)
@inline Whitespace(n) =
    FST(WHITESPACE, -1, -1, 0, n, " "^n, nothing, nothing, AllowNest, 0, -1)
@inline Placeholder(n) =
    FST(PLACEHOLDER, -1, -1, 0, n, " "^n, nothing, nothing, AllowNest, 0, -1)
@inline Notcode(startline, endline) =
    FST(NOTCODE, startline, endline, 0, 0, "", nothing, nothing, AllowNest, 0, -1)
@inline InlineComment(line) =
    FST(INLINECOMMENT, line, line, 0, 0, "", nothing, nothing, AllowNest, 0, -1)

@inline must_nest(fst::FST) = fst.nest_behavior === AlwaysNest
@inline cant_nest(fst::FST) = fst.nest_behavior === NeverNest
@inline can_nest(fst::FST) = fst.nest_behavior === AllowNest

@inline is_leaf(cst::CSTParser.EXPR) = cst.args === nothing
@inline is_leaf(fst::FST) = fst.nodes === nothing

@inline is_punc(cst::CSTParser.EXPR) = CSTParser.ispunctuation(cst)
@inline is_punc(fst::FST) = fst.typ === PUNCTUATION

@inline is_end(x::CSTParser.EXPR) = x.head === :END && x.val == "end"
@inline is_end(x::FST) = x.typ === KEYWORD && x.val == "end"

@inline is_colon(x::FST) = x.typ === OPERATOR && x.val == ":"

@inline is_comma(fst::FST) = fst.typ === COMMA || fst.typ === TRAILINGCOMMA
@inline is_comment(fst::FST) = fst.typ === INLINECOMMENT || fst.typ === NOTCODE

@inline is_circumflex_accent(x::CSTParser.EXPR) = CSTParser.isoperator(x) && endswith(CSTParser.valof(x), "^")
@inline is_fwdfwd_slash(x::CSTParser.EXPR) = CSTParser.isoperator(x) && endswith(CSTParser.valof(x), "//")

# TODO: fix this
function is_multiline(fst::FST)
    fst.typ === StringN && return true
    if fst.typ === CSTParser.x_Str && fst[2].typ === CSTParser.StringN
        return true
    elseif fst.typ === CSTParser.x_Cmd && fst[2].typ === CSTParser.StringN
        return true
    elseif fst.typ === Vcat && fst.endline > fst.startline
        return true
    elseif fst.typ === TypedVcat && fst.endline > fst.startline
        return true
    end
    false
end

function is_importer_exporter(fst::FST)
    fst.typ === Import && return true
    fst.typ === Export && return true
    fst.typ === Using && return true
    return false
end

@inline is_macrocall(fst::FST) = fst.typ === MacroCall || fst.typ === MacroBlock

function is_macrodoc(cst::CSTParser.EXPR)
    return cst.head === :macrocall &&
           length(cst) == 3 &&
           cst[1].head === :IDENTIFER && cst[1].val == "@doc" &&
           is_str_or_cmd(cst[2])
end

function is_call(cst::CSTParser.EXPR)
    CSTParser.is_func_call(cst)
end

function is_if(cst::CSTParser.EXPR)
    cst.head == :if && (cst[1].head == :IF || cst[1].head == :ELSEIF)
end

function is_conditional(cst::CSTParser.EXPR)
    CSTParser.is_conditional(x)
end

# f a function which returns a bool
function parent_is(cst::CSTParser.EXPR, valid; ignore = _ -> false)
    p = cst.parent
    p === nothing && return false
    while p !== nothing && ignore(p)
        p = p.parent
    end
    valid(p)
end

contains_comment(nodes::Vector{FST}) = findfirst(is_comment, nodes) !== nothing
function contains_comment(fst::FST)
    is_leaf(fst) && return false
    contains_comment(fst.nodes)
end

function get_args(cst::CSTParser.EXPR)
    cst.args
end
@inline n_args(x) = length(get_args(x))

function add_node!(t::FST, n::FST, s::State; join_lines = false, max_padding = -1)
    if n.typ === SEMICOLON
        join_lines = true
        loc =
            s.offset > length(s.doc.text) && t.typ === TopLevel ?
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
        if en.typ === Generator ||
           en.typ === Filter ||
           en.typ === Flatten ||
           en.typ === MacroCall ||
           en.typ === MacroBlock ||
           (is_comma(en) && t.typ === TupleN && n_args(t.ref[]) == 1)
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

    if n.typ === Block && length(n) == 0
        push!(t.nodes, n)
        return
    elseif n.typ === Parameters
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
            if (t.typ === Curly || t.typ === Where) &&
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
    elseif s.opts.import_to_using && n.typ === Import
        usings = import_to_usings(n, s)
        if length(usings) > 0
            for nn in usings
                add_node!(t, nn, s, join_lines = false, max_padding = 0)
            end
            return
        end
    elseif n.typ === Binary &&
           n[1].typ === Binary &&
           n[1][end].typ === Where
        # normalize FST representation for Where
        binaryop_to_whereop!(n, s)
    end

    if length(t.nodes) == 0
        t.startline = n.startline
        t.endline = n.endline
        t.len += length(n)
        t.line_offset = n.line_offset
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

            rm_block_nl =
                s.opts.remove_extra_newlines &&
                t.typ !== ModuleN &&
                (n.typ === Block || is_end(n))

            if remove_empty_notcode(t) || rm_block_nl
                nest = false
                for l = notcode_startline:notcode_endline
                    if hascomment(s.doc, l)
                        nest = true
                        break
                    end
                end
                if !nest
                    if rm_block_nl
                        add_node!(t, Newline(), s)
                    end
                    @goto add_node_end
                end
            end

            t.nest_behavior = AlwaysNest

            # If the previous node type is WHITESPACE - reset it.
            # This fixes cases similar to the one shown in issue #51.
            nt === WHITESPACE && (t.nodes[end] = Whitespace(0))

            hs = hascomment(s.doc, current_line)
            hs && add_node!(t, InlineComment(current_line), s)
            if nt !== PLACEHOLDER
                add_node!(t, Newline(nest_behavior = AlwaysNest), s)
            elseif hs && nt === PLACEHOLDER
                # swap PLACEHOLDER (will be NEWLINE) with INLINECOMMENT node
                idx = length(t.nodes)
                t.nodes[idx-1], t.nodes[idx] = t.nodes[idx], t.nodes[idx-1]
            end
            add_node!(t, Notcode(notcode_startline, notcode_endline), s)
            add_node!(t, Newline(nest_behavior = AlwaysNest), s)
        elseif !join_lines
            if hascomment(s.doc, current_line) && current_line != n.startline
                add_node!(t, InlineComment(current_line), s)
            end
            add_node!(t, Newline(nest_behavior = AlwaysNest), s)
        elseif nt === PLACEHOLDER &&
               current_line != n.startline &&
               hascomment(s.doc, current_line)
            t.nest_behavior = AlwaysNest
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
    elseif t.typ === StringN
        # The length of this node is the length of
        # the longest string. The length of the string is
        # only considered "in the positive" when it's past
        # the hits the initial """ offset, i.e. `t.indent`.
        t.len = max(t.len, n.indent + length(n) - t.indent)
    elseif is_multiline(n)
        is_iterable(t) && n_args(t.ref[]) > 1 && (t.nest_behavior = AlwaysNest)
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
    fst.val == "}" || fst.val == ")" || fst.val == "]"
@inline is_closer(cst::CSTParser.EXPR) =
    cst.head === :RBRACE || cst.head === :RPAREN || cst.head === :RSQUARE

@inline is_opener(fst::FST) =
    fst.val == "{" || fst.val == "(" || fst.val == "["
@inline is_opener(cst::CSTParser.EXPR) =
    cst.head === :LBRACE || cst.head === :LPAREN || cst.head === :LSQUARE

function is_iterable(x::CSTParser.EXPR)
    x.head === :tuple && return true
    x.head === :vect && return true
    x.head === :vcat && return true
    x.head === :braces && return true
    x.head === :call && return true
    x.head === :curly && return true
    x.head === :comprehension && return true
    x.head === :typed_comprehension && return true
    x.head === :macrocall && return true
    x.head === :brackets && return true
    x.head === :ref && return true
    x.head === :typed_vcat && return true
    x.head === :import && return true
    x.head === :using && return true
    x.head === :export && return true
    return false
end

function is_iterable(x::FST)
    x.typ === TupleN && return true
    x.typ === Vect && return true
    x.typ === Vcat && return true
    x.typ === Braces && return true
    x.typ === Call && return true
    x.typ === Curly && return true
    x.typ === Comprehension && return true
    x.typ === TypedComprehension && return true
    x.typ === MacroCall && return true
    x.typ === Brackets && return true
    x.typ === RefN && return true
    x.typ === TypedVcat && return true
    x.typ === Import && return true
    x.typ === Using && return true
    x.typ === Export && return true
    return false
end

function is_comprehension(x::CSTParser.EXPR)
    x.head === :comprehension && return true
    x.head === :typed_comprehension && return true
    return false
end

function is_comprehension(x::FST)
    x.typ === Comprehension && return true
    x.typ === TypedComprehension && return true
    return false
end

function is_block(x::CSTParser.EXPR)
    x.head === :if && return true
    x.head === :do && return true
    x.head === :try && return true
    x.head === :begin && return true
    x.head === :for && return true
    x.head === :while && return true
    x.head === :let && return true
    x.head === :quote && x[1].head == :QUOTE && return true
    return false
end

function is_block(x::FST)
    x.typ === If && return true
    x.typ === Do && return true
    x.typ === Try && return true
    x.typ === Begin && return true
    x.typ === For && return true
    x.typ === While && return true
    x.typ === Let && return true
    x.typ === Quote && x[1].val == "quote" && return true
    return false
end

function is_opcall(x::CSTParser.EXPR)
    is_binary(x) && return true
    x.head === :comparison && return true
    is_chain(x) && return true
    # Brackets are often mixed with operators
    # so kwargs are propagated through its related
    # functions
    x.head === :brackets && return true
    return false
end

function is_opcall(x::FST)
    x.typ === Binary && return true
    x.typ === Comparison && return true
    x.typ === Chain && return true
    # Brackets are often mixed with operators
    # so kwargs are propagated through its related
    # functions
    x.typ === Brackets && return true
    return false
end

# Generator typ
# (x for x in 1:10)
# (x for x in 1:10 if x % 2 == 0)
function is_gen(x::CSTParser.EXPR)
    x.head === :generator && return true
    x.head === :filter && return true
    x.head === :flatten && return true
    # x.head === :brackets && return true
    return false
end

function is_gen(x::FST)
    x.typ === Generator && return true
    x.typ === Filter && return true
    x.typ === Flatten && return true
    # x.typ === Brackets && return true
    return false
end

function is_binary(x::CSTParser.EXPR)
    CSTParser.isbinarycall(x) || CSTParser.isbinarysyntax(x)
end

function is_unary(x::CSTParser.EXPR)
    CSTParser.isunarycall(x) || CSTParser.isunarysyntax(x)
end

function is_chain(x::CSTParser.EXPR)
    CSTParser.ischainedcall(x)
end

function is_assignment(x::FST)
    if x.typ === Binary && is_assignment(op_kind(x))
        return true
    end

    if (
        x.typ === Const ||
        x.typ === Local ||
        x.typ === Global ||
        x.typ === Outer ||
        x.typ === MacroBlock
    ) && is_assignment(x[end])
        return true
    end

    return false
end
is_assignment(kind::Tokens.Kind) = CSTParser.precedence(kind) == CSTParser.AssignmentOp
is_assignment(::Nothing) = false

function is_function_or_macro_def(cst::CSTParser.EXPR)
    CSTParser.defines_function(cst) && return true
    cst.head === :macro && return true
    cst.head === :where && return true
    return false
end

function nest_block(cst::CSTParser.EXPR)
    cst.head === :if && return true
    cst.head === :do && return true
    cst.head === :try && return true
    cst.head === :for && return true
    cst.head === :while && return true
    cst.head === :let && return true
    return false
end

function remove_empty_notcode(fst::FST)
    is_iterable(fst) && return true
    fst.typ === Binary && return true
    fst.typ === Conditional && return true
    fst.typ === Comparison && return true
    fst.typ === Chain && return true
    return false
end

nest_assignment(cst::CSTParser.EXPR) = is_assignment(cst[2].kind)

"""
`cst` is assumed to be a single child node. Returns true if the node is of the syntactic form `{...}, [...], or (...)`.
"""
function unnestable_node(cst::CSTParser.EXPR)
    cst.head === :tuple && return true
    cst.head === :vect && return true
    cst.head === :braces && return true
    cst.head === :bracescat && return true
    cst.head === :comprehension && return true
    cst.head === :brackets && return true
    return false
end

function nestable(::S, cst::CSTParser.EXPR) where {S<:AbstractStyle}
    CSTParser.defines_function(cst) && is_unary(cst[1]) && return true
    nest_assignment(cst) && return !is_str_or_cmd(cst[3])
    true
end

function nest_rhs(cst::CSTParser.EXPR)::Bool
    if CSTParser.defines_function(cst)
        rhs = cst[3]
        rhs.head === :block && (rhs = rhs[1])
        return nest_block(rhs)
    end
    false
end

function op_kind(cst::CSTParser.EXPR)
    if is_binary(cst) ||
       cst.head === :comparison ||
       is_chain(cst)
        return cst[2].kind
    elseif is_unary(cst)
        return cst[1].head === :OPERATOR ? cst[1].head : cst[2].head
    end
    return nothing
end
function op_kind(fst::FST)
    fst.ref === nothing ? nothing : op_kind(fst.ref[])
end
op_kind(::Nothing) = nothing

get_op(fst::FST) = findfirst(n -> n.head === :OPERATOR, fst.nodes)
get_op(cst::CSTParser.EXPR) = cst[2]

is_lazy_op(kind) = kind === Tokens.LAZY_AND || kind === Tokens.LAZY_OR

"""
    is_standalone_shortcircuit(cst::CSTParser.EXPR)

Returns `true` if the `cst` is a short-circuit expression (uses `&&`, `||`)
and is *standalone*, meaning it's not directly associated with another statement or
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
        n.head === :brackets && return false
        n.head === :macrocall && return false
        n.head === :return && return false
        n.head === :if && return false
        n.head === :block && nest_assignment(n.parent) && return false
        is_binary(n) && nest_assignment(n) && return false
        return true
    end

    function ignore(n::CSTParser.EXPR)
        n.head === :brackets && return false
        n.head === :if && return false
        n.head === :block && return false
        n.head === :macrocall && return false
        n.head === :return && return false
        is_binary(n) && nest_assignment(n) && return false
        return true
    end

    return parent_is(cst, valid, ignore = ignore)
end

"""
    separate_kwargs_with_semicolon!(fst::FST)

Ensures keyword arguments are separated with a ";".

### Examples

Replace "," with ";".

```julia
a = f(x, y = 3)

->

a = f(x; y = 3)
```

Move ";" to the prior to the first positional argument.

```julia
a = f(x = 1; y = 2)

->

a = f(; x = 1, y = 2)
```
"""
function separate_kwargs_with_semicolon!(fst::FST)
    kw_idx = findfirst(n -> n.typ === Kw, fst.nodes)
    kw_idx === nothing && return
    sc_idx = findfirst(n -> n.typ === SEMICOLON, fst.nodes)
    # first "," prior to a kwarg
    comma_idx = findlast(is_comma, fst.nodes[1:kw_idx-1])
    ph_idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes[1:kw_idx-1])
    # @info "" kw_idx sc_idx comma_idx ph_idx

    if sc_idx !== nothing && sc_idx > kw_idx
        # move ; prior to first kwarg
        fst[sc_idx].val = ","
        fst[sc_idx].typ = PUNCTUATION
        if comma_idx === nothing
            if ph_idx !== nothing
                fst[ph_idx] = Placeholder(1)
                insert!(fst, ph_idx, Semicolon())
            else
                insert!(fst, kw_idx, Placeholder(1))
                insert!(fst, kw_idx, Semicolon())
            end
        end
    elseif sc_idx === nothing && comma_idx === nothing
        if ph_idx !== nothing
            fst[ph_idx] = Placeholder(1)
            insert!(fst, ph_idx, Semicolon())
        else
            insert!(fst, kw_idx, Placeholder(1))
            insert!(fst, kw_idx, Semicolon())
        end
    elseif sc_idx === nothing
        fst[comma_idx].val = ";"
        fst[comma_idx].typ = SEMICOLON
    end
    return
end
