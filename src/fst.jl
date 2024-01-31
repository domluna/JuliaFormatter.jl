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
    # non-leaf nodes
    MacroBlock,
    MacroCall,
    MacroStr,
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
    Ncat,
    TypedVcat,
    TypedNcat,
    TypedHcat,
    Row,
    NRow,
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
    Quote,
    Do,
    Let,
    Block,
    BareModule,
    TopLevel,
    StringN,
    Macro,
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
    As,
    NonStdIdentifier, # i.e. var"##iv369"
)

@enum(NestBehavior, AllowNest, AlwaysNest, NeverNest, NeverNestNode)

struct Metadata
    op_kind::Tokens.Kind
    op_dotted::Bool
end

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

    metadata::Union{Nothing,Metadata}
end

FST(typ::FNode, cst::CSTParser.EXPR, indent::Int) =
    FST(typ, -1, -1, indent, 0, nothing, FST[], Ref(cst), AllowNest, 0, -1, nothing)

FST(typ::FNode, indent::Int) =
    FST(typ, -1, -1, indent, 0, nothing, FST[], nothing, AllowNest, 0, -1, nothing)

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
        nothing,
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
        typ,
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
        nothing,
    )
end

function Base.setindex!(fst::FST, node::FST, ind::Int)
    nodes = fst.nodes::Vector{FST}
    fst.len -= nodes[ind].len
    nodes[ind] = node
    fst.len += node.len
end
Base.getindex(fst::FST, inds...) = (fst.nodes::Vector{FST})[inds...]
Base.lastindex(fst::FST) = length(fst.nodes::Vector{FST})
Base.firstindex(fst::FST) = 1
Base.length(fst::FST) = fst.len
function Base.iterate(fst::FST, state = 1)
    nodes = fst.nodes::Vector{FST}
    if state > length(nodes::Vector{FST})
        return nothing
    end
    return nodes[state], state + 1
end

function Base.insert!(fst::FST, ind::Int, node::FST)
    insert!(fst.nodes::Vector{FST}, ind, node)
    fst.len += node.len
    return
end

Newline(; length = 0, nest_behavior = AllowNest) =
    FST(NEWLINE, -1, -1, 0, length, "\n", nothing, nothing, nest_behavior, 0, -1, nothing)
Semicolon() = FST(SEMICOLON, -1, -1, 0, 1, ";", nothing, nothing, AllowNest, 0, -1, nothing)
TrailingComma() =
    FST(TRAILINGCOMMA, -1, -1, 0, 0, "", nothing, nothing, AllowNest, 0, -1, nothing)
TrailingSemicolon() =
    FST(TRAILINGSEMICOLON, -1, -1, 0, 0, "", nothing, nothing, AllowNest, 0, -1, nothing)
InverseTrailingSemicolon() = FST(
    INVERSETRAILINGSEMICOLON,
    -1,
    -1,
    0,
    1,
    ";",
    nothing,
    nothing,
    AllowNest,
    0,
    -1,
    nothing,
)
Whitespace(n) =
    FST(WHITESPACE, -1, -1, 0, n, " "^n, nothing, nothing, AllowNest, 0, -1, nothing)
Placeholder(n) =
    FST(PLACEHOLDER, -1, -1, 0, n, " "^n, nothing, nothing, AllowNest, 0, -1, nothing)
Notcode(startline, endline) =
    FST(NOTCODE, startline, endline, 0, 0, "", nothing, nothing, AllowNest, 0, -1, nothing)
InlineComment(line) =
    FST(INLINECOMMENT, line, line, 0, 0, "", nothing, nothing, AllowNest, 0, -1, nothing)

must_nest(fst::FST) = fst.nest_behavior === AlwaysNest
cant_nest(fst::FST) = fst.nest_behavior === NeverNest
can_nest(fst::FST) = fst.nest_behavior === AllowNest

is_leaf(cst::CSTParser.EXPR) = cst.args === nothing
is_leaf(fst::FST) = fst.nodes === nothing

is_punc(cst::CSTParser.EXPR) = CSTParser.ispunctuation(cst)
is_punc(fst::FST) = fst.typ === PUNCTUATION

is_end(x::CSTParser.EXPR) = x.head === :END && x.val == "end"
is_end(x::FST) = x.typ === KEYWORD && x.val == "end"

is_colon(x::FST) = x.typ === OPERATOR && x.val == ":"

is_comma(fst::FST) = fst.typ === TRAILINGCOMMA || (is_punc(fst) && fst.val == ",")
is_comment(fst::FST) = fst.typ === INLINECOMMENT || fst.typ === NOTCODE

is_circumflex_accent(x::CSTParser.EXPR) =
    CSTParser.isoperator(x) && endswith(CSTParser.valof(x)::String, "^")
is_fwdfwd_slash(x::CSTParser.EXPR) =
    CSTParser.isoperator(x) && endswith(CSTParser.valof(x)::String, "//")

# TODO: fix this
function is_multiline(fst::FST)
    fst.endline > fst.startline && (
        fst.typ === StringN ||
        fst.typ === Vcat ||
        fst.typ === TypedVcat ||
        fst.typ === Ncat ||
        fst.typ === TypedNcat ||
        fst.typ === MacroStr
    )
end

is_macrocall(fst::FST) = fst.typ === MacroCall || fst.typ === MacroBlock

function is_macrodoc(cst::CSTParser.EXPR)
    cst.head === :macrocall &&
        length(cst) == 4 &&
        CSTParser.isidentifier(cst[1]) &&
        cst[1].val == "@doc" &&
        (is_str_or_cmd(cst[3]) || is_macrostr(cst[3]))
end

function is_macrodoc(fst::FST)
    fst.typ === GlobalRefDoc && return true
    fst.typ === MacroBlock &&
        fst[1].typ === Macroname &&
        fst[1][1].val == "@doc" &&
        return true
    return false
end

function is_macrostr(cst::CSTParser.EXPR)
    cst.head === :macrocall || return false
    length(cst) > 2 || return false

    local n::Union{Nothing,CSTParser.EXPR}
    n = if CSTParser.isidentifier(cst[1])
        cst[1]
    elseif CSTParser.is_getfield_w_quotenode(cst[1])
        # last quotenode
        qn = cst[1][end]
        # identifier
        qn[end]
    else
        nothing
    end
    n !== nothing || return false

    is_macrostr_identifier(n) || return false

    return is_str_or_cmd(cst[3])
end

function is_macrostr_identifier(cst::CSTParser.EXPR)
    CSTParser.isidentifier(cst) || return false
    val = cst.val

    # handles odd cases like """M.var"@f";"""
    val !== nothing || return false

    m = match(r"@(.*)_(str|cmd)", val)
    m !== nothing || return false
    val = m.captures[1]::AbstractString

    # r"hello" is parsed as @r_str"hello"
    # if it was originally r"hello" then
    # the number of bytes in the matched string (between @ and _)
    # will be == the span of the node.
    #
    # In this example the span == 1.
    ncodeunits(val) == cst.span
end

function is_call(cst::CSTParser.EXPR)
    t = CSTParser.is_func_call(cst)
    t !== nothing && t && return is_opener(cst[2])
    return false
end

function is_if(cst::CSTParser.EXPR)
    cst.head === :if && cst[1].head === :IF && return true
    cst.head === :elseif && cst[1].head === :ELSEIF && return true
    return false
end

function is_colon_call(cst::CSTParser.EXPR)
    CSTParser.isoperator(cst) && cst.val == ":" && return true
    cst.head == :call && is_colon_call((cst.args::Vector{CSTParser.EXPR})[1]) && return true
    return false
end

function is_custom_leaf(fst::FST)
    fst.typ === NEWLINE ||
        fst.typ === SEMICOLON ||
        fst.typ === WHITESPACE ||
        fst.typ === PLACEHOLDER ||
        fst.typ === NOTCODE ||
        fst.typ === INLINECOMMENT ||
        fst.typ === TRAILINGCOMMA ||
        fst.typ === TRAILINGSEMICOLON ||
        fst.typ === INVERSETRAILINGSEMICOLON
end

function is_nothing(cst::CSTParser.EXPR)
    CSTParser.is_nothing(cst) && return true
    cst.val === nothing && cst.args === nothing && return true
    return false
end

"""
Returns whether the first unignored parent of `cst` matches the criteria determined by
`valid`, which is a function that returns a boolean. `ignore` can be used to ignore certain
parent nodes if desired, also a function which returns a boolean.
"""
function parent_is(cst::CSTParser.EXPR, valid; ignore = _ -> false)
    p = cst.parent
    p === nothing && return false
    while p !== nothing && ignore(p)
        p = p.parent
    end
    valid(p::Union{Nothing,CSTParser.EXPR})
end

contains_comment(nodes::Vector{FST}) = findfirst(is_comment, nodes) !== nothing
function contains_comment(fst::FST)
    is_leaf(fst) && return false
    contains_comment(fst.nodes::Vector)
end

# TODO: Remove once this is fixed in CSTParser.
# https://github.com/julia-vscode/CSTParser.jl/issues/108
function get_args(cst::CSTParser.EXPR)
    if cst.head === :macrocall ||
       cst.head === :typed_vcat ||
       cst.head === :typed_ncat ||
       cst.head === :ref ||
       cst.head === :curly ||
       is_call(cst) ||
       cst.head === :typed_comprehension
        return get_args(collect(cst)[2:end])
    elseif cst.head === :where
        # get the arguments in B of `A where B`
        return get_args(collect(cst)[3:end])
    elseif cst.head === :braces ||
           cst.head === :vcat ||
           cst.head === :ncat ||
           cst.head === :bracescat ||
           cst.head === :tuple ||
           cst.head === :vect ||
           cst.head === :brackets ||
           cst.head === :parameters ||
           cst.head === :comprehension
        return get_args(cst.args)
    end
    return get_args(cst.args)
end

function get_args(args::Union{Vector{Any},Vector{CSTParser.EXPR}})
    args0 = CSTParser.EXPR[]
    for a in args
        if CSTParser.ispunctuation(a) ||
           CSTParser.is_nothing(a) ||
           haskey(SEMICOLON_LOOKUP, a.head)
            continue
        end
        if a.head === :parameters
            append!(args0, get_args(a))
        else
            push!(args0, a)
        end
    end
    args0
end

function get_args(args::Nothing)
    return CSTParser.EXPR[]
end

n_args(x) = length(get_args(x))

"""
    add_node!(
        t::FST,
        n::FST,
        s::State;
        join_lines::Bool = false,
        max_padding::Int = -1,
        override_join_lines_based_on_source::Bool = false,
    )

Appends `n` to `t`.

- `join_lines` if `false` a NEWLINE node will be added and `n` will appear
on the next line, otherwise it will appear on the same line as the previous
node (when printing).
- `max_padding` >= 0 indicates margin of `t` should be based on whether the margin
of `n` + `max_padding` is greater than the current margin of `t`. Otherwise the margin
`n` will be added to `t`.
- `override_join_lines_based_on_source` is only used when `join_lines_based_on_source` option is `true`. In which
`n` is added to `t` as if `join_lines_based_on_source` was false.
"""
function add_node!(
    t::FST,
    n::FST,
    s::State;
    join_lines::Bool = false,
    max_padding::Int = -1,
    override_join_lines_based_on_source::Bool = false,
)
    tnodes = t.nodes::Vector{FST}
    if n.typ === SEMICOLON
        join_lines = true
        loc = if s.offset > length(s.doc.text) && t.typ === TopLevel
            cursor_loc(s, s.offset - 1)
        else
            cursor_loc(s)
        end
        for l in t.endline:loc[1]
            if has_semicolon(s.doc, l)
                n.startline = l
                n.endline = l
                break
            end
        end

        # If there's no semicolon, treat it
        # as formatter node
        if n.startline == -1
            t.len += length(n)
            n.startline = t.endline
            n.endline = t.endline
            push!(tnodes::Vector{FST}, n)
            return
        end
    elseif n.typ === TRAILINGCOMMA
        en = (tnodes::Vector{FST})[end]
        if en.typ === Generator ||
           en.typ === Filter ||
           en.typ === Flatten ||
           en.typ === MacroCall ||
           en.typ === MacroBlock ||
           en.typ === SEMICOLON ||
           # arg = @macro foo
           en.typ === Kw && (en[end].typ === MacroCall || en[end].typ === MacroBlock)
            # don't add trailing comma in these cases
        elseif is_comma(en) && t.typ === TupleN && n_args(t.ref[]) == 1
            # preserve comma
        elseif s.opts.trailing_comma === nothing
        elseif !s.opts.trailing_comma::Bool
            if is_comma(en)
                t[end] = Whitespace(0)
            end
        elseif is_comma(en)
            t[end] = n
        else
            t.len += length(n)
            n.startline = t.endline
            n.endline = t.endline
            push!(tnodes::Vector{FST}, n)
        end
        return
    elseif n.typ === NOTCODE
        n.indent = s.indent
        push!(tnodes::Vector{FST}, n)
        return
    elseif n.typ === INLINECOMMENT
        push!(tnodes::Vector{FST}, n)
        return
    elseif is_custom_leaf(n)
        t.len += length(n)
        n.startline = t.endline
        n.endline = t.endline
        push!(tnodes::Vector{FST}, n)
        return
    end

    if n.typ === Block && length(n) == 0
        push!(tnodes::Vector{FST}, n)
        return
    elseif n.typ === Parameters
        # unpack Parameters arguments into the parent node
        if t.ref !== nothing && n_args(t.ref[]) == n_args(n.ref[])
            # There are no arguments prior to params
            # so we can remove the initial placeholder.
            idx = findfirst(n -> n.typ === PLACEHOLDER, tnodes)
            idx !== nothing && (t[idx] = Whitespace(0))
        end
        add_node!(t, Semicolon(), s)

        if length(n.nodes::Vector{FST}) > 0
            nws = 1
            if (t.typ === Curly || t.typ === Where || t.typ === BracesCat) &&
               !s.opts.whitespace_typedefs
                nws = 0
            end
            multi_arg = t.ref !== nothing && n_args(t.ref[]) > 0
            multi_arg ? add_node!(t, Placeholder(nws), s) : add_node!(t, Whitespace(nws), s)
        end
        for nn in n.nodes::Vector{FST}
            add_node!(t, nn, s, join_lines = true)
        end
        return
    elseif s.opts.import_to_using && n.typ === Import && t.typ !== MacroBlock
        usings = import_to_usings(n, s)
        if length(usings) > 0
            for (i, nn) in enumerate(usings)
                add_node!(t, nn, s; join_lines = false, max_padding = 0)
            end
            return
        end
    elseif n.typ === Binary && n[1].typ === Binary && n[1][end].typ === Where
        # normalize FST representation for Where
        binaryop_to_whereop!(n, s)
    end

    if length(tnodes::Vector{FST}) == 0
        t.startline = n.startline
        t.endline = n.endline
        t.len += length(n)
        t.line_offset = n.line_offset
        push!(tnodes, n)
        return
    end

    # if `max_padding` >= 0 `n` should appear on the next line
    # even if it's contrary to the original source.
    if s.opts.join_lines_based_on_source &&
       !override_join_lines_based_on_source &&
       max_padding == -1 &&
       !(
           is_comma(n) ||
           is_block(t) ||
           t.typ === FunctionN ||
           t.typ === Macro ||
           is_typedef(t) ||
           t.typ === ModuleN ||
           t.typ === BareModule ||
           is_end(n)
       )
        # join based on position in original file
        join_lines = t.endline == n.startline
    end

    if !is_prev_newline(tnodes[end]::FST)
        current_line = t.endline
        notcode_startline = current_line + 1
        notcode_endline = n.startline - 1
        nt = (tnodes[end]::FST).typ

        if notcode_startline <= notcode_endline
            # If there are comments in between node elements
            # nesting is forced in an effort to preserve them.

            rm_block_nl =
                s.opts.remove_extra_newlines &&
                t.typ !== ModuleN &&
                (n.typ === Block || is_end(n))

            nest = true
            if remove_empty_notcode(t) || rm_block_nl
                nest = false
                for l in notcode_startline:notcode_endline
                    if hascomment(s.doc, l)
                        nest = true
                        break
                    end
                end
            end

            t.nest_behavior = AlwaysNest

            # If the previous node type is WHITESPACE - reset it.
            # This fixes cases similar to the one shown in issue #51.
            nt === WHITESPACE && (tnodes[end]::FST = Whitespace(0))

            if hascomment(s.doc, current_line)
                add_node!(t, InlineComment(current_line), s)
            end

            if nt !== PLACEHOLDER
                add_node!(t, Newline(nest_behavior = AlwaysNest), s)
            elseif hascomment(s.doc, current_line) && nt === PLACEHOLDER
                # swap PLACEHOLDER (will be NEWLINE) with INLINECOMMENT node
                idx = length(tnodes::Vector{FST})
                tnodes[idx-1], tnodes[idx] = tnodes[idx], tnodes[idx-1]
            end

            if nest
                add_node!(t, Notcode(notcode_startline, notcode_endline), s)
                add_node!(t, Newline(nest_behavior = AlwaysNest), s)
            end
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
            idx = length(tnodes)
            tnodes[idx-1], tnodes[idx] = tnodes[idx], tnodes[idx-1]
        elseif nt === WHITESPACE &&
               hascomment(s.doc, current_line) &&
               current_line != n.startline
            # rely on the whitespace tracked for the inline comment
            tnodes[end] = Whitespace(0)
            add_node!(t, InlineComment(current_line), s)
            add_node!(t, Newline(nest_behavior = AlwaysNest), s)
        end
    end

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
    push!(tnodes, n)
    nothing
end

function is_prev_newline(fst::FST)
    if fst.typ === NEWLINE
        return true
    elseif is_leaf(fst) || length(fst.nodes::Vector) == 0
        return false
    end
    is_prev_newline(fst[end])
end

"""
    `length_to(x::FST, ntyps; start::Int = 1)`

Returns the length to any node type in `ntyps` based off the `start` index.
"""
function length_to(fst::FST, ntyps; start::Int = 1)
    fst.typ in ntyps && return 0, true
    is_leaf(fst) && return length(fst), false
    len = 0
    nodes = fst.nodes::Vector
    for i in start:length(nodes)
        l, found = length_to(nodes[i], ntyps)
        len += l
        found && return len, found
    end
    return len, false
end

is_closer(fst::FST) =
    fst.typ === PUNCTUATION && (fst.val == "}" || fst.val == ")" || fst.val == "]")
is_closer(cst::CSTParser.EXPR) =
    cst.head === :RBRACE || cst.head === :RPAREN || cst.head === :RSQUARE

is_opener(fst::FST) =
    fst.typ === PUNCTUATION && (fst.val == "{" || fst.val == "(" || fst.val == "[")
is_opener(cst::CSTParser.EXPR) =
    cst.head === :LBRACE || cst.head === :LPAREN || cst.head === :LSQUARE

function is_iterable(x::CSTParser.EXPR)
    x.head === :tuple && return true
    x.head === :vect && return true
    x.head === :vcat && return true
    x.head === :braces && return true
    is_call(x) && return true
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
    is_named_iterable(x) && return true
    is_unnamed_iterable(x) && return true
    is_import_expr(x) && return true
    return false
end
is_iterable(::Nothing) = false

function is_unnamed_iterable(x::FST)
    x.typ === TupleN && return true
    x.typ === Vect && return true
    x.typ === Vcat && return true
    x.typ === Ncat && return true
    x.typ === Braces && return true
    x.typ === Comprehension && return true
    x.typ === Brackets && return true
    return false
end

function is_named_iterable(x::FST)
    x.typ === Call && return true
    x.typ === Curly && return true
    x.typ === TypedComprehension && return true
    x.typ === MacroCall && return true
    x.typ === RefN && return true
    x.typ === TypedVcat && return true
    x.typ === TypedNcat && return true
    return false
end

function is_import_expr(x::FST)
    x.typ === Import && return true
    x.typ === Using && return true
    x.typ === Export && return true
    return false
end

"""
Returns whether `fst` can be an iterable argument. For example in
the case of a function call, which is of type `Call`:

```julia
(a, b, c; k1=v1)
```

This would return `true` for `a`, `b`, `c` and `k1=v1` and `false` for all other nodes.
"""
function is_iterable_arg(fst::FST)
    fst.typ === PUNCTUATION && return false
    fst.typ === KEYWORD && return false
    fst.typ === OPERATOR && return false
    is_custom_leaf(fst) && return false
    return true
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
    is_if(x) && return true
    x.head === :do && return true
    x.head === :try && return true
    (x.head === :block && length(x) > 1 && x[1].head == :BEGIN) && return true
    x.head === :for && return true
    x.head === :while && return true
    x.head === :let && return true
    (x.head === :quote && x[1].head == :QUOTE) && return true
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

function is_typedef(fst::FST)
    fst.typ === Struct && return true
    fst.typ === Mutable && return true
    fst.typ === Primitive && return true
    fst.typ === Abstract && return true
    return false
end

function is_opcall(x::CSTParser.EXPR)
    is_binary(x) && return true
    x.head === :comparison && return true
    is_chain(x) && return true
    # is_unary(x) && return true
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
    # x.typ === Unary && return true
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
    (CSTParser.isbinarycall(x) || CSTParser.isbinarysyntax(x)) &&
        length(x) == 3 &&
        CSTParser.isoperator(x[2])
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
function is_assignment(cst::CSTParser.EXPR)
    op = get_binary_op(cst)
    op === nothing && return false
    precedence(op) == CSTParser.AssignmentOp
end
is_assignment(::Nothing) = false

function is_pairarrow(cst::CSTParser.EXPR)
    op = get_binary_op(cst)
    op === nothing && return false
    CSTParser.is_pairarrow(op)
end

function precedence(cst::CSTParser.EXPR)
    CSTParser.isoperator(cst) || return 0
    val = CSTParser.isdotted(cst) ? (cst.val::AbstractString)[2:end] : cst.val
    CSTParser.get_prec(val)
end

function is_function_or_macro_def(cst::CSTParser.EXPR)
    CSTParser.defines_function(cst) && return true
    cst.head === :macro && return true
    cst.head === :where && return true
    CSTParser.isoperator(cst.head) &&
        cst.head.val == "::" &&
        cst.parent !== nothing &&
        (cst.parent.head == :where || cst.parent.head === :function) &&
        return true
    return false
end

function nest_block(cst::CSTParser.EXPR)
    is_if(cst) && return true
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

function get_binary_op(cst::CSTParser.EXPR)
    if cst.head == :call
        return (cst.args::Vector{CSTParser.EXPR})[1]
    elseif length(cst) == 3 && CSTParser.isoperator(cst[2])
        return cst[2]
    elseif CSTParser.isoperator(cst.head)
        return cst.head
    else
        return nothing
    end
end

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

function is_binaryop_nestable(::S, cst::CSTParser.EXPR) where {S<:AbstractStyle}
    CSTParser.defines_function(cst) && is_unary(cst[1]) && return true
    if is_assignment(cst) || is_pairarrow(cst)
        return !is_str_or_cmd(cst[3])
    end
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
    if is_binary(cst) || cst.head === :comparison || is_chain(cst)
        return tokenize(cst[2].val)
    elseif is_unary(cst)
        op = CSTParser.isoperator(cst[1]) ? cst[1] : cst[2]
        return tokenize(op.val)
    end
    return nothing
end

function op_kind(fst::FST)
    if fst.typ === OPERATOR
        return fst.metadata.op_kind
    elseif is_opcall(fst)
        idx = findfirst(n -> n.typ === OPERATOR, fst.nodes::Vector)
        idx === nothing && return nothing
        return fst[idx].metadata.op_kind
    end
    return nothing
end

function _valid_parent_node_for_standalone_circuit(n::CSTParser.EXPR)
    n.head === :brackets && return false
    n.head === :macrocall && return false
    n.head === :return && return false
    is_if(n) && return false
    n.head === :block && is_assignment(n.parent) && return false
    is_binary(n) && is_assignment(n) && return false
    return true
end
_valid_parent_node_for_standalone_circuit(n::Nothing) = true

function _ignore_node_for_standalone_circuit(n::CSTParser.EXPR)
    n.head === :brackets && return false
    is_if(n) && return false
    n.head === :block && return false
    n.head === :macrocall && return false
    n.head === :return && return false
    is_binary(n) && is_assignment(n) && return false
    return true
end

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
    val = cst[2].val
    (val == "&&" || val == "||") || return false

    return parent_is(
        cst,
        _valid_parent_node_for_standalone_circuit,
        ignore = _ignore_node_for_standalone_circuit,
    )
end
is_standalone_shortcircuit(::Nothing) = false

"""
    eq_to_in_normalization!(fst::FST, always_for_in::Bool, for_in_replacement::String)
    eq_to_in_normalization!(fst::FST, always_for_in::Nothing, for_in_replacement::String)

Transforms

```julia
for i = iter body end

=>

for i in iter body end
```

AND

```julia
for i in 1:10 body end

=>

for i = 1:10 body end
```

`always_for_in=nothing` disables this normalization behavior.

- https://github.com/domluna/JuliaFormatter.jl/issues/34
"""
function eq_to_in_normalization!(fst::FST, always_for_in::Bool, for_in_replacement::String)
    if fst.typ === Binary
        idx = findfirst(n -> n.typ === OPERATOR, fst.nodes::Vector)
        idx === nothing && return
        op = fst[idx]

        !valid_for_in_op(op.val) && return

        if always_for_in
            op.val = for_in_replacement
            op.len = length(op.val)
        elseif op.val == "=" && op_kind(fst[end]) !== Tokens.COLON
            op.val = "in"
            op.len = length(op.val)
        elseif op.val == "in" && op_kind(fst[end]) === Tokens.COLON
            op.val = "="
            op.len = length(op.val)
        end
    elseif fst.typ === Block || fst.typ === Brackets || fst.typ === Filter
        past_if = false
        for (i, n) in enumerate(fst.nodes::Vector)
            if n.typ === KEYWORD && n.val == "if"
                # [x for x in xs if x in 1:length(ys)]
                # we do not want to convert the binary operations after an "if" keyword.
                past_if = true
            end
            past_if && break
            eq_to_in_normalization!(n, always_for_in, for_in_replacement)
        end
    end
end

eq_to_in_normalization!(::FST, ::Nothing, ::String) = nothing

# Check if the caller of a call is in `list`
# Note that this also works for CSTParser.EXPR
function caller_in_list(fst::Union{FST,CSTParser.EXPR}, list::Vector{String})
    if is_leaf(fst[1]) && (fst[1].val)::String in list
        return true
    elseif !is_leaf(fst[1]) && is_leaf(fst[1][1]) && (fst[1][1].val)::String in list
        return true
    end

    return false
end

"""
    is_dot_op(x::CSTParser.EXPR)

Checks if the binary operation is a dot operation (e.g. `x.y`, `x..z`, `x...z`). This is different from a dot or broadcast operation.
"""
function is_dot_op(x::CSTParser.EXPR)
    op_kind(x) in (Tokens.DDDOT, Tokens.DDOT, Tokens.DDOTS)
end

function is_isa(x::CSTParser.EXPR)
    CSTParser.isoperator(x) && x.val == "isa"
end
