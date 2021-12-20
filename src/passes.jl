# FST passes/transforms

function flattenable(kind::Tokens.Kind)
    kind === Tokens.AND && return true
    kind === Tokens.OR && return true
    kind === Tokens.LAZY_AND && return true
    kind === Tokens.LAZY_OR && return true
    kind === Tokens.RPIPE && return true
    return false
end
flattenable(::Nothing) = false

"""
Flattens a binary operation call tree if the operation repeats 2 or more times.
"a && b && c" will be transformed while "a && b" will not.
"""
function flatten_binaryopcall(fst::FST; top = true)
    nodes = FST[]
    kind = op_kind(fst)

    lhs = fst[1]
    rhs = fst[end]
    lhs_kind = op_kind(lhs)
    rhs_kind = op_kind(rhs)
    lhs_same_op = lhs_kind === kind
    rhs_same_op = rhs_kind === kind
    idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes)

    if (top && !lhs_same_op && !rhs_same_op) || idx === nothing
        return nodes
    end

    if lhs_same_op
        push!(nodes, flatten_binaryopcall(lhs, top = false)...)
    else
        flatten_fst!(lhs)
        push!(nodes, lhs)
    end
    # everything except the indentation placeholder
    push!(nodes, fst[2:idx-1]...)

    if rhs_same_op
        push!(nodes, flatten_binaryopcall(rhs, top = false)...)
    else
        flatten_fst!(rhs)
        push!(nodes, rhs)
    end

    return nodes
end

function flatten_conditionalopcall(fst::FST)
    nodes = FST[]
    for n in fst.nodes
        if n.typ === Conditional
            push!(nodes, flatten_conditionalopcall(n)...)
        else
            push!(nodes, n)
        end
    end
    return nodes
end

function flatten_fst!(fst::FST)
    is_leaf(fst) && return
    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif n.typ === Binary && flattenable(op_kind(n))
            # possibly convert Binary to Chain
            nnodes = flatten_binaryopcall(n)
            if length(nnodes) > 0
                n.typ = Chain
                n.nodes = nnodes
            else
                flatten_fst!(n)
            end
        else
            flatten_fst!(n)
        end
    end
end

"""
    pipe_to_function_call_pass!(fst::FST)

Rewrites `x |> f` to `f(x)`.
"""
function pipe_to_function_call_pass!(fst::FST)
    is_leaf(fst) && return

    # the RHS must be a valid type to apply a function call.
    if op_kind(fst) === Tokens.RPIPE && (fst[end].typ !== PUNCTUATION)
        fst.nodes = pipe_to_function_call(fst)
        fst.typ = Call
        return
    end
    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif op_kind(n) === Tokens.RPIPE && (n[end].typ !== PUNCTUATION)
            n.nodes = pipe_to_function_call(n)
            n.typ = Call
        else
            pipe_to_function_call_pass!(n)
        end
    end
end

function pipe_to_function_call(fst::FST)
    nodes = FST[]
    dot = fst[3].metadata !== nothing && fst[3].metadata.op_dotted
    arg2 = fst[end]

    if dot && arg2.typ === IDENTIFIER
        arg2.val *= "."
        arg2.len += 1
    elseif arg2.typ === Binary &&
           arg2[end].typ === Quotenode &&
           arg2[end][end].typ === IDENTIFIER
        iden = arg2[end][end]
        iden.val *= "."
        iden.len += 1
    end

    push!(nodes, arg2)
    paren = FST(PUNCTUATION, -1, arg2.endline, arg2.endline, "(")
    push!(nodes, paren)
    pipe_to_function_call_pass!(fst[1])
    arg1 = fst[1]
    push!(nodes, arg1)
    paren = FST(PUNCTUATION, -1, arg1.endline, arg1.endline, ")")
    push!(nodes, paren)
    return nodes
end

function import_to_usings(fst::FST, s::State)
    findfirst(n -> is_colon(n) || n.typ === As, fst.nodes) === nothing || return FST[]
    findfirst(n -> n.typ === PUNCTUATION && n.val == ".", fst[3].nodes) === nothing ||
        return FST[]

    usings = FST[]
    idxs = findall(n -> !is_leaf(n), fst.nodes)

    for i in idxs
        n = fst[i]
        sl = n.startline
        el = n.endline
        use = FST(Using, fst.indent)
        use.startline = n.startline
        use.endline = n.endline

        add_node!(use, FST(KEYWORD, -1, sl, el, "using"), s)
        add_node!(use, Whitespace(1), s)

        add_node!(use, n, s, join_lines = true)
        colon = FST(OPERATOR, -1, sl, el, ":")
        colon.metadata = Metadata(Tokens.COLON, false)
        add_node!(use, colon, s, join_lines = true)
        add_node!(use, Whitespace(1), s)
        add_node!(use, n[end], s, join_lines = true)

        push!(usings, use)
    end

    return usings
end

"""
    annotate_typefields_with_any!(fst::FST, s::State)

Annotates fields in a type definitions with `::Any` if
no type annotation is provided.
"""
function annotate_typefields_with_any!(fst::FST, s::State)
    is_leaf(fst) && return
    for (i, n) in enumerate(fst.nodes)
        if n.typ === IDENTIFIER
            nn = FST(Binary, n.indent)
            nn.startline = n.startline
            nn.endline = n.endline
            add_node!(nn, n, s)
            line_offset = n.line_offset + length(n)
            op = FST(OPERATOR, line_offset, n.startline, n.endline, "::")
            op.metadata = Metadata(Tokens.DECLARATION, false)
            add_node!(nn, op, s, join_lines = true)
            line_offset += 2
            add_node!(
                nn,
                FST(IDENTIFIER, line_offset, n.startline, n.endline, "Any"),
                s,
                join_lines = true,
            )
            fst[i] = nn
        else
            continue
        end
    end
end

"""
    short_to_long_function_def!(fst::FST, s::State)

Transforms a *short* function definition

```julia
f(arg1, arg2) = body
```

to a *long* function definition

```julia
function f(arg2, arg2)
    body
end
```
"""
function short_to_long_function_def!(fst::FST, s::State)
    (fst[1].typ !== Call && fst[1].typ !== Where) && return

    # 3 cases
    #
    # case 1
    #   func(a) = body
    #
    # case 2
    #   func(a::T) where T = body
    #
    # case 3
    #   func(a::T)::R where T = body
    funcdef = FST(FunctionN, fst.indent)
    # function
    kw = FST(KEYWORD, -1, fst[1].startline, fst[1].endline, "function")
    add_node!(funcdef, kw, s)
    add_node!(funcdef, Whitespace(1), s)

    # func(a)
    # OR
    # func(a) where T
    add_node!(funcdef, fst[1], s, join_lines = true)

    # body

    s.opts.always_use_return && prepend_return!(fst[end], s)
    if fst[end].typ === Block
        add_node!(funcdef, fst[end], s, max_padding = s.opts.indent)
    else
        # ```
        # function
        #     body
        # end
        # ```
        #
        # `body` is parsed wrapped block node. Wrapping it in
        # a `Block` node ensures the indent is correct.
        bl = FST(Block, fst[end].indent)
        add_node!(bl, fst[end], s)
        add_node!(funcdef, bl, s, max_padding = s.opts.indent)
    end
    add_indent!(funcdef[end], s, s.opts.indent)

    # end
    kw = FST(KEYWORD, -1, fst[end].startline, fst[end].endline, "end")
    add_node!(funcdef, kw, s)

    fst.typ = funcdef.typ
    fst.nodes = funcdef.nodes
    fst.len = funcdef.len
end

"""
    binaryop_to_whereop(fst::FST, s::State)

Handles the case of a function def defined
as:

```julia
foo(a::A)::R where A = body
```

In this case instead of it being parsed as (1):

```
Binary
 - Where
 - OP
 - RHS
```

It's parsed as (2):

```
Binary
 - Binary
  - LHS
  - OP
  - Where
   - R
   - ...
 - OP
 - RHS
```

(1) is preferrable since it's the same parsed result as:

```julia
foo(a::A) where A = body
```

This transformation converts (2) to (1).

ref https://github.com/julia-vscode/CSTParser.jl/issues/93
"""
function binaryop_to_whereop!(fst::FST, s::State)
    # transform fst[1] to a Where
    oldbinop = fst[1]
    oldwhereop = fst[1][end]
    binop = FST(Binary, fst[1].indent)

    # foo(a::A)
    add_node!(binop, oldbinop[1], s)
    # foo(a::A)::
    add_node!(binop, oldbinop[2], s, join_lines = true)
    # foo(a::A)::R
    add_node!(binop, oldwhereop[1], s, join_lines = true)

    whereop = FST(Where, fst[1].indent)
    add_node!(whereop, binop, s)

    # "foo(a::A)::R where A"
    for n in oldwhereop[2:end]
        add_node!(whereop, n, s, join_lines = true)
    end

    fst[1] = whereop
end

"""
    prepend_return!(fst::FST, s::State)

Prepends `return` to the last expression of a block if applicable.

```julia
function foo()
    a = 2 * 3
    a / 3
end
```

to

```julia
function foo()
    a = 2 * 3
    return a / 3
end
```
"""
function prepend_return!(fst::FST, s::State)
    fst.typ === Block || return
    ln = fst[end]
    is_block(ln) && return
    ln.typ === Return && return
    ln.typ === MacroCall && return
    ln.typ === MacroBlock && return
    ln.typ === MacroStr && return
    if length(fst.nodes) > 2 && (fst[end-2].typ === MacroStr || is_macrodoc(fst[end-2]))
        # The last node is has a docstring prior to it so a return should not be prepended
        # fst[end-1] is a newline
        return
    end
    # fix #426
    # don't add return if the last node is a throw call. throw is a built-in function
    # that shouldn't be overwritten for over purposes so this should be fine.
    ln.typ === Call && ln[1].typ === IDENTIFIER && ln[1].val == "throw" && return

    # check to see if the last node already has a return
    found_return = false
    f = (fst::FST, ::State) -> begin
        if fst.typ === Return
            found_return = true
        end
        return
    end
    walk(f, ln, s)
    found_return && return

    ret = FST(Return, fst.indent)
    kw = FST(KEYWORD, -1, fst[end].startline, fst[end].endline, "return")
    add_node!(ret, kw, s)
    add_node!(ret, Whitespace(1), s)
    add_node!(ret, ln, s, join_lines = true)
    fst[end] = ret
    return
end

"""
    move_at_sign_to_the_end(fst::FST, s::State)

NOTE: Assumes `fst` is the caller name of a macrocall such as
`@macro` or `Module.@macro`.

Moves `@` to the last indentifier.

Example:

```julia
@Module.macro
```

to

```julia
Module.@macro
```
"""
function move_at_sign_to_the_end(fst::FST, s::State)
    t = FST[]
    f = (t) -> (n, s) -> begin
        if is_macrocall(n) || (n.typ === Quotenode && !is_leaf(n[1]))
            # 1. Do not move "@" in nested macro calls
            # 2. Do not move "@" if in the middle of a chain, i.e. "a.@b.c"
            # since it's semantically different to "@a.b.c" and "a.b.@c"
            push!(t, n)
            return false
        elseif is_leaf(n)
            push!(t, n)
        end
    end
    walk(f(t), fst, s)

    macroname = FST(Macroname, fst.indent)
    for (i, n) in enumerate(t)
        if n.val == "@"
            continue
        elseif n.typ === IDENTIFIER && i < length(t) && n.val[1] == '@'
            n.val = n.val[2:end]
            n.len -= 1
            add_node!(macroname, n, s, join_lines = true)
        elseif i < length(t) || n.typ == Quotenode
            add_node!(macroname, n, s, join_lines = true)
        else
            if n.typ === IDENTIFIER && n.val[1] != '@'
                n.val = "@" * n.val
                n.len += 1
                add_node!(macroname, n, s, join_lines = true)
            else
                add_node!(macroname, n, s, join_lines = true)
            end
        end
    end

    return macroname
end

function conditional_to_if_block!(fst::FST, s::State; top = true)
    t = FST(If, fst.indent)
    kw = FST(KEYWORD, -1, fst.startline, fst.startline, top ? "if" : "elseif")
    add_node!(t, kw, s, max_padding = 0)
    add_node!(t, Whitespace(1), s, join_lines = true)
    add_node!(t, fst[1], s, join_lines = true)

    idx1 = findfirst(n -> n.typ === OPERATOR && n.val == "?", fst.nodes)
    idx2 = findfirst(n -> n.typ === OPERATOR && n.val == ":", fst.nodes)

    block1 = FST(Block, fst.indent + s.opts.indent)
    for n in fst.nodes[idx1+1:idx2-1]
        if n.typ === PLACEHOLDER ||
           n.typ === WHITESPACE ||
           n.typ === NEWLINE ||
           is_comment(n)
            continue
        end
        add_node!(block1, n, s)
    end
    add_node!(t, block1, s, max_padding = s.opts.indent)

    block2 = FST(Block, fst.indent)
    padding = 0
    if fst[end].typ === Conditional
        conditional_to_if_block!(fst[end], s, top = false)
    else
        block2.indent += s.opts.indent
        padding = s.opts.indent
        kw = FST(KEYWORD, -1, -1, -1, "else")
        add_node!(t, kw, s, max_padding = 0)
    end
    add_node!(block2, fst[end], s)
    add_node!(t, block2, s, max_padding = 0)

    if top
        kw = FST(KEYWORD, -1, -1, -1, "end")
        add_node!(t, kw, s, max_padding = 0)
    end

    fst.typ = t.typ
    fst.nodes = t.nodes
    fst.len = t.len

    return nothing
end

"""
    separate_kwargs_with_semicolon!(fst::FST)

Ensures keyword arguments are separated by a ";".

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
        else
            fst[comma_idx].val = ";"
            fst[comma_idx].typ = SEMICOLON
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

"""
    remove_superflous_whitespace!(fst::FST)

Soft deletes `WHITESPACE` or `PLACEHOLDER` that's directly followed by a `NEWLINE` or `INLINECOMMENT` node.
"""
function remove_superflous_whitespace!(fst::FST)
    is_leaf(fst) && return
    for (i, n) in enumerate(fst.nodes)
        if (n.typ === WHITESPACE || n.typ === PLACEHOLDER || n.typ === NEWLINE) &&
           i < length(fst.nodes) &&
           (fst[i+1].typ === NEWLINE || fst[i+1].typ === INLINECOMMENT)
            fst[i] = Whitespace(0)
        else
            remove_superflous_whitespace!(n)
        end
    end
    return
end
