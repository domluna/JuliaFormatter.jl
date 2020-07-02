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

    # @info "a" top kind

    lhs = fst[1]
    rhs = fst[end]
    lhs_kind = op_kind(lhs)
    rhs_kind = op_kind(rhs)
    lhs_same_op = lhs_kind === kind
    rhs_same_op = rhs_kind === kind

    if top && !lhs_same_op && !rhs_same_op
        return nodes
    end

    if lhs_same_op
        push!(nodes, flatten_binaryopcall(lhs, top = false)...)
    else
        flatten_fst!(lhs)
        push!(nodes, lhs)
    end
    # everything except the indentation placeholder
    push!(nodes, fst[2:end-2]...)

    if rhs_same_op
        push!(nodes, flatten_binaryopcall(rhs, top = false)...)
    else
        flatten_fst!(rhs)
        push!(nodes, rhs)
    end

    return nodes
end

function flatten_fst!(fst::FST)
    is_leaf(fst) && return
    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif n.typ === CSTParser.BinaryOpCall && flattenable(op_kind(n))
            # possibly convert BinaryOpCall to ChainOpCall
            nnodes = flatten_binaryopcall(n)
            if length(nnodes) > 0
                n.typ = CSTParser.ChainOpCall
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

    if op_kind(fst) === Tokens.RPIPE
        fst.nodes = pipe_to_function_call(fst)
        fst.typ = CSTParser.Call
        return
    end

    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif op_kind(n) === Tokens.RPIPE
            n.nodes = pipe_to_function_call(n)
            n.typ = CSTParser.Call
        else
            pipe_to_function_call_pass!(n)
        end
    end
end

function pipe_to_function_call(fst::FST)
    nodes = FST[]
    arg2 = fst[end]
    push!(nodes, arg2)
    paren = FST(CSTParser.PUNCTUATION, arg2.endline, arg2.endline, "(")
    push!(nodes, paren)
    pipe_to_function_call_pass!(fst[1])
    arg1 = fst[1]
    push!(nodes, arg1)
    paren = FST(CSTParser.PUNCTUATION, arg1.endline, arg1.endline, ")")
    push!(nodes, paren)
    return nodes
end

function import_to_usings(fst::FST, s::State)
    findfirst(is_colon, fst.nodes) === nothing || return FST[]
    findfirst(n -> is_punc(n) && n.val == ".", fst.nodes) === nothing || return FST[]

    usings = FST[]
    idxs = findall(n -> n.typ === CSTParser.IDENTIFIER, fst.nodes)

    for i in idxs
        name = fst[i].val
        sl = fst[i].startline
        el = fst[i].endline
        use = FST(CSTParser.Using, fst.indent)
        use.startline = sl
        use.endline = el

        add_node!(use, FST(CSTParser.KEYWORD, sl, el, "using"), s)
        add_node!(use, Whitespace(1), s)

        # collect the dots prior to a identifier
        # import ..A
        j = i - 1
        while fst[j].typ === CSTParser.OPERATOR
            add_node!(use, fst[j], s, join_lines = true)
            j -= 1
        end

        add_node!(use, FST(CSTParser.IDENTIFIER, sl, el, name), s, join_lines = true)
        add_node!(use, FST(CSTParser.OPERATOR, sl, el, ":"), s, join_lines = true)
        add_node!(use, Whitespace(1), s)
        add_node!(use, FST(CSTParser.IDENTIFIER, sl, el, name), s, join_lines = true)

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
        if n.typ === CSTParser.IDENTIFIER
            nn = FST(CSTParser.BinaryOpCall, n.indent)
            nn.startline = n.startline
            nn.endline = n.endline
            add_node!(nn, n, s)
            add_node!(
                nn,
                FST(CSTParser.OPERATOR, n.startline, n.endline, "::"),
                s,
                join_lines = true,
            )
            add_node!(
                nn,
                FST(CSTParser.IDENTIFIER, n.startline, n.endline, "Any"),
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

Transforms a _short_ function definition

```julia
f(arg1, arg2) = body
```

to a _long_ function definition

```julia
function f(arg2, arg2)
    body
end
```
"""
function short_to_long_function_def!(fst::FST, s::State)
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

    funcdef = FST(CSTParser.FunctionDef, fst.indent)
    if fst[1].typ === CSTParser.Call || fst[1].typ === CSTParser.WhereOpCall
        # function
        kw = FST(CSTParser.KEYWORD, fst[1].startline, fst[1].endline, "function")
        add_node!(funcdef, kw, s)
        add_node!(funcdef, Whitespace(1), s)

        # func(a)
        # OR
        # func(a) where T
        add_node!(funcdef, fst[1], s, join_lines = true)

        # body
        s.opts.always_use_return && prepend_return!(fst[end], s)
        add_node!(funcdef, fst[end], s, max_padding = s.indent_size)
        add_indent!(funcdef[end], s, s.indent_size)

        # end
        kw = FST(CSTParser.KEYWORD, fst[end].startline, fst[end].endline, "end")
        add_node!(funcdef, kw, s)

        fst.typ = funcdef.typ
        fst.nodes = funcdef.nodes
        fst.len = funcdef.len
    elseif fst[1].typ === CSTParser.BinaryOpCall &&
           fst[1][end].typ === CSTParser.WhereOpCall
        # function
        kw = FST(CSTParser.KEYWORD, fst[1].startline, fst[1].endline, "function")
        add_node!(funcdef, kw, s)
        add_node!(funcdef, Whitespace(1), s)

        # func(a)
        add_node!(funcdef, fst[1][1], s, join_lines = true)

        whereop = fst[1][end]
        decl = FST(CSTParser.OPERATOR, fst[1].startline, fst[1].endline, "::")

        # ::R where T
        add_node!(funcdef, decl, s, join_lines = true)
        add_node!(funcdef, whereop, s, join_lines = true)

        # body
        s.opts.always_use_return && prepend_return!(fst[end], s)
        add_node!(funcdef, fst[end], s, max_padding = s.indent_size)
        add_indent!(funcdef[end], s, s.indent_size)

        # end
        kw = FST(CSTParser.KEYWORD, fst[end].startline, fst[end].endline, "end")
        add_node!(funcdef, kw, s)

        fst.typ = funcdef.typ
        fst.nodes = funcdef.nodes
        fst.len = funcdef.len
    end
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
CSTParser.BinaryOpCall
 - CSTParser.WhereOpCall
 - OP
 - RHS
```

It's parsed as (2):

```
CSTParser.BinaryOpCall
 - CSTParser.BinaryOpCall
  - LHS
  - OP
  - CSTParser.WhereOpCall
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
    # transform fst[1] to a WhereOpCall
    oldbinop = fst[1]
    oldwhereop = fst[1][end]
    binop = FST(CSTParser.BinaryOpCall, fst[1].indent)

    # foo(a::A)
    add_node!(binop, oldbinop[1], s)
    # foo(a::A)::
    add_node!(binop, oldbinop[2], s, join_lines = true)
    # foo(a::A)::R
    add_node!(binop, oldwhereop[1], s, join_lines = true)

    whereop = FST(CSTParser.WhereOpCall, fst[1].indent)
    add_node!(whereop, binop, s)

    # "foo(a::A)::R where A"
    for n in oldwhereop[2:end]
        add_node!(whereop, n, s, join_lines = true)
    end

    fst[1] = whereop
end

"""
    prepend_return!(fst::FST, s::State)

Prepends `return` to the last expression of a block.

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
    fst.typ === CSTParser.Block || return
    fst[end].typ !== CSTParser.Return || return

    ret = FST(CSTParser.Return, fst.indent)
    ln = fst[end]
    kw = FST(CSTParser.KEYWORD, fst[end].startline, fst[end].endline, "return")
    add_node!(ret, kw, s)
    add_node!(ret, Whitespace(1), s)
    add_node!(ret, fst[end], s, join_lines = true)
    fst[end] = ret
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
    f = (t) -> (n, s) -> is_leaf(n) && push!(t, n)
    walk(f(t), fst, s)

    macroname = FST(CSTParser.MacroName, fst.indent)
    for (i, n) in enumerate(t)
        if n.val == "@"
            continue
        elseif i < length(t)
            add_node!(macroname, n, s, join_lines = true)
        else
            at = FST(CSTParser.PUNCTUATION, n.startline, n.endline, "@")
            add_node!(macroname, at, s, join_lines = true)
            add_node!(macroname, n, s, join_lines = true)
        end
    end

    return macroname
end

"""
    remove_superflous_whitespace!(fst::FST)

Removes whitespace that's directly followed by a `NEWLINE` or `INLINECOMMENT` node.

NOTE: Instead of explicitly removing the `WHITESPACE` node it's replaced with another `WHITESPACE`
node of length 0.

"""
function remove_superflous_whitespace!(fst::FST)
    is_leaf(fst) && return
    for (i, n) in enumerate(fst.nodes)
        if n.typ === WHITESPACE &&
           i < length(fst.nodes) &&
           (fst[i+1].typ === NEWLINE || fst[i+1].typ === INLINECOMMENT)
            fst[i] = Whitespace(0)
        end
    end
end
