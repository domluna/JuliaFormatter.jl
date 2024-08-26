flattenable(k) = kind(k) in KSet"&& || |>"
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
    idx = findlast(n -> n.typ === PLACEHOLDER, fst.nodes::Vector)

    if (top && !lhs_same_op && !rhs_same_op) || idx === nothing
        return nodes
    end

    if lhs_same_op
        append!(nodes, flatten_binaryopcall(lhs, top = false))
    else
        flatten_fst!(lhs)
        push!(nodes, lhs)
    end
    # everything except the indentation placeholder
    append!(nodes, fst[2:idx-1])

    if rhs_same_op
        append!(nodes, flatten_binaryopcall(rhs, top = false))
    else
        flatten_fst!(rhs)
        push!(nodes, rhs)
    end

    return nodes
end

function flatten_conditionalopcall(fst::FST)
    nodes = FST[]
    for n in fst.nodes::Vector
        if n.typ === Conditional
            append!(nodes, flatten_conditionalopcall(n))
        else
            push!(nodes, n)
        end
    end
    return nodes
end

function flatten_fst!(fst::FST)
    is_leaf(fst) && return
    for n in fst.nodes::Vector
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
    if op_kind(fst) === K"|>" && (fst[end].typ !== PUNCTUATION)
        fst.nodes = pipe_to_function_call(fst)
        fst.typ = Call
        return
    end
    for n in fst.nodes::Vector
        if is_leaf(n)
            continue
        elseif op_kind(n) === K"|>" && (n[end].typ !== PUNCTUATION)
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

    # is RHS is an anon function?
    # need to wrap it parens, i.e. "(x -> x + 1)(arg)"
    # and then possibly add a "." as well !
    if op_kind(arg2) === K"->"
        n = FST(PUNCTUATION, -1, arg2.endline, arg2.endline, "(")
        push!(nodes, n)

        # go into anon function and convert all the pipe calls there too.
        # The precedence of -> is greater then |> so if the anon func is of the
        # the form `x -> x |> f`, the pipe call will not be converted unless well
        # recurse into the anon func.
        pipe_to_function_call_pass!(arg2)

        push!(nodes, arg2)
        n = FST(PUNCTUATION, -1, arg2.endline, arg2.endline, ")")
        push!(nodes, n)
        if dot
            n = FST(PUNCTUATION, -1, arg2.endline, arg2.endline, ".")
            push!(nodes, n)
        end
    else
        push!(nodes, arg2)

        if dot && arg2.typ === IDENTIFIER
            n = FST(PUNCTUATION, -1, arg2.endline, arg2.endline, ".")
            push!(nodes, n)
        elseif dot &&
               arg2.typ === Binary &&
               arg2[end].typ === Quotenode &&
               arg2[end][end].typ === IDENTIFIER
            n = FST(PUNCTUATION, -1, arg2.endline, arg2.endline, ".")
            push!(nodes, n)
        elseif dot && arg2.typ === Brackets
            idx = findfirst(n -> n.typ === Binary && op_kind(n) === K"->", arg2.nodes)
            if idx !== nothing
                n = FST(PUNCTUATION, -1, arg2.endline, arg2.endline, ".")
                push!(nodes, n)
            end
        end
    end

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
    nodes = fst.nodes::Vector
    findfirst(n -> is_colon(n) || n.typ === As, nodes) === nothing || return FST[]
    findfirst(n -> n.typ === PUNCTUATION && n.val == ".", fst[3].nodes) === nothing ||
        return FST[]

    # handle #723 "import ..f" should not become "using ..f: f"
    if length(nodes) == 3 && !is_leaf(nodes[3]) && nodes[3].nodes[1].val == "."
        return FST[]
    end

    usings = FST[]
    idxs = findall(n -> !is_leaf(n), nodes)

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
        colon.metadata = Metadata(K"::", false)
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
    for (i, n) in enumerate(fst.nodes::Vector)
        if n.typ === IDENTIFIER
            nn = FST(Binary, n.indent)
            nn.startline = n.startline
            nn.endline = n.endline
            add_node!(nn, n, s)
            line_offset = n.line_offset + length(n)
            op = FST(OPERATOR, line_offset, n.startline, n.endline, "::")
            op.metadata = Metadata(K"::", false)
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
    (fst[1].typ !== Call && fst[1].typ !== Where) && return false

    # TODO: what does this mean?
    # do not apply if parent is a function or macro definition
    # parent_is(
    #     fst.ref[],
    #     n -> is_function_or_macro_def(n) || n.head == :macrocall;
    #     ignore = n -> is_block(n),
    # ) && return false

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
    elseif fst[end].typ === Begin
        # case where body is wrapped in a `begin` block
        # which becomes superfluous when converted to a
        # long function definition
        #
        # abc() = begin
        #    body
        # end
        #
        #
        # find Block node in fs[tend]
        idx = findfirst(n -> n.typ === Block, fst[end].nodes)
        idx === nothing && return false
        bnode = fst[end][idx]
        add_indent!(bnode, s, -s.opts.indent)
        add_node!(funcdef, bnode, s, max_padding = s.opts.indent)
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
    return true
end

"""
    long_to_short_function_def!(fst::FST, s::State)

Transforms a *long* function definition

```julia
function f(arg2, arg2)
    body
end
```

to a *short* function definition

```julia
f(arg1, arg2) = body
```
"""
function long_to_short_function_def!(fst::FST, s::State)
    nodes = fst.nodes::Vector
    any(is_comment, nodes) && return false

    I = findall(n -> n.typ === Block, nodes)
    length(I) == 1 || return false  # function must have a single block

    block = nodes[first(I)]
    length(block.nodes::Vector{FST}) == 1 || return false  # block must have a single statement

    I = findfirst(n -> n.typ === Call || n.typ === Where, nodes)
    I === nothing && return false
    lhs = nodes[I]

    rhs = first(block.nodes)

    if rhs.typ === Return
        rhs = (rhs.nodes::Vector{FST})[end]
    end

    # length(Whitespace(1) * "=" * Whitespace(1)) = 3
    line_margin = s.line_offset + length(lhs) + 3 + length(rhs) + fst.extra_margin
    line_margin > s.opts.margin && return false

    if rhs.indent > 0
        add_indent!(rhs, s, -s.opts.indent)
    end

    funcdef = FST(Binary, fst.indent)
    funcdef.metadata = Metadata(K"=", false, false, true, false, false)
    kw = (join_lines = true, override_join_lines_based_on_source = true)

    add_node!(funcdef, lhs, s; kw...)
    add_node!(funcdef, Whitespace(1), s; kw...)
    op = FST(OPERATOR, 0, -1, -1, "=")
    op.metadata = Metadata(K"=", false)
    add_node!(funcdef, op, s; kw...)
    add_node!(funcdef, Placeholder(1), s; kw...)
    add_node!(funcdef, Placeholder(0), s; kw...)
    add_node!(funcdef, rhs, s; kw...)

    if rhs.typ in (If, Do, Try, For, While, Let)
        funcdef.nest_behavior = AlwaysNest
    end

    fst.typ = funcdef.typ
    fst.nodes = funcdef.nodes
    fst.len = funcdef.len
    fst.metadata = funcdef.metadata
    fst.nest_behavior = funcdef.nest_behavior
    return true
end

# TODO: revisit this
"""
    binaryop_to_whereop(fst::FST, s::State)

Handles the case of a function def defined as:

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

    # get everything up to the where
    binop = FST(Binary, fst[1].indent)
    for n in oldbinop.nodes
        if n.typ === Where
            break
        end
        add_node!(binop, n, s, join_lines = true)
    end
    # # foo(a::A)::R gets the "R"
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
    length(fst.nodes::Vector{FST}) == 0 && return
    ln = fst[end]
    is_block(ln) && return
    ln.typ === Return && return
    ln.typ === MacroCall && return
    ln.typ === MacroBlock && return
    ln.typ === MacroStr && return
    if length(fst.nodes::Vector) > 2 &&
       (fst[end-2].typ === MacroStr || is_macrodoc(fst[end-2]))
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
    lo = s.line_offset
    walk(f, ln, s)
    s.line_offset = lo
    found_return && return

    ret = FST(Return, fst.indent)
    kw = FST(KEYWORD, -1, ln.startline, ln.startline, "return")
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

Moves `@` to the last identifier.

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
    val = ""
    has_at = false
    f = (n::FST, _) -> begin
        if n.typ === MACRONAME && n.line_offset == -33
            val *= gettreeval(n)
            return false
        elseif is_leaf(n)
            v = gettreeval(n)
            has_at = has_at || contains(v, "@")
            v = replace(v, "@" => "")
            val *= v
        end
    end
    walk(f, fst, s)

    if !has_at
        return fst
    end

    # Find the last occurrence of . and insert @ after it
    last_dot_index = findlast('.', val)
    if last_dot_index !== nothing
        val = val[1:last_dot_index] * "@" * val[last_dot_index+1:end]
    else
        # If there's no dot, add @ to the beginning
        val = "@" * val
    end

    return FST(MACRONAME, -33, fst.startline, fst.startline, val)
end

function conditional_to_if_block!(fst::FST, s::State; top = true)
    t = FST(If, fst.indent)
    kw = FST(KEYWORD, -1, fst.startline, fst.startline, top ? "if" : "elseif")
    add_node!(t, kw, s, max_padding = 0)
    add_node!(t, Whitespace(1), s, join_lines = true)
    add_node!(t, fst[1], s, join_lines = true)

    nodes = fst.nodes::Vector
    idx1 = findfirst(n -> n.typ === OPERATOR && n.val == "?", nodes)::Int
    idx2 = findfirst(n -> n.typ === OPERATOR && n.val == ":", nodes)::Int

    block1 = FST(Block, fst.indent + s.opts.indent)
    for n in nodes[idx1+1:idx2-1]
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
    nodes = fst.nodes::Vector
    kw_idx = findfirst(n -> n.typ === Kw, nodes)
    isnothing(kw_idx) && return
    parameters_idx = findfirst(n -> n.typ === Parameters, nodes)
    # first "," prior to a kwarg
    comma_idx = findlast(is_comma, nodes[1:kw_idx-1])
    ph_idx = findlast(n -> n.typ === PLACEHOLDER, nodes[1:kw_idx-1])

    if !isnothing(parameters_idx) && parameters_idx > kw_idx
        params_node = nodes[parameters_idx]
        sc_idx = findfirst(n -> n.typ === SEMICOLON, params_node.nodes)
        # move ; prior to first kwarg
        params_node[sc_idx].val = ","
        params_node[sc_idx].typ = PUNCTUATION
        if isnothing(comma_idx)
            if !isnothing(ph_idx)
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
    elseif isnothing(parameters_idx) && isnothing(comma_idx)
        if !isnothing(ph_idx)
            fst[ph_idx] = Placeholder(1)
            insert!(fst, ph_idx, Semicolon())
        else
            insert!(fst, kw_idx, Placeholder(1))
            insert!(fst, kw_idx, Semicolon())
        end
    elseif isnothing(parameters_idx)
        fst[comma_idx].val = ";"
        fst[comma_idx].typ = SEMICOLON
    end

    return
end

"""
    remove_superfluous_whitespace!(fst::FST)

Soft deletes `WHITESPACE` or `PLACEHOLDER` that's directly followed by a `NEWLINE` or `INLINECOMMENT` node.
"""
function remove_superfluous_whitespace!(fst::FST)
    is_leaf(fst) && return
    nodes = fst.nodes::Vector
    for (i, n) in enumerate(nodes)
        if (n.typ === WHITESPACE || n.typ === PLACEHOLDER || n.typ === NEWLINE) &&
           i < length(nodes) &&
           (fst[i+1].typ === NEWLINE || fst[i+1].typ === INLINECOMMENT)
            fst[i] = Whitespace(0)
        else
            remove_superfluous_whitespace!(n)
        end
    end
    return
end

function _short_circuit_to_if!(fst::FST, s::State)
    # change it into an if
    t = FST(If, fst.indent)
    kw = FST(KEYWORD, -1, fst.startline, fst.startline, "if")
    add_node!(t, kw, s, max_padding = 0)
    add_node!(t, Whitespace(1), s, join_lines = true)

    nodes = fst.nodes::Vector
    idx = findlast(n -> n.typ === OPERATOR && (n.val == "||" || n.val == "&&"), nodes)::Int
    is_or = nodes[idx].val == "||"

    wrap_with_parens = !(fst.nodes[1].typ === Brackets)

    if is_or
        call = FST(Unary, fst.indent)
        add_node!(call, FST(OPERATOR, -1, fst.startline, fst.startline, "!"), s)

        if wrap_with_parens
            brackets = FST(Brackets, fst.indent)
            add_node!(
                brackets,
                FST(PUNCTUATION, -1, fst.startline, fst.startline, "("),
                s,
                join_lines = true,
            )
            add_node!(brackets, Placeholder(0), s)
            # inner
            lhs = FST(Chain, fst.indent)
            for n in nodes[1:idx-1]
                add_node!(lhs, n, s, join_lines = true)
            end
            # remove extra ws
            if lhs[end].typ === WHITESPACE
                lhs[end] = Whitespace(0)
            end

            add_node!(brackets, lhs, s, join_lines = true)
            add_node!(brackets, Placeholder(0), s)
            add_node!(
                brackets,
                FST(PUNCTUATION, -1, nodes[idx-2].startline, nodes[idx-2].startline, ")"),
                s,
                join_lines = true,
            )
            add_node!(call, brackets, s, join_lines = true)
        else
            add_node!(call, fst[1], s, join_lines = true)
        end

        add_node!(t, call, s, join_lines = true)
    else
        # from idx-1 go backwards until we find a node that's not a
        lhs = FST(Chain, fst.indent)
        for n in nodes[1:idx-1]
            add_node!(lhs, n, s, join_lines = true)
        end
        # remove extra ws
        if lhs[end].typ === WHITESPACE
            lhs[end] = Whitespace(0)
        end
        add_node!(t, lhs, s, join_lines = true)
    end

    block1 = FST(Block, fst.indent + s.opts.indent)
    for n in nodes[idx+1:end]
        if n.typ === PLACEHOLDER ||
           n.typ === WHITESPACE ||
           n.typ === NEWLINE ||
           is_comment(n)
            continue
        end
        add_node!(block1, n, s)
    end
    add_node!(t, block1, s, max_padding = s.opts.indent)

    kw = FST(KEYWORD, -1, -1, -1, "end")
    add_node!(t, kw, s, max_padding = 0)

    fst.typ = t.typ
    fst.nodes = t.nodes
    fst.len = t.len

    return nothing
end

function short_circuit_to_if_pass!(fst::FST, s::State)
    is_leaf(fst) && return
    for n in fst.nodes::Vector
        if is_leaf(n)
            continue
        elseif (n.typ === Binary || n.typ === Chain) &&
            !isnothing(n.metadata) && n.metadata.is_standalone_shortcircuit
            _short_circuit_to_if!(n, s)
        else
            short_circuit_to_if_pass!(n, s)
        end
    end
end
