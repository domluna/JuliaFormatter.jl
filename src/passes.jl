# FST passes/transforms

"""
Flattens a binary operation call tree if the operation repeats 2 or more times.
"a && b && c" will be transformed while "a && b" will not.
"""
function flatten_binaryopcall(fst::FST; top = true)
    nodes = FST[]

    lhs = fst[1]
    kind = op_kind(fst)
    rhs = fst[end]
    lhs_same_op = op_kind(lhs) === kind
    rhs_same_op = op_kind(rhs) === kind

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
                n.nodes = nnodes
                n.typ = CSTParser.ChainOpCall
            end
        else
            flatten_fst!(n)
        end
    end
end

is_pipe(n) = op_kind(n) === Tokens.RPIPE


"""
    pipe_to_function_call_pass!(fst::FST)

Rewrites `x |> f` to `f(x)`.
"""
function pipe_to_function_call_pass!(fst::FST)
    is_leaf(fst) && return

    if is_pipe(fst)
        fst.nodes = pipe_to_function_call(fst)
        fst.typ = CSTParser.Call
        return
    end

    for n in fst.nodes
        if is_leaf(n)
            continue
        elseif is_pipe(n)
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
        add_node!(use, Whitespace(1), s, join_lines = true)

        # collect the dots prior to a identifier
        # import ..A
        j = i - 1
        while fst[j].typ === CSTParser.OPERATOR
            add_node!(use, fst[j], s, join_lines = true)
            j -= 1
        end

        add_node!(use, FST(CSTParser.IDENTIFIER, sl, el, name), s, join_lines = true)
        add_node!(use, FST(CSTParser.OPERATOR, sl, el, ":"), s, join_lines = true)
        add_node!(use, Whitespace(1), s, join_lines = true)
        add_node!(use, FST(CSTParser.IDENTIFIER, sl, el, name), s, join_lines = true)

        push!(usings, use)
    end
    # @info "" usings[1].startline usings[1].endline usings[end].startline usings[end].endline
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
        add_node!(funcdef, Whitespace(1), s, join_lines = true)

        # func(a) OR func(a) where T
        add_node!(funcdef, fst[1], s, join_lines = true)

        # body
        add_node!(funcdef, fst[end], s, max_padding = s.indent_size)
        add_indent!(funcdef[end], s, s.indent_size)

        # end
        kw = FST(CSTParser.KEYWORD, fst[end].startline, fst[end].endline, "end")
        add_node!(funcdef, kw, s)

        fst.typ = funcdef.typ
        fst.nodes = funcdef.nodes
        fst.len = funcdef.len
        # @info "" funcdef.startline funcdef.endline
    elseif fst[1].typ === CSTParser.BinaryOpCall &&
           fst[1][end].typ === CSTParser.WhereOpCall
        kw = FST(CSTParser.KEYWORD, fst[1].startline, fst[1].endline, "function")
        add_node!(funcdef, kw, s)
        add_node!(funcdef, Whitespace(1), s, join_lines = true)

        # func(a)
        add_node!(funcdef, fst[1][1], s, join_lines = true)

        whereop = fst[1][end]
        decl = FST(CSTParser.OPERATOR, fst[end].startline, fst[end].endline, "::")

        # ::R where T
        add_node!(funcdef, decl, s, join_lines = true)
        add_node!(funcdef, whereop, s, join_lines = true)

        # body
        add_node!(funcdef, fst[end], s, max_padding = s.indent_size)
        add_indent!(funcdef[end], s, s.indent_size)

        # end
        kw = FST(CSTParser.KEYWORD, fst[end].startline, fst[end].endline, "end")
        add_node!(funcdef, kw, s)

        fst.typ = funcdef.typ
        fst.nodes = funcdef.nodes
        fst.len = funcdef.len
        # @info "" funcdef.startline funcdef.endline
    end

    return funcdef
end
