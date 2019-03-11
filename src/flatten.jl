
# String[]
struct Edit{T<:CSTParser.LeafNode}
    startline::Int
    endline::Int
    text::String
end
Base.length(e::Edit) = length(e.text)
merge(s::String, e::Edit{T}) where {T} = Edit{T}(e.startline, e.endline, s * e.text)
merge(e::Edit{T}, s::String) where {T} = Edit{T}(e.startline, e.endline, e.text * s)

#= nl(startline, endline) = Edit(startline, endline, "\n") =#
#= nest(startline, endline, width::Int) = Edit(startline, endline, repeat(" ", width)) =#
nest(startline, endline, width::Int) = Edit(startline, endline, "\n" * repeat(" ", width))

mutable struct EditTree{T<:CSTParser.AbstractEXPR}
    startline::Int
    endline::Int
    edits::Vector{Union{Edit, EditTree}}
    # Flag to note if a CSTParser.Block should be
    # on a single line.
    single_line::Bool
end

function EditTree{T}(edits, single_line=false) where {T}
    l1 = minimum(map(e -> e.startline, edits))
    l2 = maximum(map(e -> e.endline, edits))
    EditTree{T}(l1, l2, edits, single_line)
end

function merge(s::String, t::EditTree)
    @assert length(t.edits) > 0
    e = Edit{CSTParser.LITERAL}(t.edits[1].startline, t.edits[1].endline, s)
    insert!(t.edits, 1, e)
    return t
end

function merge(t::EditTree, s::String)
    @assert length(t.edits) > 0
    e = Edit{CSTParser.LITERAL}(t.edits[end].startline, t.edits[end].endline, s)
    push!(t.edits, e)
    return t
end
Base.length(t::EditTree) = length(t.edits)

#= function Base.push!(t::EditTree, e::Union{EditTree,Edit}) =#
#=     e.startline < t.startline && (t.startline = e.startline) =#
#=     e.endline > t.endline && (t.endline = e.endline) =#
#=     push!(t.edits, e) =#
#=     return nothing =#
#= end =#

function flatten(x::CSTParser.IDENTIFIER, s::State)
    loc = cursor_loc(s)
    s.offset += x.fullspan
    return Edit{typeof(x)}(loc[1], loc[1], x.val)
end

function flatten(x::CSTParser.OPERATOR, s::State)
    loc = cursor_loc(s)
    text = string(CSTParser.Expr(x))
    s.offset += x.fullspan
    return Edit{typeof(x)}(loc[1], loc[1], text)
end

function flatten(x::CSTParser.KEYWORD, s::State)
    loc = cursor_loc(s)
    text = ""
    text = x.kind == Tokens.ABSTRACT ? "abstract " :
        x.kind == Tokens.BAREMODULE ? "baremodule " :
        x.kind == Tokens.BEGIN ? "begin" :
        x.kind == Tokens.BREAK ? "break" :
        x.kind == Tokens.CATCH ? "catch" :
        x.kind == Tokens.CONST ? "const " :
        x.kind == Tokens.CONTINUE ? "continue" :
        x.kind == Tokens.DO ? " do " :
        x.kind == Tokens.IF ? "if " :
        x.kind == Tokens.ELSEIF ? "elseif " :
        x.kind == Tokens.ELSE ? "else" :
        x.kind == Tokens.END ? "end" :
        x.kind == Tokens.EXPORT ? "export " :
        x.kind == Tokens.FINALLY ? "finally" :
        x.kind == Tokens.FOR ? "for " :
        x.kind == Tokens.FUNCTION ? "function " :
        x.kind == Tokens.GLOBAL ? "global " :
        x.kind == Tokens.IMPORT ? "import " :
        x.kind == Tokens.IMPORTALL ? "importall " :
        x.kind == Tokens.LET ? "let " :
        x.kind == Tokens.LOCAL ? "local " :
        x.kind == Tokens.MACRO ? "macro " :
        x.kind == Tokens.MODULE ? "module " :
        x.kind == Tokens.MUTABLE ? "mutable " :
        x.kind == Tokens.OUTER ? "outer " :
        x.kind == Tokens.PRIMITIVE ? "primitive " :
        x.kind == Tokens.QUOTE ? "quote" :
        x.kind == Tokens.RETURN ? "return" :
        x.kind == Tokens.STRUCT ? "struct " :
        x.kind == Tokens.TRY ? "try" :
        x.kind == Tokens.TYPE ? "type " :
        x.kind == Tokens.USING ? "using " :
        x.kind == Tokens.WHILE ? "while " : ""
    s.offset += x.fullspan
    return Edit{typeof(x)}(loc[1], loc[1], text)
end

function flatten(x::CSTParser.PUNCTUATION, s::State)
    loc = cursor_loc(s)
    text = x.kind == Tokens.LPAREN ? "(" :
        x.kind == Tokens.LBRACE ? "{" :
        x.kind == Tokens.LSQUARE ? "[" :
        x.kind == Tokens.RPAREN ? ")" :
        x.kind == Tokens.RBRACE ? "}" :
        x.kind == Tokens.RSQUARE ? "]" :
        x.kind == Tokens.COMMA ? "," :
        x.kind == Tokens.SEMICOLON ? ";" :
        x.kind == Tokens.AT_SIGN ? "@" :
        x.kind == Tokens.DOT ? "." : ""
    s.offset += x.fullspan
    return Edit{typeof(x)}(loc[1], loc[1], text)
end

# TODO: don't escape newlines in TRIPLE_STRING
# this needs a change in CSTParser
function flatten(x::CSTParser.LITERAL, s::State, surround_with_quotes=true)
    loc = cursor_loc(s)
    text = x.kind == Tokens.TRIPLE_STRING && surround_with_quotes ? "\"\"\"" * escape_string(x.val, "\$") * "\"\"\"" :
           x.kind == Tokens.STRING && surround_with_quotes ? "\"" * escape_string(x.val, "\$") * "\"" :
           x.val
    s.offset += x.fullspan
    return Edit{typeof(x)}(loc[1], loc[1], text)
end

function flatten(x::T, s::State) where T <: Union{CSTParser.AbstractEXPR,Vector}
    edits = Union{Edit, EditTree}[]
	for a in x
        push!(edits, flatten(a, s))
	end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.StringH}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        e = flatten(a, s)
        if i == 1
            push!(edits, Edit{CSTParser.LITERAL}(e.startline, e.endline, "\""))
        end
        if a isa CSTParser.LITERAL && a.val != ""
            push!(edits, e)
        end
        if i == length(x)
            push!(edits, Edit{CSTParser.LITERAL}(e.startline, e.endline, "\""))
        end
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.MacroCall}, s::State)
    # Docstring
    edits = Union{Edit, EditTree}[]
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        #= @info "HERE" =#
        #= flatten(x.args[1], s) =#

        offset = s.offset
        loc1 = cursor_loc(s)
        s.offset += x.args[2].fullspan
        loc2 = cursor_loc(s, s.offset-1)
        #= @info "DOC POSITION START", loc1 =#
        #= @info "DOC POSITION END", loc2 =#

        tq = "\"\"\""
        w = repeat(" ", s.indent_width * s.indents)
        is_ts = startswith(s.doc.text[offset:offset+loc1[3]-loc1[2]], tq)
        quote_len = is_ts ? 3 : 1
        #= @info "STARTS WITH TRIPLE QUOTES", is_ts =#

        push!(edits, Edit{CSTParser.LITERAL}(loc1[1], loc1[1], tq * "\n"))

        v = ""
        if loc1[3] - loc1[2] > quote_len
            sidx = is_ts ? offset + 3 : offset + 1
            if loc1[1] == loc2[1]
                eidx = is_ts ? offset+loc1[3]-loc1[2]-4 : offset+loc1[3]-loc1[2]-2
                v = s.doc.text[sidx:eidx]
                #= @info "H1", v =#
            else
                #= eidx = is_ts ? o+loc1[3]-loc1[2]-1 : o+loc1[3]-loc1[2]-1 =#
                eidx = offset+loc1[3]-loc1[2]-1
                v = s.doc.text[sidx:eidx]
                #= @info "H2", v =#
            end
            push!(edits, Edit{CSTParser.LITERAL}(loc1[1], loc1[1], v))
        end

        offset = s.offset
        if loc1[1] == loc2[1]
            push!(edits, Edit{CSTParser.LITERAL}(loc2[1]+1, loc2[1]+1, "\n" * tq * "\n"))
        #= elseif loc2[3] > quote_len + 1 =#
        elseif loc2[3] > quote_len + 1
            v = strip(is_ts ? s.doc.text[offset-loc2[2]:offset-5] : s.doc.text[offset-loc2[2]:offset-3])
            #= @info "H3", v, loc1[1], loc2[1] =#
            if v  != ""
                push!(edits, Edit{CSTParser.LITERAL}(loc2[1], loc2[1], v))
            end
            push!(edits, Edit{CSTParser.LITERAL}(loc2[1], loc2[1], "\n" * tq * "\n"))
            #= push!(edits, Edit{CSTParser.LITERAL}(loc2[1], loc2[1],  tq * "\n")) =#
        elseif v != ""
            push!(edits, Edit{CSTParser.LITERAL}(loc2[1], loc2[1], "\n" * tq * "\n"))
        else
            push!(edits, Edit{CSTParser.LITERAL}(loc2[1], loc2[1], tq * "\n"))
        end

        push!(edits, flatten(x.args[3], s))
        return EditTree{typeof(x)}(edits)
    end

    # same as CSTParser.EXPR{CSTParser.CALL} but whitespace sensitive
    for (i, a) in enumerate(x)
        e = flatten(a, s)
        if a isa CSTParser.KEYWORD
            e = merge(e, " ")
        elseif CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            e = merge(e, " ")
        else
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.MacroName}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        e = flatten(a, s)
        if i == length(x) && x.fullspan > x.span
            e = merge(e, " ")
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

# TODO: Block and/or If nodes causing trouble
function flatten(x::CSTParser.EXPR{CSTParser.Block}, s::State; ignore_single_line=false)
    single_line = ignore_single_line ? false : cursor_loc(s)[1] == cursor_loc(s, s.offset+x.span-1)[1] 
    @info "BLOCK" single_line x.args

    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        e = flatten(a, s)
        if i < length(x) && CSTParser.is_comma(a) && x.args[i+1] isa CSTParser.PUNCTUATION
            push!(edits, e)
        elseif CSTParser.is_comma(a) && i != length(x)
            e = merge(e, " ")
            push!(edits, e)
        elseif single_line
            if i == 1 || CSTParser.is_comma(x.args[i-1])
                push!(edits, e)
            else
                e = merge(e, "; ")
                push!(edits, e)
            end
        else
            @info "" i length(x) typeof(e) e a
            # Corner case
            if typeof(e) !== EditTree{CSTParser.EXPR{CSTParser.If}}
                e = merge(e, "\n")
            end
            push!(edits, e)
        end
    end

    if length(edits) == 0
        loc = cursor_loc(s)
        push!(edits, Edit{CSTParser.LITERAL}(loc[1], loc[1], ""))
    end

    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Abstract}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        e = flatten(a, s)
        if i == length(x)
            e = merge(e, " ")
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.FunctionDef}, s::State)
    edits = Union{Edit, EditTree}[]
    if length(x) > 3
        for (i, a) in enumerate(x)
            e = i == 3 ? flatten(a, s; ignore_single_line=true) : flatten(a, s)
            i == 2 && (e = merge(e, "\n"))
            push!(edits, e)
        end
    else
        for (i, a) in enumerate(x)
            e = flatten(a, s)
            i == 2 && (e = merge(e, " "))
            push!(edits, e)
        end
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Macro,CSTParser.Struct,CSTParser.For,CSTParser.While}
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        e = i == 3 ? flatten(a, s; ignore_single_line=true) : flatten(a, s)
        i == 2 && (e = merge(e, "\n"))
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Mutable}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        e = i == 4 ? flatten(a, s; ignore_single_line=true) : flatten(a, s)
        i == 3 && (e = merge(e, "\n"))
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Do}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        if i == 4 && a isa CSTParser.EXPR{CSTParser.Block}
            e = flatten(a, s; ignore_single_line=true)
        elseif i == 3
            e = flatten(a, s)
            e = merge(e, "\n")
        else
            e = flatten(a, s)
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Try}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        if i in (2, 5, 7)
            e = flatten(a, s; ignore_single_line=true)
        elseif i in (1, 4, 6)
            e = flatten(a, s)
            e = merge(e, "\n")
        else
            e = flatten(a, s)
            if i == 3 && x.args[i+1].fullspan != 0
                e = merge(e, " ")
            end
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Return}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        e = flatten(a, s)
        if i == 1 && x.args[2].fullspan != 0
            e = merge(e, " ")
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Quote}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
            if i == 1
                e = flatten(a, s)
                e = merge(e, "\n")
            elseif i == 2
                e = flatten(a, s; ignore_single_line=true)
            else
                e = flatten(a, s)
            end
        else
            e = flatten(a, s)
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Begin}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        if i == 1
            e = flatten(a, s)
            e = merge(e, "\n")
        elseif i == 2
            e = flatten(a, s; ignore_single_line=true)
        else
            e = flatten(a, s)
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.ModuleH}, s::State)
    edits = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x)
        if i == 2
            e = flatten(a, s)
            e = merge(e, "\n")
        elseif i == 3
            e = flatten(a, s; ignore_single_line=true)
        else
            e = flatten(a, s)
        end
        push!(edits, e)
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Let}, s::State)
    edits = Union{Edit, EditTree}[]
    if length(x) > 3
        for (i, a) in enumerate(x)
            if i == 2
                e = flatten(a, s)
                e = merge(e, "\n")
            elseif i == 3
                e = flatten(a, s; ignore_single_line=true)
            else
                e = flatten(a, s)
            end
            push!(edits, e)
        end
    else
        for (i, a) in enumerate(x)
            if i == 1
                e = flatten(a, s)
                e = merge(e, "\n")
            elseif i == 2
                e = flatten(a, s; ignore_single_line=true)
            else
                e = flatten(a, s)
            end
            push!(edits, e)
        end
    end
    return EditTree{typeof(x)}(edits)
end

# newlines after i == 2, 5 if elseif 4 otherwise
function flatten(x::CSTParser.EXPR{CSTParser.If}, s::State)
    edits = Union{Edit, EditTree}[]
    push!(edits, flatten(x.args[1], s))
    #= if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.IF =#
    #=     for (i, a) in enumerate(x) =#
    #=         if i == 2 =#
    #=             e = flatten(a, s) =#
    #=             e = merge(e, "\n") =#
    #=         elseif i == 3 =#
    #=             e = flatten(a, s; ignore_single_line=true) =#
    #=         elseif i == 4 && x.args[4].kind == Tokens.ELSE =#
    #=             e = flatten(a, s) =#
    #=             #= e = merge(e, "\n") =# =#
    #=         elseif i == 5 =#
    #=             if x.args[4].kind == Tokens.ELSE =#
    #=                 e = flatten(a, s; ignore_single_line=true) =#
    #=             else =#
    #=                 e = flatten(a, s) =#
    #=             end =#
    #=         else =#
    #=             e = flatten(a, s) =#
    #=         end =#
    #=         push!(edits, e) =#
    #=     end =#
    #= else =#
    #=     e = flatten(x.args[1], s) =#
    #=     e = merge(e, "\n") =#
    #=     push!(edits, e) =#
    #=  =#
    #=     e = flatten(x.args[2], s; ignore_single_line=true) =#
    #=     push!(edits, e) =#
    #=  =#
    #=     if length(x) > 2 =#
    #=         if x.args[3].kind == Tokens.ELSE =#
    #=             e = flatten(x.args[3], s) =#
    #=             e = merge(e, "\n") =#
    #=             push!(edits, e) =#
    #=         else =#
    #=             e = flatten(x.args[3], s) =#
    #=             push!(edits, e) =#
    #=         end =#
    #=  =#
    #=         if x.args[3].kind == Tokens.ELSE =#
    #=             e = flatten(x.args[4], s; ignore_single_line=true) =#
    #=             push!(edits, e) =#
    #=         else =#
    #=             e = flatten(x.args[4], s) =#
    #=             #= e = merge(e, "\n") =# =#
    #=             push!(edits, e) =#
    #=         end =#
    #=     end =#
    #= end =#
    #= return EditTree{typeof(x)}(edits) =#
    #

    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.IF
        push!(edits, merge(flatten(x.args[2], s), "\n"))
        #= push!(edits, flatten(x.args[2], s)) =#
        push!(edits, flatten(x.args[3], s; ignore_single_line=true))
        #= push!(edits, flatten(x.args[4], s)) =#
        if length(x.args) > 4
            # this either else or elseif
            push!(edits, flatten(x.args[4], s))
            if x.args[4].kind == Tokens.ELSEIF
                push!(edits, merge(flatten(x.args[5], s), "\n"))
            else
                push!(edits, flatten(x.args[4], s))
                push!(edits, flatten(x.args[5], s; ignore_single_line=true))
            end
            # END
            push!(edits, flatten(x.args[6], s))
        end
    else
        # EXPR{Block}
        push!(edits, flatten(x.args[2], s; ignore_single_line=true))
        if length(x.args) > 2
            # this either else or elseif
            push!(edits, flatten(x.args[3], s))
            if x.args[3].kind == Tokens.ELSEIF
                push!(edits, merge("\n", flatten(x.args[4], s), "\n"))
            else
                push!(edits, flatten(x.args[4], s; ignore_single_line=true))
            end
        end
    end
    return EditTree{typeof(x)}(edits)
end

function flatten(x::Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}, s::State) 
    edits = Union{Edit, EditTree}[]
    arg1 = flatten(x.arg1, s)
    push!(edits, arg1)
    op = flatten(x.op, s)
    if CSTParser.precedence(x.op) in (8, 13, 14, 16) && x.op.kind != Tokens.ANON_FUNC
    elseif x.op.kind == Tokens.EX_OR
        op = merge(" ", op)
    else
        op = merge(" ", op)
        op = merge(op, " ")
    end
    #= @info op =#
    #= arg1 = merge(arg1, op) =#
    push!(edits, op)
    arg2 = flatten(x.arg2, s)
    push!(edits, arg2)
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.WhereOpCall, s::State)
    edits = Union{Edit, EditTree}[]
    push!(edits, flatten(x.arg1, s))
    op = flatten(x.op, s)
    op = merge(" ", op)
    op = merge(op, " ")

    push!(edits, op)

    e = Union{Edit, EditTree}[]
    for a in x.args
        push!(e, flatten(a, s))
        if CSTParser.is_comma(a)
            typ = typeof(e[1]).parameters[1]
            t = EditTree{typ}(e)
            push!(edits, t)
            e = Union{Edit, EditTree}[]
        end
    end
    typ = typeof(e[1]).parameters[1]
    t = EditTree{typ}(e)
    push!(edits, t)

    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.ConditionalOpCall, s::State)
    edits = Union{Edit, EditTree}[]
    push!(edits, flatten(x.cond, s))
    op = flatten(x.op1, s)
    op = merge(" ", op)
    op = merge(op, " ")
    push!(edits, op)

    push!(edits, flatten(x.arg1, s))
    op = flatten(x.op2, s)
    op = merge(" ", op)
    op = merge(op, " ")
    push!(edits, op)

    push!(edits, flatten(x.arg2, s))
    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.InvisBrackets,CSTParser.Braces}
    edits = Union{Edit, EditTree}[]
    sep = x isa CSTParser.EXPR{CSTParser.Braces} ? "" : " "

    e = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x.args)
        ei = flatten(a, s)
        if CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            !(x isa CSTParser.EXPR{CSTParser.Braces}) && (ei = merge(ei, " "))
            push!(e, ei)
            typ = typeof(e[1]).parameters[1]
            t = EditTree{typ}(e)
            push!(edits, t)
            e = Union{Edit, EditTree}[]
        else
            push!(e, ei)
        end
    end
    typ = typeof(e[1]).parameters[1]
    t = EditTree{typ}(e)
    push!(edits, t)

    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Call,CSTParser.Curly}
    edits = Union{Edit, EditTree}[]

    e = Union{Edit, EditTree}[]
    push!(e, flatten(x.args[1], s))
    push!(e, flatten(x.args[2], s))
    typ = typeof(e[1]).parameters[1]
    t = EditTree{typ}(e)
    push!(edits, t)

    e = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x.args[3:end])
        ei = flatten(a, s)
        if CSTParser.is_comma(a) && i < length(x) - 3 && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            x isa CSTParser.EXPR{CSTParser.Call} && (ei = merge(ei, " "))
            push!(e, ei)
            typ = typeof(e[1]).parameters[1]
            t = EditTree{typ}(e)
            push!(edits, t)
            e = Union{Edit, EditTree}[]
        elseif a isa CSTParser.EXPR{CSTParser.Parameters}
            typ = typeof(e[1]).parameters[1]
            t = EditTree{typ}(e)
            t = merge(t, "; ")
            push!(edits, t)
            push!(e, ei)
            e = Union{Edit, EditTree}[]
        else
            push!(e, ei)
        end
    end
    typ = typeof(e[1]).parameters[1]
    t = EditTree{typ}(e)
    push!(edits, t)

    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{CSTParser.Parameters}, s::State)
    edits = Union{Edit, EditTree}[]

    e = Union{Edit, EditTree}[]
    for (i, a) in enumerate(x.args)
        ei = flatten(a, s)
        if CSTParser.is_comma(a)
            ei = merge(ei, " ")
            push!(e, ei)
            typ = typeof(e[1]).parameters[1]
            t = EditTree{typ}(e)
            push!(edits, t)
            e = Union{Edit, EditTree}[]
        else
            push!(e, ei)
        end
    end
    typ = typeof(e[1]).parameters[1]
    t = EditTree{typ}(e)
    push!(edits, t)

    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Using,CSTParser.Export,CSTParser.Import}
    edits = Union{Edit, EditTree}[]
    e = Union{Edit, EditTree}[]

    # Initial edit
    colon = findfirst(CSTParser.is_colon, x.args)
    idx = colon == nothing ? 2 : first(colon) + 1
    i = 1
    while i < idx
        push!(e, flatten(x.args[i], s))
        i += 1
    end
    typ = typeof(e[1]).parameters[1]
    t = EditTree{typ}(e)
    push!(edits, t)

    e = Union{Edit, EditTree}[]
    for a in x.args[idx:end]
        ei = flatten(a, s)
        if CSTParser.is_comma(a)
            ei = merge(ei, " ")
            push!(e, ei)
            typ = typeof(e[1]).parameters[1]
            t = EditTree{typ}(e)
            push!(edits, t)
            e = Union{Edit, EditTree}[]
        else
            push!(e, ei)
        end
    end
    typ = typeof(e[1]).parameters[1]
    t = EditTree{typ}(e)
    push!(edits, t)

    return EditTree{typeof(x)}(edits)
end

function flatten(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Comparison,CSTParser.ChainOpCall,CSTParser.Kw}
    edits = Union{Edit, EditTree}[]

    for (i, a) in enumerate(x)
        e = flatten(a, s)
        if a isa CSTParser.OPERATOR
            e = merge(" ", e)
            e = merge(e, " ")
        elseif CSTParser.is_comma(a) && i != length(x)
            e = merge(e, " ")
        else
        end
        push!(edits, e)
    end

    return EditTree{typeof(x)}(edits)
end


print_tree(io::IOBuffer, t::Edit) = write(io, t.text)
function print_tree(io::IOBuffer, t::EditTree)
    for e in t.edits
        print_tree(io, e)
    end
end


#= function print_tree(io::IOBuffer, t::EditTree{T}, s::State, indent::Indent=nothing) where {T} =#
#=     for (i, j) in zip(1:length(t.edits)-1, 2:length(t.edits)) =#
#=         e1 = t.edits[i] =#
#=         e2 = t.edits[j] =#
#=  =#
#=         indent_block = e1 isa EditTree{CSTParser.EXPR{CSTParser.Block}} && !t.single_line =#
#=         indent_block && (s.indents += 1) =#
#=         print_tree(io, e1, s) =#
#=         indent_block && (s.indents -= 1) =#
#=  =#
#=         #= comments = gather_comments(s, e1.endline+1, e2.startline-1) =# =#
#=         #= write(io, w) =# =#
#=         #= write(io, comments) =# =#
#=     end =#
#=     print_tree(io, t.edits[end], s) =#
#=     return nothing =#
#= end =#
#=  =#
#= function print_tree(io::IOBuffer, t::EditTree{CSTParser.EXPR{CSTParser.Block}}, s::State, indent::Indent=nothing) =#
#=     #= @info "INDENTS", s.indents =# =#
#=     w = repeat(" ", s.indents * s.indent_width) =#
#=     for (i, j) in zip(1:length(t.edits)-1, 2:length(t.edits)) =#
#=         e1 = t.edits[i] =#
#=         e2 = t.edits[j] =#
#=         write(io, w) =#
#=         print_tree(io, e1, s) =#
#=         #= comments = gather_comments(s, e1.endline+1, e2.startline-1) =# =#
#=         #= write(io, comments) =# =#
#=     end =#
#=     print_tree(io, t.edits[end], s) =#
#=     return nothing =#
#= end =#
#=  =#
#=  =#
#= function print_tree(io::IOBuffer, e::Edit, s::State, indent::Indent=nothing) =#
#=     write(io, e.text) =#
#=     return nothing =#
#= end =#

#= function gather_comments(s::State, endline::Int, startline::Int) =#
#=     #= w = repeat(" ", indent == nothing ? s.indents * s.indent_width : indent) =# =#
#=     w = repeat(" ", s.indents * s.indent_width) =#
#=     comments = "" =#
#=     comment_range = endline:startline =#
#=     for (i, l) in enumerate(comment_range) =#
#=         v = s.doc.text[s.doc.ranges[l]] =#
#=  =#
#=         #= @info l, v =# =#
#=  =#
#=         # remove extra newlines =#
#=         if i < length(comment_range) && v == "\n" =#
#=             vn = s.doc.text[s.doc.ranges[l+1]] =#
#=             v == vn && (continue) =#
#=         end =#
#=  =#
#=         v == "\n" && (comments = rstrip(comments, ' ') * v * w; continue) =#
#=  =#
#=         i = first(findfirst(x -> !isspace(x), v)) =#
#=         if v[i] == '#' =#
#=             comments *= v[i:end] * w =#
#=         else =#
#=             # This captures the possible additional indentation in a docstring =#
#=             i = max(min(i, s.indents-1 * s.indent_width), 1) =#
#=             comments *= v[i:end] * w =#
#=         end =#
#=     end =#
#=  =#
#=     #= @info comments =# =#
#=     return comments =#
#= end =#


