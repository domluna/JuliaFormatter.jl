
struct Edit
    startline::Int
    endline::Int
    text::AbstractString
    #= text::IOBuffer =#
end
#= Base.length(e::Edit) = e.text.size =#
Base.length(e::Edit) = length(e.text)


"""
Returns an Edit, a prettified text representation of x
along with the lines containing x in the original file.

`a`

# comments
# ...

`b`
"""
function merge_edits(a::Edit, b::Edit, s::State; join_lines=false, indent::Indent=nothing)

    if (a.startline == b.startline || a.endline == b.endline) && indent == nothing
        return Edit(a.startline, b.endline, a.text * b.text)
    elseif a.text == ""
        return b
    elseif b.text == "" && b.startline != length(s.doc.ranges)
        return a
    end

    # default to current indentation state
    w = repeat(" ", indent == nothing ? s.indents * s.indent_width : indent)

    text = ""

    if a.text[end] != '\n' && !join_lines
        text *= rstrip(a.text, ' ') * "\n" * w
        s.line_offset = length(w)
    elseif a.text[end] == '\n'
        text *= a.text * w
        s.line_offset = length(w)
    else
        text *= a.text
    end

    # TODO: try moving comments prior to the initial
    # text if the lines are being joined
    #
    # comments shouldn't be in between joinable lines anyway :(
    
    comment_text = ""
    #= if !join_lines =#
    #=     comment_range = a.endline+1:b.startline-1 =#
    #=     for (i, l) in enumerate(comment_range) =#
    #=         v = s.doc.text[s.doc.ranges[l]] =#
    #=  =#
    #=         @info l, v =#
    #=  =#
    #=         # remove extra newlines =#
    #=         if i < length(comment_range) && v == "\n" =#
    #=             vn = s.doc.text[s.doc.ranges[l+1]] =#
    #=             v == vn && (continue) =#
    #=         end =#
    #=  =#
    #=         v == "\n" && (comment_text = rstrip(comment_text, ' ') * v * w; continue) =#
    #=  =#
    #=         i = first(findfirst(x -> !isspace(x), v)) =#
    #=         if v[i] == '#' =#
    #=             comment_text *= v[i:end] * w =#
    #=         else =#
    #=             # This captures the possible additional indentation in a docstring =#
    #=             i = max(min(i, s.indents-1 * s.indent_width), 1) =#
    #=             comment_text *= v[i:end] * w =#
    #=         end =#
    #=     end =#
    #= end =#
    text *= comment_text * b.text
    Edit(a.startline, b.endline, text)
end

function merge_edits(a::AbstractString, b::AbstractString, s::State; kwargs...)
    l, _, _ = cursor_loc(s)
    merge_edits(Edit(l, l, a), Edit(l, l, b), s; kwargs...)
end

Base.:*(a::Edit, b::AbstractString) = Edit(a.startline, a.endline, a.text * b)
Base.:*(a::AbstractString, b::Edit) = Edit(b.startline, b.endline, a * b.text)
merge_edits(a::Edit, b::AbstractString, s::State; kwargs...) = merge_edits(a, Edit(a.startline, a.endline, b), s; kwargs...)
merge_edits(a::AbstractString, b::Edit, s::State; kwargs...) = merge_edits(Edit(b.startline, b.endline, a), b, s; kwargs...)

# Determines whether the Edit `e` should be nested.
function should_nest(e::Edit, line_offset::Int, indent::Int, max_width::Int)
    #= line_offset + length(e) > max_width && indent + length(e) < max_width =#
    line_offset + length(e) > max_width
end

function nestable(x::T) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    #= CSTParser.precedence(x.op) in (4, 5, 7, 9, 11) && (return true) =#
    #= CSTParser.precedence(x.op) in (4, 5, 7, 9, 11) && (return true) =#
    x.op.kind == Tokens.EQ && CSTParser.defines_function(x) && (return true)
    x.op.kind == Tokens.LAZY_OR && (return true)
    x.op.kind == Tokens.LAZY_AND && (return true)
    x.op.kind == Tokens.OR && (return true)
    x.op.kind == Tokens.AND && (return true)
    false
    #= x.op.kind == Tokens.ANON_FUNC && (return false) =#
    #= CSTParser.precedence(x.op) == 6 && (return false) =#
    #= CSTParser.issyntaxcall(x.op) && (return false) =#
    #= true =#
end

function pretty(x::CSTParser.AbstractEXPR, s::State)
    e = ""
    for a in x
        ei = pretty(a, s)
        (ei.text == "") && (continue)
        e = merge_edits(e, ei, s, join_lines=true)
    end
    e
end

function pretty(x::Vector, s::State)
    @info "Vector" s.line_offset length(x) x
    e = ""
    for a in x
        ei = pretty(a, s)
        (ei.text == "") && (continue)
        e = merge_edits(e, ei, s, join_lines=true)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.FileH}, s::State)
    e = ""
    for a in x
        e = merge_edits(e, pretty(a, s), s)
        s.line_offset = s.indents * s.indent_width
    end
    e
end

function pretty(x::CSTParser.IDENTIFIER, s::State)
    loc = cursor_loc(s)
    s.offset += x.fullspan
    s.line_offset += length(x.val)
    Edit(loc[1], loc[1], x.val)
end

function pretty(x::CSTParser.OPERATOR, s::State)
    loc = cursor_loc(s)
    text = string(CSTParser.Expr(x))
    s.offset += x.fullspan
    s.line_offset += length(text)
    Edit(loc[1], loc[1], text)
end

function pretty(x::CSTParser.KEYWORD, s::State)
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
    s.line_offset += length(text)
    Edit(loc[1], loc[1], text)
end

function pretty(x::CSTParser.PUNCTUATION, s::State)
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
    s.line_offset += length(text)
    Edit(loc[1], loc[1], text)
end

# TODO: don't escape newlines in TRIPLE_STRING
# this needs a change in CSTParser
function pretty(x::CSTParser.LITERAL, s::State; surround_with_quotes=true)
    loc = cursor_loc(s)
    text = x.kind == Tokens.TRIPLE_STRING && surround_with_quotes ? "\"\"\"" * escape_string(x.val, "\$") * "\"\"\"" :
           x.kind == Tokens.STRING && surround_with_quotes ? "\"" * escape_string(x.val, "\$") * "\"" :
           x.val
    s.offset += x.fullspan
    s.line_offset += length(text)
    Edit(loc[1], loc[1], text)
end

function pretty(x::CSTParser.EXPR{CSTParser.StringH}, s::State)
    e = ""
    for a in x
        if a isa CSTParser.LITERAL
            ei = pretty(a, s, surround_with_quotes=false)
            a.val == "" && (continue)
            e = merge_edits(e, ei, s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    Edit(e.startline, e.endline, "\"" * e.text * "\"")
end

function pretty(x::CSTParser.EXPR{CSTParser.MacroCall}, s::State)
    # Docstring
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        pretty(x.args[1], s)

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

        e = Edit(loc1[1], loc1[1], tq)

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
            e = merge_edits(e, Edit(loc1[1], loc1[1], "\n" * w * v), s)
        end

        offset = s.offset
        if loc1[1] == loc2[1]
            e = merge_edits(e, Edit(loc2[1]+1, loc2[1]+1, tq), s)
        elseif loc2[3] > quote_len + 1
            v = strip(is_ts ? s.doc.text[offset-loc2[2]:offset-5] : s.doc.text[offset-loc2[2]:offset-3])
            #= @info "H3", v =#
            if v  != ""
                e = merge_edits(e, Edit(loc2[1], loc2[1], v * "\n" * w * tq), s)
            else
                e = merge_edits(e, Edit(loc2[1], loc2[1], tq), s)
            end
        else
            e = merge_edits(e, Edit(loc2[1], loc2[1], tq), s)
        end

        indent = s.indents * s.indent_width
        return merge_edits(e, pretty(x.args[3], s), s, indent=indent)
    end

    # same as CSTParser.EXPR{CSTParser.CALL} but whitespace sensitive
    e = ""
    for (i, a) in enumerate(x)
        ei = pretty(a, s)
        @info "MACROCALL" a

        # i == 1 is probably redundant
        if i == 1 && a isa CSTParser.EXPR{CSTParser.MacroName}
            if a.fullspan - a.span > 0
                e = merge_edits(e, ei * " ", s, join_lines=true)
                s.line_offset += 1
            else
                e = merge_edits(e, ei, s, join_lines=true)
                # assumes the next argument is a brace of some sort
            end
        #= elseif a isa CSTParser.KEYWORD =#
        #=     s.line_offset += 1 =#
        #=     e = merge_edits(e, " " * ei, s; join_lines=true) =#
        elseif a.fullspan - a.span > 0
            e = merge_edits(e, ei * " ", s, join_lines=true)
            s.line_offset += 1
        elseif CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            e = merge_edits(e, ei * " ", s, join_lines=true)
            s.line_offset += 1
        else
            e = merge_edits(e, ei, s, join_lines=true)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Block}, s::State; ignore_single_line=false)
    single_line = ignore_single_line ? false : cursor_loc(s)[1] == cursor_loc(s, s.offset+x.span-1)[1] 
    #= @info "" single_line =#
    e = ""
    indent = s.indents * s.indent_width

    for (i, a) in enumerate(x)
        # Reset line offset unless the block is part of a single line
        #= !single_line && (s.line_offset = line_offset) =#
        !single_line && (s.line_offset = s.indents * s.indent_width)

        ei = pretty(a, s)
        if i < length(x) && CSTParser.is_comma(a) && x.args[i+1] isa CSTParser.PUNCTUATION
            e = merge_edits(e, ei, s)
        elseif CSTParser.is_comma(a) && i != length(x)
            e = merge_edits(e, ei * " ", s)
            s.line_offset += 1
        elseif single_line
            if i == 1 ||CSTParser.is_comma(x.args[i-1])
                e = merge_edits(e, ei, s)
            else
                s.line_offset += 2
                e = merge_edits(e, "; " * ei, s)
            end
        else
            #= @info "BLOCK LINE OFFSET" line_offset e ei =#
            e = merge_edits(e, ei, s, indent=indent)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Abstract}, s::State)
    e = pretty(x.args[1], s)
    e = merge_edits(e, pretty(x.args[2], s), s, join_lines=true)
    e = merge_edits(e, pretty(x.args[3], s), s, join_lines=true)
    s.line_offset += 1
    e = merge_edits(e, " " * pretty(x.args[4], s), s, join_lines=true)
end

function pretty(x::CSTParser.EXPR{CSTParser.FunctionDef}, s::State)
    e = pretty(x.args[1], s)
    e = merge_edits(e, pretty(x.args[2], s), s, join_lines=true)
    if length(x) > 3
        s.indents += 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[3], s, ignore_single_line=true), s, indent=indent)
        s.indents -= 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[4], s), s, indent=indent)
    else
        # function stub, i.e. "function foo end"
        # this should be on one line
        s.line_offset += 1
        e = merge_edits(e, " " * pretty(x.args[3], s), s, join_lines=true)
    end
    e
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Macro,CSTParser.Struct}
    e = pretty(x.args[1], s)
    e = merge_edits(e, pretty(x.args[2], s), s, join_lines=true)
    s.indents += 1
    indent = s.indents * s.indent_width
    e = merge_edits(e, pretty(x.args[3], s, ignore_single_line=true), s, indent=indent)
    s.indents -= 1
    indent = s.indents * s.indent_width
    merge_edits(e, pretty(x.args[4], s), s, indent=indent)
end

function pretty(x::CSTParser.EXPR{CSTParser.Mutable}, s::State)
    e = merge_edits(pretty(x.args[1], s), pretty(x.args[2], s), s, join_lines=true)
    e = merge_edits(e, pretty(x.args[3], s), s, join_lines=true)
    s.indents += 1
    indent = s.indents * s.indent_width
    e = merge_edits(e, pretty(x.args[4], s, ignore_single_line=true), s, indent=indent)
    s.indents -= 1
    indent = s.indents * s.indent_width
    merge_edits(e, pretty(x.args[5], s), s, indent=indent)
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.For,CSTParser.While}
    e = pretty(x.args[1], s)
    if x.args[2] isa CSTParser.EXPR{CSTParser.Block}
        e = merge_edits(e, pretty(x.args[2], s), s, join_lines=true)
    else
        e = merge_edits(e, pretty(x.args[2], s), s, join_lines=true)
    end
    s.indents += 1
    indent = s.indents * s.indent_width
    e = merge_edits(e, pretty(x.args[3], s, ignore_single_line=true), s, indent=indent)
    s.indents -= 1
    indent = s.indents * s.indent_width
    merge_edits(e, pretty(x.args[4], s), s, indent=indent)
end

function pretty(x::CSTParser.EXPR{CSTParser.Do}, s::State)
    e = pretty(x.args[1], s)
    e = merge_edits(e, pretty(x.args[2], s), s, join_lines=true)
    e = merge_edits(e, pretty(x.args[3], s), s, join_lines=true)
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        s.indents += 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[4], s, ignore_single_line=true), s, indent=indent)
        s.indents -= 1
        indent = s.indents * s.indent_width
    end
    merge_edits(e, pretty(x.args[end], s), s, indent=indent)
end

function pretty(x::CSTParser.EXPR{CSTParser.Try}, s::State)
    e = pretty(x.args[1], s)
    s.indents += 1
    indent = s.indents * s.indent_width
    e = merge_edits(e, pretty(x.args[2], s, ignore_single_line=true), s, indent=indent)
    s.indents -= 1
    indent = s.indents * s.indent_width
    e = merge_edits(e, pretty(x.args[3], s), s, indent=indent)

    if x.args[4].fullspan != 0
        s.line_offset += 1
        e = merge_edits(e, " " * pretty(x.args[4], s), s, join_lines=true)
    end

    s.indents += 1
    indent = s.indents * s.indent_width
    e = merge_edits(e, pretty(x.args[5], s, ignore_single_line=true), s, indent=indent)
    s.indents -= 1
    indent = s.indents * s.indent_width
    e = merge_edits(e, pretty(x.args[6], s), s, indent=indent)

    if length(x.args) > 6
        s.indents += 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[7], s, ignore_single_line=true), s, indent=indent)
        s.indents -= 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[8], s), s, indent=indent)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.ModuleH}, s::State)
    e = merge_edits(pretty(x.args[1], s), pretty(x.args[2], s), s, join_lines=true)
    e = merge_edits(e, pretty(x.args[3], s), s)
    indent = s.indents * s.indent_width
    merge_edits(e, pretty(x.args[4], s), s, indent=indent)
end

function pretty(x::CSTParser.EXPR{CSTParser.Return}, s::State)
    e = pretty(x.args[1], s)
    if x.args[2].fullspan != 0
        e *= " "
        s.line_offset += 1
        for a in x.args[2:end]
            e = merge_edits(e, pretty(a, s), s, join_lines=true)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Begin}, s::State)
    e = pretty(x.args[1], s)
    s.indents += 1
    indent = s.indents * s.indent_width
    e = merge_edits(e, pretty(x.args[2], s, ignore_single_line=true), s, indent=indent)
    s.indents -= 1
    indent = s.indents * s.indent_width
    merge_edits(e, pretty(x.args[3], s), s, indent=indent)
end

function pretty(x::CSTParser.EXPR{CSTParser.Quote}, s::State)
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        e = pretty(x.args[1], s)
        s.indents += 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[2], s, ignore_single_line=true), s, indent=indent)
        s.indents -= 1
        indent = s.indents * s.indent_width
        return merge_edits(e, pretty(x.args[3], s), s; indent=indent)
    end
    pretty(x.args, s, indent)
end

function pretty(x::CSTParser.EXPR{CSTParser.Let}, s::State)
    e = pretty(x.args[1], s)
    if length(x.args) > 3
        if x.args[2] isa CSTParser.EXPR{CSTParser.Block}
            e = merge_edits(e, pretty(x.args[2], s), s, join_lines=true)
        else
            e = merge_edits(e, pretty(x.args[2], s), s, join_lines=true)
        end
        s.indents += 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[3], s, ignore_single_line=true), s, indent=indent)
        s.indents -= 1
        indent = s.indents * s.indent_width
    else
        # TODO: revisit, the indentation is tricky here
        s.indents += 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[2], s, ignore_single_line=true), s, indent=indent)
        s.indents -= 1
        indent = s.indents * s.indent_width
    end
    e = merge_edits(e, pretty(x.args[end], s), s, indent=indent)
end

function pretty(x::CSTParser.EXPR{CSTParser.If}, s::State)
    e = pretty(x.args[1], s)
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.IF
        e = merge_edits(e, pretty(x.args[2], s), s; join_lines=true)
        s.indents += 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[3], s, ignore_single_line=true), s, indent=indent)
        s.indents -= 1
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[4], s), s, indent=indent)
        if length(x.args) > 4
            s.indents += 1
            indent = s.indents * s.indent_width
            if x.args[4].kind == Tokens.ELSEIF
                e = merge_edits(e, pretty(x.args[5], s), s, join_lines=true)
            else
                e = merge_edits(e, pretty(x.args[5], s, ignore_single_line=true), s, indent=indent)
            end
            s.indents -= 1
            indent = s.indents * s.indent_width
            # END KEYWORD
            e = merge_edits(e, pretty(x.args[6], s), s, indent=indent)
        end
    else
        indent = s.indents * s.indent_width
        e = merge_edits(e, pretty(x.args[2], s, ignore_single_line=true), s, indent=indent)
        if length(x.args) > 2
            s.indents -= 1
            indent = s.indents * s.indent_width

            e = merge_edits(e, pretty(x.args[3], s), s, indent=indent)

            s.indents += 1
            indent = s.indents * s.indent_width

            # this either else or elseif
            if x.args[3].kind == Tokens.ELSEIF
                e = merge_edits(e, pretty(x.args[4], s), s, join_lines=true)
            else
                e = merge_edits(e, pretty(x.args[4], s, ignore_single_line=true), s, indent=indent)
            end
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Comparison,CSTParser.ChainOpCall,CSTParser.Kw}
    e = ""
    for (i, a) in enumerate(x)
        ei = pretty(a, s)
        if a isa CSTParser.OPERATOR
            s.line_offset += 1
            e = merge_edits(e, " " * ei * " ", s; join_lines=true)
            s.line_offset += 1
        elseif i == length(x) - 1 && a isa CSTParser.PUNCTUATION && x.args[i+1] isa CSTParser.PUNCTUATION
            e = merge_edits(e, ei, s; join_lines=true)
        elseif a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA && i != length(x)
            e = merge_edits(e, ei * " ", s; join_lines=true)
            s.line_offset += 1
        else
            e = merge_edits(e, ei, s; join_lines=true)
        end
    end
    e
end

function pretty(x::T, s::State) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    e = pretty(x.arg1, s)
    if CSTParser.precedence(x.op) in (8, 13, 14, 16) && x.op.kind != Tokens.ANON_FUNC
        e = merge_edits(e, pretty(x.op, s), s; join_lines=true)
    elseif x.op.kind == Tokens.EX_OR
        s.line_offset += 1
        e = merge_edits(e, " " * pretty(x.op, s), s, join_lines=true)
    else
        s.line_offset += 1
        e = merge_edits(e, " " * pretty(x.op, s) * " ", s, join_lines=true)
        s.line_offset += 1
    end
    merge_edits(e, pretty(x.arg2, s), s, join_lines=true)
end

# A where B
# should format B prior to A
#
# line_offset_A = ...
# line_offset_B = ...
# indent_A = ...
# indent_B = ...
#
# line_offset
function pretty(x::CSTParser.WhereOpCall, s::State)
    e = pretty(x.arg1, s)
    s.line_offset += 1
    e = merge_edits(e, " " * pretty(x.op, s) * " ", s; join_lines=true)
    s.line_offset += 1

    edits = Edit[]
    e2 = ""
    for (i, a) in enumerate(x.args)
        if CSTParser.is_comma(a)
            e2 = merge_edits(e2, pretty(a, s, indent), s; join_lines=true)
            push!(edits, e2)
            e2 = ""
        else
            e2 = merge_edits(e2, pretty(a, s, indent), s; join_lines=true)
        end
    end
    push!(edits, e2)
    merge_edits(e, e2, s)
end

# C ? E1 : E2
#
# if the above is > s.max_width
# format to
#
# C ? E1 :
# E2
#
# still doesn't fit?
#
# C ?
# E1 :
# E2
#
# C1 ? E1 : C2 ? E2 : C3 ? E3 : C4 ? E4 : E5
#
# [C1 ?, E1 :, C2 ? E2 : C3 ? E3 : C4 ? E4 : E5]
# [C2 ?, E2 :, C3 ? E3 : C4 ? E4 : E5]
# [C3 ?, E3 :, C4 ? E4 : E5]
# [C4 ?, E4 :, E5]
function pretty(x::CSTParser.ConditionalOpCall, s::State)
    edits = Edit[]
    e = pretty(x.cond, s)

    s.line_offset += 1
    e = merge_edits(e, " " * pretty(x.op1, s) * " ", s, join_lines=true)
    s.line_offset += 1
    push!(edits, e)

    e = pretty(x.arg1, s)
    s.line_offset += 1
    e = merge_edits(e, " " * pretty(x.op2, s) * " ", s, join_lines=true)
    s.line_offset += 1
    push!(edits, e)
    push!(edits, pretty(x.arg2, s))

    e = ""
    for (i, ei) in enumerate(edits)
        e = merge_edits(e, ei, s, join_lines=true)
    end
    e
end

function pretty(x::CSTParser.UnarySyntaxOpCall, s::State)
    @info "" typeof(x) s.line_offset 
    e = pretty(x.arg1, s)
    merge_edits(e, pretty(x.arg2, s), s, join_lines=true)
end

function pretty(x::CSTParser.UnaryOpCall, s::State)
    @info "" typeof(x) s.line_offset 
    e = pretty(x.op, s)
    merge_edits(e, pretty(x.arg, s), s, join_lines=true)
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Curly,CSTParser.Call}
    e = pretty(x.args[1], s)
    edits = Edit[merge_edits(e, pretty(x.args[2], s), s; join_lines=true)]
    sep = x isa CSTParser.EXPR{CSTParser.Call} ? " " : ""
    e = ""
    for (i, a) in enumerate(x.args[3:end])
        if CSTParser.is_comma(a) && i < length(x) - 3 && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            e = merge_edits(e, pretty(a, s) * sep, s; join_lines=true)
            s.line_offset += length(sep)
            push!(edits, e)
            e = ""
        elseif a isa CSTParser.EXPR{CSTParser.Parameters}
            push!(edits, e * "; ")
            s.line_offset += 2
            e = ""
            e = merge_edits(e, pretty(a, s), s; join_lines=true)
        else
            e = merge_edits(e, pretty(a, s), s; join_lines=true)
        end
    end
    push!(edits, e)
    #= edits =#
    e = ""
    for ei in edits
        e = merge_edits(e, ei, s, join_lines=true)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Parameters}, s::State)
    edits = Edit[]
    e = ""
    for (i, a) in enumerate(x)
        if CSTParser.is_comma(a)
            e = merge_edits(e, pretty(a, s) * " ", s; join_lines=true)
            s.line_offset += 1
            push!(edits, e)
            e = ""
        else
            e = merge_edits(e,  pretty(a, s), s; join_lines=true)
        end
    end
    push!(edits, e)
    #= edits =#
    e = ""
    for ei in edits
        e = merge_edits(e, ei, s, join_lines=true)
    end
    e
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.Vect,CSTParser.InvisBrackets,CSTParser.Braces}
    sep = x isa CSTParser.EXPR{CSTParser.Braces} ? "" : " "
    e = ""
    edits = Edit[]
    for (i, a) in enumerate(x)
        if CSTParser.is_comma(a) && i < length(x) && !(x.args[i+1] isa CSTParser.PUNCTUATION)
            e = merge_edits(e, pretty(a, s) * sep, s; join_lines=true)
            s.line_offset += length(sep)
            push!(edits, e)
            e = ""
        else
            e = merge_edits(e, pretty(a, s), s; join_lines=true)
        end
    end
    push!(edits, e)
    #= edits =#
    e = ""
    for ei in edits
        e = merge_edits(e, ei, s, join_lines=true)
    end
    e
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Export,CSTParser.Import,CSTParser.Using}
    edits = Edit[pretty(x.args[1], s)]
    sidx = 2
    e = ""
    for (i, a) in enumerate(x.args[2:end])
        if CSTParser.is_comma(a) || CSTParser.is_colon(a)
            CSTParser.is_colon(a) && (sidx = 3)
            e = merge_edits(e, pretty(a, s) * " ", s; join_lines=true)
            s.line_offset += 1
            push!(edits, e)
            e = ""
        else
            e = merge_edits(e, pretty(a, s), s; join_lines=true)
        end
    end
    push!(edits, e)
    #= edits =#
    e = ""
    for ei in edits
        e = merge_edits(e, ei, s, join_lines=true)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Vcat}, s::State)
    e = ""
    for (i, a) in enumerate(x)
        if i > 1 && i < length(x) - 1
            e = merge_edits(e, pretty(a, s) * "; ", s, join_lines=true)
            s.line_offset += 2
        else
            e = merge_edits(e, pretty(a, s), s, join_lines=true)
        end
    end
    e
end


function pretty(x::CSTParser.EXPR{CSTParser.TypedVcat}, s::State)
    e = ""
    for (i, a) in enumerate(x)
        if i > 2 && i < length(x) - 1
            e = merge_edits(e, pretty(a, s) * "; ", s, join_lines=true)
            s.line_offset += 2
        else
            e = merge_edits(e, pretty(a, s), s, join_lines=true)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Hcat}, s::State)
    e = ""
    for (i, a) in enumerate(x)
        if i > 1 && i < length(x) - 1
            e = merge_edits(e, pretty(a, s) * " ", s, join_lines=true)
            s.line_offset += 1
        else
            e = merge_edits(e, pretty(a, s), s, join_lines=true)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.TypedHcat}, s::State)
    e = ""
    for (i, a) in enumerate(x)
        if i > 2 && i < length(x) - 1
            e = merge_edits(e, pretty(a, s) * " ", s, join_lines=true)
            s.line_offset += 1
        else
            e = merge_edits(e, pretty(a, s), s, join_lines=true)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Row}, s::State)
    e = ""
    for (i, a) in enumerate(x)
        if i < length(x)
            e = merge_edits(e, pretty(a, s) * " ", s, join_lines=true)
            s.line_offset += 1
        else
            e = merge_edits(e, pretty(a, s), s, join_lines=true)
        end
    end
    e
end

# Expr KEYWORD Expr
function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Generator,CSTParser.Filter}
    e = ""
    for (i, a) in enumerate(x)
        if a isa CSTParser.KEYWORD
            s.line_offset += 1
            e = merge_edits(e, " " * pretty(a, s), s, join_lines=true)
        else
            e = merge_edits(e, pretty(a, s), s, join_lines=true)
        end
    end
    e
end
