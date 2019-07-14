# Creates a _prettified_ version of a CST.

@enum(PLeaf, NEWLINE, SEMICOLON, WHITESPACE, PLACEHOLDER, NOTCODE, INLINECOMMENT)

mutable struct PTree
    typ::Union{CSTParser.Head, PLeaf}
    startline::Int
    endline::Int
    indent::Int
    len::Int
    val::Union{Nothing, AbstractString}
    nodes::Union{Nothing, Vector{PTree}}
    ref::Union{Nothing,Ref{CSTParser.EXPR}}
end

PTree(x::CSTParser.EXPR, indent::Int) = PTree(x.typ, -1, -1, indent, 0, nothing, PTree[], Ref(x))

function PTree(x::CSTParser.EXPR, startline::Int, endline::Int, val::AbstractString)
    PTree(x.typ, startline, endline, 0, length(val), val, nothing, Ref(x))
end

Newline() = PTree(NEWLINE, -1, -1, 0, 0, "\n", nothing, nothing)
Semicolon() = PTree(SEMICOLON, -1, -1, 0, 1, ";", nothing, nothing)
Whitespace(n) = PTree(WHITESPACE, -1, -1, 0, n, " "^n, nothing, nothing)
Placeholder(n) = PTree(PLACEHOLDER, -1, -1, 0, n, " "^n, nothing, nothing)
Notcode(startline, endline, indent) = PTree(NOTCODE, startline, endline, indent, 0, "", nothing, nothing)
InlineComment(line) = PTree(INLINECOMMENT, line, line, 0, 0, "", nothing, nothing)

Base.length(x::PTree) = x.len

is_leaf(x::PTree) = x.nodes === nothing
empty_start(x::PTree) = x.startline == 1 && x.endline == 1 && x.val == ""

is_punc(x) = CSTParser.ispunctuation(x)

function add_node!(t::PTree, n; join_lines=false)
    if n.typ isa PLeaf
        push!(t.nodes, n)
        t.len += length(n)
        # Don't want to alter the startline/endline of these types
        if n.typ !== NOTCODE && n.typ !== INLINECOMMENT
            n.startline = t.startline
            n.endline = t.endline
        end
        return
    end

    if n.typ === CSTParser.Block && length(n) == 0 
        return
    elseif n.typ === CSTParser.Parameters
        add_node!(t, Semicolon())
        add_node!(t, Placeholder(1))
    end

    if length(t.nodes) == 0
        t.startline = n.startline
        t.endline = n.endline
        t.len += length(n)
        push!(t.nodes, n)
        return
    end

    if !is_prev_newline(t.nodes[end]) && !join_lines
        current_line = t.nodes[end].endline
        notcode_startline = current_line + 1 
        notcode_endline = n.startline - 1
        
        add_node!(t, InlineComment(current_line))

        if notcode_startline <= notcode_endline && n.typ !== CSTParser.LITERAL
            add_node!(t, Newline())
            if !is_leaf(n)
                add_node!(t, Notcode(notcode_startline, notcode_endline, n.indent))
            else
                add_node!(t, Notcode(notcode_startline, notcode_endline, t.indent))
            end
        end

        add_node!(t, Newline())
    end

    if n.startline < t.startline || t.startline == -1 
        t.startline = n.startline
    end
    if n.endline > t.endline || t.endline == -1 
        t.endline = n.endline
    end
    if t.typ === CSTParser.StringH
        # The length of this node is the length of
        # the longest string
        t.len = max(t.len, length(n))
    else
        t.len += length(n)
    end
    push!(t.nodes, n)
    nothing
end

function is_prev_newline(x::PTree) 
    if x.typ === NEWLINE
        return true
    elseif is_leaf(x) || length(x.nodes) == 0
        return false
    end
    is_prev_newline(x.nodes[end])
end

is_closer(x::PTree) = x.val == "}" || x.val == ")" || x.val == "]"
is_closer(x::CSTParser.EXPR) = x.kind === Tokens.RBRACE || x.kind === Tokens.RPAREN || x.kind === Tokens.RSQUARE 

is_opener(x::PTree) = x.val == "{" || x.val == "(" || x.val == "["
is_opener(x::CSTParser.EXPR) = x.kind === Tokens.LBRACE || x.kind === Tokens.LPAREN || x.kind === Tokens.LSQUARE 

function pretty(x::CSTParser.EXPR, s::State)
    # @info "" x.typ x.args
    if x.typ === CSTParser.IDENTIFIER
        return p_identifier(x, s)
    elseif x.typ === CSTParser.OPERATOR
        return p_operator(x, s)
    elseif x.typ === CSTParser.PUNCTUATION
        return p_punctuation(x, s)
    elseif x.typ === CSTParser.KEYWORD
        return p_keyword(x, s)
    elseif x.typ === CSTParser.LITERAL
        return p_literal(x, s)
    elseif x.typ === CSTParser.StringH
        return p_stringh(x, s)
    elseif x.typ === CSTParser.Block
        return p_block(x, s)
    elseif x.typ === CSTParser.ModuleH
        return p_module(x, s)
    elseif x.typ === CSTParser.BareModule
        return p_module(x, s)
    elseif x.typ === CSTParser.FunctionDef
        return p_function(x, s)
    elseif x.typ === CSTParser.Macro
        return p_function(x, s)
    elseif x.typ === CSTParser.Primitive
        return p_primitive(x, s)
    elseif x.typ === CSTParser.Struct
        return p_struct(x, s)
    elseif x.typ === CSTParser.Mutable
        return p_mutable(x, s)
    elseif x.typ === CSTParser.Abstract
        return p_abstract(x, s)
    elseif x.typ === CSTParser.Primitive
        return p_primitive(x, s)
    elseif x.typ === CSTParser.For
        return p_loop(x, s)
    elseif x.typ === CSTParser.While
        return p_loop(x, s)
    elseif x.typ === CSTParser.Do
        return p_do(x, s)
    elseif x.typ === CSTParser.If
        return p_if(x, s)
    elseif x.typ === CSTParser.Try
        return p_try(x, s)
    elseif x.typ === CSTParser.Begin
        return p_begin(x, s)
    elseif x.typ === CSTParser.Quote
        return p_quote(x, s)
    elseif x.typ === CSTParser.Let
        return p_let(x, s)
    elseif x.typ === CSTParser.Vect
        return p_vect(x, s)
    elseif x.typ === CSTParser.Braces
        return p_braces(x, s)
    elseif x.typ === CSTParser.TupleH
        return p_tuple(x, s)
    elseif x.typ === CSTParser.InvisBrackets
        return p_invisbrackets(x, s)
    elseif x.typ === CSTParser.Curly
        return p_curly(x, s)
    elseif x.typ === CSTParser.Call
        return p_call(x, s)
    elseif x.typ === CSTParser.MacroCall
        return p_macrocall(x, s)
    elseif x.typ === CSTParser.WhereOpCall
        return p_wherecall(x, s)
    elseif x.typ === CSTParser.ConditionalOpCall
        return p_condcall(x, s)
    elseif x.typ === CSTParser.BinaryOpCall
        return p_binarycall(x, s)
    elseif x.typ === CSTParser.UnaryOpCall
        return p_unarycall(x, s)
    elseif x.typ === CSTParser.ChainOpCall
        return p_chaincall(x, s)
    elseif x.typ === CSTParser.Comparison
        return p_chaincall(x, s)
    elseif x.typ === CSTParser.Kw
        return p_kw(x, s)
    elseif x.typ === CSTParser.Parameters
        return p_params(x, s)
    elseif x.typ === CSTParser.Local
        return p_vardef(x, s)
    elseif x.typ === CSTParser.Global
        return p_vardef(x, s)
    elseif x.typ === CSTParser.Const
        return p_vardef(x, s)
    elseif x.typ === CSTParser.Return
        return p_vardef(x, s)
    elseif x.typ === CSTParser.Import
        return p_import(x, s)
    elseif x.typ === CSTParser.ImportAll
        return p_import(x, s)
    elseif x.typ === CSTParser.Export
        return p_import(x, s)
    elseif x.typ === CSTParser.Using
        return p_import(x, s)
    elseif x.typ === CSTParser.Row
        return p_row(x, s)
    elseif x.typ === CSTParser.Vcat
        return p_vcat(x, s)
    elseif x.typ === CSTParser.TypedVcat
        return p_vcat(x, s)
    elseif x.typ === CSTParser.Hcat
        return p_hcat(x, s)
    elseif x.typ === CSTParser.TypedHcat
        return p_hcat(x, s)
    elseif x.typ === CSTParser.Ref
        return p_ref(x, s)
    elseif x.typ === CSTParser.Comprehension
        return p_comprehension(x, s)
    elseif x.typ === CSTParser.Generator
        return p_comprehension(x, s)
    elseif x.typ === CSTParser.Filter
        return p_comprehension(x, s)
    end

    t = PTree(x, nspaces(s))
    join_lines = x.typ !== CSTParser.FileH
    for a in x
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        # @info "" a a.typ
        add_node!(t, pretty(a, s), join_lines=join_lines)
    end
    t
end

function p_identifier(x, s)
    loc = cursor_loc(s)
    s.offset += x.fullspan
    PTree(x, loc[1], loc[1], x.val)
end

function p_operator(x, s)
    loc = cursor_loc(s)
    val = string(CSTParser.Expr(x))
    s.offset += x.fullspan
    PTree(x, loc[1], loc[1], val)
end

function p_keyword(x, s)
    loc = cursor_loc(s)
    val = x.kind === Tokens.ABSTRACT ? "abstract" :
          x.kind === Tokens.BAREMODULE ? "baremodule" :
          x.kind === Tokens.BEGIN ? "begin" :
          x.kind === Tokens.BREAK ? "break" :
          x.kind === Tokens.CATCH ? "catch" :
          x.kind === Tokens.CONST ? "const" :
          x.kind === Tokens.CONTINUE ? "continue" :
          x.kind === Tokens.NEW ? "new" :
          x.kind === Tokens.DO ? "do" :
          x.kind === Tokens.IF ? "if" :
          x.kind === Tokens.ELSEIF ? "elseif" :
          x.kind === Tokens.ELSE ? "else" :
          x.kind === Tokens.END ? "end" :
          x.kind === Tokens.EXPORT ? "export" :
          x.kind === Tokens.FINALLY ? "finally" :
          x.kind === Tokens.FOR ? "for" :
          x.kind === Tokens.FUNCTION ? "function" :
          x.kind === Tokens.GLOBAL ? "global" :
          x.kind === Tokens.IMPORT ? "import" :
          x.kind === Tokens.IMPORTALL ? "importall" :
          x.kind === Tokens.LET ? "let" :
          x.kind === Tokens.LOCAL ? "local" :
          x.kind === Tokens.MACRO ? "macro" :
          x.kind === Tokens.MODULE ? "module" :
          x.kind === Tokens.MUTABLE ? "mutable" :
          x.kind === Tokens.OUTER ? "outer " :
          x.kind === Tokens.PRIMITIVE ? "primitive" :
          x.kind === Tokens.QUOTE ? "quote" :
          x.kind === Tokens.RETURN ? "return" :
          x.kind === Tokens.STRUCT ? "struct" :
          x.kind === Tokens.TYPE ? "type" :
          x.kind === Tokens.TRY ? "try" :
          x.kind === Tokens.USING ? "using" :
          x.kind === Tokens.WHILE ? "while" : ""
    s.offset += x.fullspan
    PTree(x, loc[1], loc[1], val)
end

function p_punctuation(x, s)
    loc = cursor_loc(s)
    val = x.kind === Tokens.LPAREN ? "(" :
        x.kind === Tokens.LBRACE ? "{" :
        x.kind === Tokens.LSQUARE ? "[" :
        x.kind === Tokens.RPAREN ? ")" :
        x.kind === Tokens.RBRACE ? "}" :
        x.kind === Tokens.RSQUARE ? "]" :
        x.kind === Tokens.COMMA ? "," :
        x.kind === Tokens.SEMICOLON ? ";" :
        x.kind === Tokens.AT_SIGN ? "@" :
        x.kind === Tokens.DOT ? "." : ""
    s.offset += x.fullspan
    PTree(x, loc[1], loc[1], val)
end

function p_literal(x, s; include_quotes=true)
    loc = cursor_loc(s)
    if !is_str_or_cmd(x.kind)
        s.offset += x.fullspan
        return PTree(x, loc[1], loc[1], x.val)
    end
    
    # At the moment CSTParser does not return Tokens.TRIPLE_STRING
    # tokens :(
    # https://github.com/ZacLN/CSTParser.jl/issues/88
    #
    # Also strings are unescaped to by CSTParser
    # to mimic Meta.parse which makes finding newlines
    # for indentation problematic.
    #
    # So we'll just look at the source directly!
    startline, endline, str = s.doc.lit_strings[s.offset-1]
    # @info "" loc startline endline str

    # Since a line of a multiline string can already
    # have it's own indentation we check if it needs
    # additional indentation by comparing the number
    # of spaces before a character of the line to
    # the ground truth indentation.
    line = s.doc.text[s.doc.ranges[startline]]
    fc = findfirst(c -> !isspace(c), line)-1
    ns = max(0, nspaces(s) - fc)

    if !include_quotes
        idx = startswith(str, "\"\"\"") ? 4 : 2
        str = str[idx:end-idx+1]
        str = strip(str, ' ')
        str[1] == '\n' && (str = str[2:end])
        str[end] == '\n' && (str = str[1:end-1])
    end
    s.offset += x.fullspan

    lines = split(str, "\n")

    if length(lines) == 1 
        return PTree(x, loc[1], loc[1], lines[1])
    end

    t = PTree(CSTParser.StringH, -1, -1, ns, 0, nothing, PTree[], Ref(x))
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        tt = PTree(CSTParser.LITERAL, ln, ln, nspaces(s), length(l), l, nothing, nothing)
        add_node!(t, tt)
    end
    t
end

# StringH
function p_stringh(x, s; include_quotes=true)
    startline, endline, str = s.doc.lit_strings[s.offset-1]

    line = s.doc.text[s.doc.ranges[startline]]

    fc = findfirst(c -> !isspace(c), line)-1
    ns = max(0, nspaces(s) - fc)

    if !include_quotes
        idx = startswith(str, "\"\"\"") ? 4 : 2
        str = str[idx:end-idx+1]
        str = strip(str, ' ')
        str[1] == '\n' && (str = str[2:end])
        str[end] == '\n' && (str = str[1:end-1])
    end
    s.offset += x.fullspan

    lines = split(str, "\n")

    if length(lines) == 1 
        t = PTree(x, startline, startline, lines[1])
        t.typ = CSTParser.LITERAL
        return t
    end

    t = PTree(x, ns)
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        tt = PTree(CSTParser.LITERAL, ln, ln, nspaces(s), length(l), l, nothing, nothing)
        add_node!(t, tt)
    end
    t
end


# MacroCall
function p_macrocall(x, s)
    t = PTree(x, nspaces(s))
    if x.args[1].typ === CSTParser.GlobalRefDoc
        loc = cursor_loc(s)
        # x.args[1] is empty and fullspan is 0 so we can skip it
        add_node!(t, PTree(CSTParser.LITERAL, loc[1], loc[1], nspaces(s), 3, "\"\"\"", nothing, nothing))
        if x.args[2].typ === CSTParser.LITERAL
            add_node!(t, p_literal(x.args[2], s, include_quotes=false))
        elseif x.args[2].typ == CSTParser.StringH
            add_node!(t, p_stringh(x.args[2], s, include_quotes=false))
        end
        loc = cursor_loc(s)
        add_node!(t, PTree(CSTParser.LITERAL, loc[1], loc[1], nspaces(s), 3, "\"\"\"", nothing, nothing))
        add_node!(t, pretty(x.args[3], s))
        return t
    end

    multi_arg = length(x) > 5 && CSTParser.is_lparen(x.args[2]) ? true : false

    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if a.typ === CSTParser.MacroName
            if a.fullspan - a.span > 0 && length(x) > 1
                add_node!(t, n, join_lines=true)
                add_node!(t, Whitespace(1))
            else
                # assumes the next argument is a brace of some sort
                add_node!(t, n, join_lines=true)
            end
        elseif is_opener(n)
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(0))
            s.indent += s.indent_size
        elseif is_closer(n)
            add_node!(t, Placeholder(0))
            add_node!(t, n, join_lines=true)
            s.indent -= s.indent_size
        elseif CSTParser.is_comma(a) && i < length(x) && !is_punc(x.args[i+1])
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(1))
        elseif a.fullspan - a.span > 0 && i < length(x)
            add_node!(t, n, join_lines=true)
            add_node!(t, Whitespace(1))
        else
            add_node!(t, n, join_lines=true)
        end
    end
    t
end

# Block
function p_block(x, s; ignore_single_line=false, from_quote=false)
    t = PTree(x, nspaces(s))
    single_line = ignore_single_line ? false : cursor_loc(s)[1] == cursor_loc(s, s.offset+x.span-1)[1] 
    # @info "" from_quote single_line ignore_single_line
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if from_quote && !single_line
            if i == 1 || CSTParser.is_comma(a)
                add_node!(t, n, join_lines=true)
            elseif CSTParser.is_comma(x.args[i-1])
                add_node!(t, Whitespace(1))
                add_node!(t, n, join_lines=true)
            else
                add_node!(t, Semicolon())
                add_node!(t, n)
            end
        elseif single_line
            if i == 1 || CSTParser.is_comma(a)
                add_node!(t, n, join_lines=true)
            elseif CSTParser.is_comma(x.args[i-1])
                add_node!(t, Whitespace(1))
                add_node!(t, n, join_lines=true)
            else
                add_node!(t, Semicolon())
                add_node!(t, Whitespace(1))
                add_node!(t, n, join_lines=true)
            end
        else
            if i < length(x) && CSTParser.is_comma(a) && is_punc(x.args[i+1])
                add_node!(t, n, join_lines=true)
            elseif CSTParser.is_comma(a) && i != length(x)
                add_node!(t, n, join_lines=true)
            else
                add_node!(t, n)
            end
        end
    end
    t
end

# Abstract
function p_abstract(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[3], s), join_lines=true)
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[4], s), join_lines=true)
    t
end

# Primitive
function p_primitive(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[3], s), join_lines=true)
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[4], s), join_lines=true)
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[5], s), join_lines=true)
    t
end

# FunctionDef/Macro
function p_function(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    if length(x) > 3
        if x.args[3].fullspan == 0
            add_node!(t, Whitespace(1))
            add_node!(t, pretty(x.args[4], s), join_lines=true)
        else
            s.indent += s.indent_size
            add_node!(t, p_block(x.args[3], s, ignore_single_line=true))
            s.indent -= s.indent_size
            add_node!(t, pretty(x.args[4], s))
        end
    else
        # function stub, i.e. "function foo end"
        # this should be on one line
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(x.args[3], s), join_lines=true)
    end
    t
end

# Struct
function p_struct(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    if x.args[3].fullspan == 0
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(x.args[4], s), join_lines=true)
    else
        s.indent += s.indent_size
        add_node!(t, p_block(x.args[3], s, ignore_single_line=true))
        s.indent -= s.indent_size
        add_node!(t, pretty(x.args[4], s))
    end
    t
end

# Mutable
function p_mutable(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[3], s), join_lines=true)
    if x.args[4].fullspan == 0
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(x.args[5], s), join_lines=true)
    else
        s.indent += s.indent_size
        add_node!(t, p_block(x.args[4], s, ignore_single_line=true))
        s.indent -= s.indent_size
        add_node!(t, pretty(x.args[5], s))
    end
    t
end

# For/While
function p_loop(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    s.indent += s.indent_size
    add_node!(t, p_block(x.args[3], s, ignore_single_line=true))
    s.indent -= s.indent_size
    add_node!(t, pretty(x.args[4], s))
    t
end

# Do
function p_do(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    if x.args[3].fullspan != 0
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(x.args[3], s), join_lines=true)
    end
    if x.args[4].typ === CSTParser.Block
        s.indent += s.indent_size
        add_node!(t, p_block(x.args[4], s, ignore_single_line=true))
        s.indent -= s.indent_size
    end
    add_node!(t, pretty(x.args[end], s))
    t
end

# Try
function p_try(x, s)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if a.fullspan == 0
        elseif a.typ === CSTParser.KEYWORD
            add_node!(t, pretty(a, s))
        elseif a.typ === CSTParser.Block
            s.indent += s.indent_size
            add_node!(t, p_block(a, s, ignore_single_line=true))
            s.indent -= s.indent_size
        else
            add_node!(t, Whitespace(1))
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# ModuleH/BareModule
function p_module(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    if x.args[3].fullspan == 0
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(x.args[4], s), join_lines=true)
    else
        add_node!(t, pretty(x.args[3], s))
        add_node!(t, pretty(x.args[4], s))
    end
    t
end

# Const/Local/Global/Return
function p_vardef(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if x.args[2].fullspan != 0
        for a in x.args[2:end]
            add_node!(t, Whitespace(1))
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Begin
function p_begin(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if x.args[2].fullspan == 0
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(x.args[3], s), join_lines=true)
    else
        s.indent += s.indent_size
        add_node!(t, p_block(x.args[2], s, ignore_single_line=true))
        s.indent -= s.indent_size
        add_node!(t, pretty(x.args[3], s))
    end
    t
end

# Quote
function p_quote(x, s)
    t = PTree(x, nspaces(s))
    if x.args[1].typ === CSTParser.KEYWORD && x.args[1].kind === Tokens.QUOTE
        add_node!(t, pretty(x.args[1], s))
        if x.args[2].fullspan == 0
            add_node!(t, Whitespace(1))
            add_node!(t, pretty(x.args[3], s), join_lines=true)
        else
            s.indent += s.indent_size
            add_node!(t, p_block(x.args[2], s, ignore_single_line=true))
            s.indent -= s.indent_size
            add_node!(t, pretty(x.args[3], s))
        end
    else
        for a in x.args
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Let
function p_let(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if length(x.args) > 3
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(x.args[2], s), join_lines=true)
        s.indent += s.indent_size
        add_node!(t, p_block(x.args[3], s, ignore_single_line=true))
        s.indent -= s.indent_size
    else
        s.indent += s.indent_size
        add_node!(t, p_block(x.args[2], s, ignore_single_line=true))
        s.indent -= s.indent_size
    end
    add_node!(t, pretty(x.args[end], s))
    t
end

# If
function p_if(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    if x.args[1].typ === CSTParser.KEYWORD && x.args[1].kind === Tokens.IF
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(x.args[2], s), join_lines=true)
        s.indent += s.indent_size
        add_node!(t, p_block(x.args[3], s, ignore_single_line=true))
        s.indent -= s.indent_size
        add_node!(t, pretty(x.args[4], s))
        if length(x.args) > 4
            if x.args[4].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1))
                add_node!(t, pretty(x.args[5], s), join_lines=true)
            else
                s.indent += s.indent_size
                add_node!(t, p_block(x.args[5], s, ignore_single_line=true))
                s.indent -= s.indent_size
            end
            # END KEYWORD
            add_node!(t, pretty(x.args[6], s))
        end
    else
        s.indent += s.indent_size
        add_node!(t, p_block(x.args[2], s, ignore_single_line=true))
        s.indent -= s.indent_size
        if length(x.args) > 2
            add_node!(t, pretty(x.args[3], s))

            # this either else or elseif
            if x.args[3].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1))
                add_node!(t, pretty(x.args[4], s), join_lines=true)
            else
                s.indent += s.indent_size
                add_node!(t, p_block(x.args[4], s, ignore_single_line=true))
                s.indent -= s.indent_size
            end
        end
    end
    t
end

# ChainOpCall/Comparison
function p_chaincall(x, s)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if a.typ === CSTParser.OPERATOR
            add_node!(t, Whitespace(1))
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(1))
        elseif i == length(x) - 1 && is_punc(a) && is_punc(x.args[i+1])
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a) && i != length(x)
            add_node!(t, n, join_lines=true)
            add_node!(t, Whitespace(1))
        else
            add_node!(t, n, join_lines=true)
        end
    end
    t
end

# CSTParser.Kw
function p_kw(x, s)
    t = PTree(x, nspaces(s))
    for a in x
        if a.kind === Tokens.EQ
            add_node!(t, Whitespace(1))
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace(1))
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function nestable(x::CSTParser.EXPR)
    CSTParser.defines_function(x) && (return true)
    op = x.args[2]
    op.kind === Tokens.ANON_FUNC && (return false)
    op.kind === Tokens.PAIR_ARROW && (return false)
    CSTParser.precedence(op) in (1, 6) && (return false)
    if op.kind == Tokens.LAZY_AND || op.kind == Tokens.LAZY_OR
        arg = x.args[1]
        while arg.typ === CSTParser.InvisBrackets
            arg = arg.args[2]
        end
        if arg.typ === CSTParser.BinaryOpCall
            op = arg.args[2].kind
            (op == Tokens.LAZY_AND || op == Tokens.LAZY_OR) && (return true)
        end

        arg = x.args[3]
        while arg.typ === CSTParser.InvisBrackets
            arg = arg.args[2]
        end
        if arg.typ === CSTParser.BinaryOpCall
            op = arg.args[2].kind
            (op == Tokens.LAZY_AND || op == Tokens.LAZY_OR) && (return true)
        end

        p = x.parent
        p === nothing && (return false)
        while p !== nothing && p.typ === CSTParser.InvisBrackets
            p = p.parent
        end
        return p.typ === CSTParser.If || p.typ === CSTParser.BinaryOpCall || p.typ === CSTParser.While
    end
    true
end



# BinaryOpCall
function p_binarycall(x, s; nonest=false, nospaces=false)
    t = PTree(x, nspaces(s))
    op = x.args[2]
    nonest = nonest || op.kind === Tokens.COLON
    if x.parent.typ === CSTParser.Curly && op.kind in (Tokens.ISSUBTYPE, Tokens.ISSUPERTYPE)
        nospaces = true
    elseif op.kind === Tokens.COLON
        nospaces = true
    end

    if x.args[1].typ === CSTParser.BinaryOpCall
        add_node!(t, p_binarycall(x.args[1], s, nonest=nonest, nospaces=nospaces))
    else
        add_node!(t, pretty(x.args[1], s))
    end

    if (CSTParser.precedence(op) in (8, 13, 14, 16) && op.kind !== Tokens.ANON_FUNC) || nospaces
        add_node!(t, pretty(op, s), join_lines=true)
    elseif op.kind === Tokens.EX_OR
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(op, s), join_lines=true)
    elseif nestable(x) && !nonest
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(op, s), join_lines=true)
        # for newline
        add_node!(t, Placeholder(1))
        # for indent, will be converted to `indent_size` if needed
        add_node!(t, Placeholder(0))
    else
        add_node!(t, Whitespace(1))
        add_node!(t, pretty(op, s), join_lines=true)
        add_node!(t, Whitespace(1))
    end
    

    CSTParser.defines_function(x) && (s.indent += s.indent_size)
    if x.args[3].typ === CSTParser.BinaryOpCall
        n = p_binarycall(x.args[3], s, nonest=nonest, nospaces=nospaces)
    else
        n = pretty(x.args[3], s)
    end
    CSTParser.defines_function(x) && (s.indent -= s.indent_size)
    add_node!(t, n, join_lines=true)
    t
end

# WhereOpCall
function p_wherecall(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))

    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, Whitespace(1))

    # Used to mark where `B` starts.
    add_node!(t, Placeholder(0))

    multi_arg = length(CSTParser.get_where_params(x)) > 1
    in_braces = CSTParser.is_lbrace(x.args[3])

    # @info "" multi_arg in_braces x.args[3].val == "{" x.args[end].val

    for a in x.args[3:end]
        if is_opener(a) && multi_arg
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Placeholder(0))
            s.indent += s.indent_size
        elseif is_closer(a) && multi_arg
            add_node!(t, Placeholder(0))
            add_node!(t, pretty(a, s), join_lines=true)
            s.indent -= s.indent_size
        elseif CSTParser.is_comma(a)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Placeholder(0))
        elseif in_braces && a.typ === CSTParser.BinaryOpCall
            add_node!(t, p_binarycall(a, s, nospaces=true), join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Conditional
function p_condcall(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    add_node!(t, Placeholder(1))

    add_node!(t, pretty(x.args[3], s), join_lines=true)
    add_node!(t, Whitespace(1))
    add_node!(t, pretty(x.args[4], s), join_lines=true)
    add_node!(t, Placeholder(1))

    add_node!(t, pretty(x.args[5], s), join_lines=true)
    t
end

# UnaryOpCall
function p_unarycall(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)
    t
end

function p_curly(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)

    multi_arg = length(CSTParser.get_curly_params(x)) > 1

    if multi_arg
        add_node!(t, Placeholder(0))
        s.indent += s.indent_size
    end

    for (i, a) in enumerate(x.args[3:end])
        if i + 2 == length(x) && multi_arg
            add_node!(t, Placeholder(0))
            add_node!(t, pretty(a, s), join_lines=true)
            s.indent -= s.indent_size
        elseif CSTParser.is_comma(a) && i < length(x) - 3 && !is_punc(x.args[i+1])
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Placeholder(0))
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

function p_call(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, pretty(x.args[2], s), join_lines=true)

    multi_arg = length(CSTParser.get_args(x)) > 1

    if multi_arg
        add_node!(t, Placeholder(0))
        s.indent += s.indent_size
    end

    for (i, a) in enumerate(x.args[3:end])
        if i + 2 == length(x) && multi_arg
            add_node!(t, Placeholder(0))
            add_node!(t, pretty(a, s), join_lines=true)
            s.indent -= s.indent_size
        elseif CSTParser.is_comma(a) && i < length(x) - 3 && !is_punc(x.args[i+1])
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Placeholder(1))
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# InvisBrackets
function p_invisbrackets(x, s)
    t = PTree(x, nspaces(s))
    multi_arg = length(x) > 4

    # multi_arg && (s.indent += s.indent_size)
    for (i, a) in enumerate(x)
        # @info "" a.typ === CSTParser.Block
        n = a.typ === CSTParser.Block ? p_block(a, s, from_quote=true) : pretty(a, s)
        if is_opener(n) && multi_arg
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(0))
        elseif is_closer(n) && multi_arg
            add_node!(t, Placeholder(0))
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a) && i < length(x) && !is_punc(x.args[i+1])
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(1))
        else
            add_node!(t, n, join_lines=true)
        end
    end
    # multi_arg && (s.indent -= s.indent_size)
    t
end

# TupleH
function p_tuple(x, s)
    t = PTree(x, nspaces(s))
    multi_arg = false
    if CSTParser.is_lparen(x.args[1]) && length(x) > 4
        multi_arg = true
    elseif !CSTParser.is_lparen(x.args[1]) && length(x) > 2
        multi_arg = true
    end

    # multi_arg && (s.indent += s.indent_size)
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if is_opener(n) && multi_arg
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(0))
        elseif is_closer(n) && multi_arg
            add_node!(t, Placeholder(0))
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a) && i < length(x) && !is_punc(x.args[i+1])
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(1))
        else
            add_node!(t, n, join_lines=true)
        end
    end
    # multi_arg && (s.indent -= s.indent_size)
    t
end

# Braces
function p_braces(x, s)
    t = PTree(x, nspaces(s))
    # {a,b}
    multi_arg = length(x) > 4
    # @info "" multi_arg typeof(x)

    # multi_arg && (s.indent += s.indent_size)
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if i == 1 && multi_arg
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(0))
        elseif i == length(x) && multi_arg
            add_node!(t, Placeholder(0))
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a) && i < length(x) && !is_punc(x.args[i+1])
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(0))
        else
            add_node!(t, n, join_lines=true)
        end
    end
    # multi_arg && (s.indent -= s.indent_size)
    t
end

# Vect
function p_vect(x, s)
    t = PTree(x, nspaces(s))
    # [a,b]
    multi_arg = length(x) > 4

    # multi_arg && (s.indent += s.indent_size)
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if i == 1 && multi_arg
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(0))
        elseif i == length(x) && multi_arg
            add_node!(t, Placeholder(0))
            add_node!(t, n, join_lines=true)
        elseif CSTParser.is_comma(a) && i < length(x) && !is_punc(x.args[i+1])
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(1))
        else
            add_node!(t, n, join_lines=true)
        end
    end
    # multi_arg && (s.indent -= s.indent_size)
    t
end


# Parameters
function p_params(x, s)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        n = pretty(a, s)
        if CSTParser.is_comma(a) && i < length(x) && !is_punc(x.args[i+1])
            add_node!(t, n, join_lines=true)
            add_node!(t, Placeholder(1))
        else
            add_node!(t, n, join_lines=true)
        end
    end
    t
end

# Import, Export, Using, ImportAll
function p_import(x, s)
    t = PTree(x, nspaces(s))
    add_node!(t, pretty(x.args[1], s))
    add_node!(t, Whitespace(1))
    for (i, a) in enumerate(x.args[2:end])
        if CSTParser.is_comma(a)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Placeholder(1))
        elseif CSTParser.is_colon(a)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace(1))
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Ref
function p_ref(x, s)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if CSTParser.is_comma(a)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace(1))
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(t, p_binarycall(a, s, nonest=true, nospaces=true), join_lines=true)
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Vcat/TypedVcat
function p_vcat(x, s)
    t = PTree(x, nspaces(s))
    st = x.typ === CSTParser.Vcat ? 1 : 2
    for (i, a) in enumerate(x)
        if i > st && i < length(x) - 1
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Semicolon())
            add_node!(t, Whitespace(1))
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Hcat/TypedHcat
function p_hcat(x, s)
    t = PTree(x, nspaces(s))
    st = x.typ === CSTParser.Hcat ? 1 : 2
    for (i, a) in enumerate(x)
        if i > st && i < length(x) - 1
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace(1))
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Row
function p_row(x, s)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if i < length(x)
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace(1))
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end

# Comprehension/Generator/Filter
function p_comprehension(x, s)
    t = PTree(x, nspaces(s))
    for (i, a) in enumerate(x)
        if a.typ === CSTParser.KEYWORD
            add_node!(t, Whitespace(1))
            add_node!(t, pretty(a, s), join_lines=true)
            add_node!(t, Whitespace(1))
        else
            add_node!(t, pretty(a, s), join_lines=true)
        end
    end
    t
end
