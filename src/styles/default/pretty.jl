# Creates a _prettified_ version of a CST.
function pretty(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; kwargs...)
    style = getstyle(ds)
    if cst.typ === CSTParser.IDENTIFIER
        return p_identifier(style, cst, s)
    elseif cst.typ === CSTParser.OPERATOR
        return p_operator(style, cst, s)
    elseif cst.typ === CSTParser.PUNCTUATION
        return p_punctuation(style, cst, s)
    elseif cst.typ === CSTParser.KEYWORD
        return p_keyword(style, cst, s)
    elseif cst.typ === CSTParser.LITERAL
        return p_literal(style, cst, s)
    elseif cst.typ === CSTParser.StringH
        return p_stringh(style, cst, s)
    elseif cst.typ === CSTParser.Block
        return p_block(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.ModuleH
        return p_module(style, cst, s)
    elseif cst.typ === CSTParser.BareModule
        return p_baremodule(style, cst, s)
    elseif cst.typ === CSTParser.FunctionDef
        return p_functiondef(style, cst, s)
    elseif cst.typ === CSTParser.Macro
        return p_macro(style, cst, s)
    elseif cst.typ === CSTParser.Primitive
        return p_primitive(style, cst, s)
    elseif cst.typ === CSTParser.Struct
        return p_struct(style, cst, s)
    elseif cst.typ === CSTParser.Mutable
        return p_mutable(style, cst, s)
    elseif cst.typ === CSTParser.Abstract
        return p_abstract(style, cst, s)
    elseif cst.typ === CSTParser.Primitive
        return p_primitive(style, cst, s)
    elseif cst.typ === CSTParser.For
        return p_for(style, cst, s)
    elseif cst.typ === CSTParser.While
        return p_while(style, cst, s)
    elseif cst.typ === CSTParser.Do
        return p_do(style, cst, s)
    elseif cst.typ === CSTParser.If
        return p_if(style, cst, s)
    elseif cst.typ === CSTParser.Try
        return p_try(style, cst, s)
    elseif cst.typ === CSTParser.TopLevel
        return p_toplevel(style, cst, s)
    elseif cst.typ === CSTParser.Begin
        return p_begin(style, cst, s)
    elseif cst.typ === CSTParser.Quote
        return p_quote(style, cst, s)
    elseif cst.typ === CSTParser.Let
        return p_let(style, cst, s)
    elseif cst.typ === CSTParser.Vect
        return p_vect(style, cst, s)
    elseif cst.typ === CSTParser.Comprehension
        return p_comprehension(style, cst, s)
    elseif cst.typ === CSTParser.TypedComprehension
        return p_typedcomprehension(style, cst, s)
    elseif cst.typ === CSTParser.Braces
        return p_braces(style, cst, s)
    elseif cst.typ === CSTParser.BracesCat
        return p_bracescat(style, cst, s)
    elseif cst.typ === CSTParser.TupleH
        return p_tupleh(style, cst, s)
    elseif cst.typ === CSTParser.InvisBrackets
        return p_invisbrackets(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.Curly
        return p_curly(style, cst, s)
    elseif cst.typ === CSTParser.Call
        return p_call(style, cst, s)
    elseif cst.typ === CSTParser.MacroCall && cst[1].typ === CSTParser.GlobalRefDoc
        return p_globalrefdoc(style, cst, s)
    elseif cst.typ === CSTParser.MacroCall && is_macrodoc(cst)
        return p_macrodoc(style, cst, s)
    elseif cst.typ === CSTParser.MacroCall
        return p_macrocall(style, cst, s)
    elseif cst.typ === CSTParser.WhereOpCall
        return p_whereopcall(style, cst, s)
    elseif cst.typ === CSTParser.ConditionalOpCall
        return p_conditionalopcall(style, cst, s)
    elseif cst.typ === CSTParser.BinaryOpCall
        return p_binaryopcall(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.UnaryOpCall
        return p_unaryopcall(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.ChainOpCall
        return p_chainopcall(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.Comparison
        return p_comparison(style, cst, s; kwargs...)
    elseif cst.typ === CSTParser.ColonOpCall
        return p_colonopcall(style, cst, s)
    elseif cst.typ === CSTParser.Kw
        return p_kw(style, cst, s)
    elseif cst.typ === CSTParser.Parameters
        return p_parameters(style, cst, s)
    elseif cst.typ === CSTParser.Local
        return p_local(style, cst, s)
    elseif cst.typ === CSTParser.Global
        return p_global(style, cst, s)
    elseif cst.typ === CSTParser.Const
        return p_const(style, cst, s)
    elseif cst.typ === CSTParser.Return
        return p_return(style, cst, s)
    elseif cst.typ === CSTParser.Outer
        return p_outer(style, cst, s)
    elseif cst.typ === CSTParser.Import
        return p_import(style, cst, s)
    elseif cst.typ === CSTParser.Export
        return p_export(style, cst, s)
    elseif cst.typ === CSTParser.Using
        return p_using(style, cst, s)
    elseif cst.typ === CSTParser.Row
        return p_row(style, cst, s)
    elseif cst.typ === CSTParser.Vcat
        return p_vcat(style, cst, s)
    elseif cst.typ === CSTParser.TypedVcat
        return p_typedvcat(style, cst, s)
    elseif cst.typ === CSTParser.Hcat
        return p_hcat(style, cst, s)
    elseif cst.typ === CSTParser.TypedHcat
        return p_typedhcat(style, cst, s)
    elseif cst.typ === CSTParser.Ref
        return p_ref(style, cst, s)
    elseif cst.typ === CSTParser.Generator
        return p_generator(style, cst, s)
    elseif cst.typ === CSTParser.Filter
        return p_filter(style, cst, s)
    elseif cst.typ === CSTParser.Flatten
        return p_flatten(style, cst, s)
    elseif cst.typ === CSTParser.FileH
        return p_fileh(style, cst, s)
    end

    t = FST(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, join_lines = true)
    end
    t
end
pretty(style::S, cst::CSTParser.EXPR, s::State; kwargs...) where {S<:AbstractStyle} =
    pretty(DefaultStyle(style), cst, s; kwargs...)

function p_fileh(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, join_lines = false, max_padding = 0)
    end
    t
end
p_fileh(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_fileh(DefaultStyle(style), cst, s)

@inline function p_identifier(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    s.offset += length(cst.val) + (cst.fullspan - cst.span)
    FST(cst, loc[2], loc[1], loc[1], cst.val)
end
p_identifier(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_identifier(DefaultStyle(style), cst, s)

@inline function p_operator(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    val = string(CSTParser.Expr(cst))
    s.offset += length(val) + (cst.fullspan - cst.span)
    FST(cst, loc[2], loc[1], loc[1], val)
end
p_operator(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_operator(DefaultStyle(style), cst, s)

@inline function p_keyword(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    val =
        cst.kind === Tokens.ABSTRACT ? "abstract" :
        cst.kind === Tokens.BAREMODULE ? "baremodule" :
        cst.kind === Tokens.BEGIN ? "begin" :
        cst.kind === Tokens.BREAK ? "break" :
        cst.kind === Tokens.CATCH ? "catch" :
        cst.kind === Tokens.CONST ? "const" :
        cst.kind === Tokens.CONTINUE ? "continue" :
        cst.kind === Tokens.NEW ? "new" :
        cst.kind === Tokens.DO ? "do" :
        cst.kind === Tokens.IF ? "if" :
        cst.kind === Tokens.ELSEIF ? "elseif" :
        cst.kind === Tokens.ELSE ? "else" :
        cst.kind === Tokens.END ? "end" :
        cst.kind === Tokens.EXPORT ? "export" :
        cst.kind === Tokens.FINALLY ? "finally" :
        cst.kind === Tokens.FOR ? "for" :
        cst.kind === Tokens.FUNCTION ? "function" :
        cst.kind === Tokens.GLOBAL ? "global" :
        cst.kind === Tokens.IMPORT ? "import" :
        cst.kind === Tokens.LET ? "let" :
        cst.kind === Tokens.LOCAL ? "local" :
        cst.kind === Tokens.MACRO ? "macro" :
        cst.kind === Tokens.MODULE ? "module" :
        cst.kind === Tokens.MUTABLE ? "mutable" :
        cst.kind === Tokens.OUTER ? "outer" :
        cst.kind === Tokens.PRIMITIVE ? "primitive" :
        cst.kind === Tokens.QUOTE ? "quote" :
        cst.kind === Tokens.RETURN ? "return" :
        cst.kind === Tokens.STRUCT ? "struct" :
        cst.kind === Tokens.TYPE ? "type" :
        cst.kind === Tokens.TRY ? "try" :
        cst.kind === Tokens.USING ? "using" : cst.kind === Tokens.WHILE ? "while" : ""
    s.offset += cst.fullspan
    FST(cst, loc[2], loc[1], loc[1], val)
end
p_keyword(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_keyword(DefaultStyle(style), cst, s)

@inline function p_punctuation(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    val =
        cst.kind === Tokens.LPAREN ? "(" :
        cst.kind === Tokens.LBRACE ? "{" :
        cst.kind === Tokens.LSQUARE ? "[" :
        cst.kind === Tokens.RPAREN ? ")" :
        cst.kind === Tokens.RBRACE ? "}" :
        cst.kind === Tokens.RSQUARE ? "]" :
        cst.kind === Tokens.COMMA ? "," :
        cst.kind === Tokens.SEMICOLON ? ";" :
        cst.kind === Tokens.AT_SIGN ? "@" : cst.kind === Tokens.DOT ? "." : ""
    s.offset += cst.fullspan
    FST(cst, loc[2], loc[1], loc[1], val)
end
p_punctuation(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_punctuation(DefaultStyle(style), cst, s)

struct FormatRule{T<:AbstractStyle}
    style::T
    opts::Options
end
format_text(text::AbstractString, fr::FormatRule) = format_text(text, fr.style, fr.opts)

block_modifier(rule::FormatRule) =
    Rule(1) do parser, block
        block.t isa CodeBlock || return
        language = block.t.info
        code = block.literal
        if startswith(language, "@example") ||
           startswith(language, "@repl") ||
           startswith(language, "@eval") ||
           startswith(language, "julia")
            block.literal = format_text(code, rule)
        elseif startswith(language, "jldoctest")
            block.literal = if occursin(r"^julia> "m, code)
                doctests = IOBuffer()
                first_test = true
                for (an_input, output) in repl_splitter(code)
                    if first_test
                        first_test = false
                    else
                        write(doctests, "\n\n")
                    end
                    write(doctests, "julia> ")
                    first_line = true
                    for line in split(format_text(an_input, rule), '\n')
                        if first_line
                            first_line = false
                        else
                            write(doctests, "\n       ")
                        end
                        write(doctests, line)
                    end
                    write(doctests, '\n')
                    write(doctests, output)
                end
                write(doctests, '\n')
                String(take!(doctests))
            else
                an_input, output = split(code, r"\n+# output\n+", limit = 2)
                string(
                    format_text(format_text(String(an_input), rule), rule),
                    "\n\n# output\n\n",
                    output,
                )
            end
        end
    end

function format_docstring(style::AbstractStyle, state::State, text::AbstractString)
    state_indent = state.indent
    boundaries = findall(text) do character
        character != '"'
    end
    # first, we need to remove any user indent
    # only some lines will "count" towards increasing the user indent
    # start at a very big guess
    user_indent = typemax(Int)
    user_indented = text[boundaries[1]:boundaries[end]]
    deindented = IOBuffer()
    user_lines = split(user_indented, '\n')
    for (index, line) in enumerate(user_lines)
        # the first line doesn't count
        if index != 1
            first_character = findfirst(character -> !isspace(character), line)
            if first_character === nothing
                # if the line is only spaces, it only counts if it is the last line
                if index == length(user_lines)
                    user_indent = min(user_indent, length(line))
                end
            else
                user_indent = min(user_indent, first_character - 1)
            end
        end
    end
    deindented_string =
    # if there are no lines at all, or if the user indent is zero, we don't have to change anything
        if user_indent == typemax(Int) || user_indent == 0
            user_indented
        else
            # else, deindent non-first lines
            first_line = true
            for line in split(user_indented, '\n')
                if first_line
                    first_line = false
                    write(deindented, line)
                else
                    write(deindented, '\n')
                    write(deindented, SubString(line, user_indent + 1, length(line)))
                end
            end
            String(take!(deindented))
        end
    # then, we format
    formatted = markdown(
        enable!(
            Parser(),
            [
                AdmonitionRule(),
                FootnoteRule(),
                MathRule(),
                TableRule(),
                FormatRule(style, state.opts),
            ],
        )(
            deindented_string,
        ),
    )
    # Indent all non-first lines to match the current parser indent
    buf = IOBuffer()
    indent = " "^state_indent
    # This is the first line, so the rest have to be indented. A newline for it will be added below
    write(buf, "\"\"\"")
    for line in split(formatted, '\n')
        # The last line will be empty and will turn into an indent, so no need to indent the last line below
        write(buf, '\n')
        write(buf, indent)
        write(buf, line)
    end
    write(buf, "\"\"\"")
    String(take!(buf))
end

@inline function p_literal(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    from_docstring = false,
)
    loc = cursor_loc(s)
    if !is_str_or_cmd(cst.kind)
        val = cst.val

        if cst.kind === Tokens.FLOAT && endswith(cst.val, "f0")
            # Float32
            val = val[1:end-2]
            dotidx = findlast(c -> c == '.', val)
            if dotidx === nothing
                val *= ".0"
            elseif dotidx == length(val)
                val *= '0'
            elseif dotidx == 1
                val = '0' * val
            end
            val *= "f0"
        elseif cst.kind === Tokens.FLOAT
            if endswith(cst.val, ".")
                # If a floating point ends in `.`, add trailing zero.
                val *= '0'
            elseif startswith(cst.val, ".")
                val = '0' * val
            end
        end

        s.offset += length(cst.val) + (cst.fullspan - cst.span)
        return FST(cst, loc[2], loc[1], loc[1], val)
    end

    # Strings are unescaped by CSTParser
    # to mimic Meta.parse which makes reproducing
    # the correct output from the LITERAL value problematic.
    # So we'll just look at the source directly!
    str_info = get(s.doc.lit_strings, s.offset - 1, nothing)

    # @info "" str_info loc s.offset

    # Tokenize treats the `ix` part of r"^(=?[^=]+)=(.*)$"ix as an
    # IDENTIFIER where as CSTParser parses it as a LITERAL.
    # An IDENTIFIER won't show up in the string literal lookup table.
    if str_info === nothing &&
       (cst.parent.typ === CSTParser.x_Str || cst.parent.typ === CSTParser.x_Cmd)
        s.offset += length(cst.val) + (cst.fullspan - cst.span)
        return FST(cst, loc[2], loc[1], loc[1], cst.val)
    end

    startline, endline, str = str_info
    s.offset += length(str) + (cst.fullspan - cst.span)

    if from_docstring && s.opts.format_docstrings
        str = format_docstring(ds, s, str)
    end

    lines = split(str, "\n")

    if length(lines) == 1
        return FST(cst, loc[2], loc[1], loc[1], lines[1])
    end

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    t = FST(
        CSTParser.StringH,
        -1,
        -1,
        loc[2] - 1,
        0,
        nothing,
        FST[],
        Ref(cst),
        AllowNest,
        0,
        loc[2],
    )
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        tt = FST(
            CSTParser.LITERAL,
            ln,
            ln,
            sidx - 1,
            length(l),
            l,
            nothing,
            nothing,
            AllowNest,
            0,
            -1,
        )
        add_node!(t, tt, s)
    end
    # make sure endline matches up with source code
    t.endline = endline
    t
end
p_literal(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    from_docstring = false,
) where {S<:AbstractStyle} =
    p_literal(DefaultStyle(style), cst, s; from_docstring = from_docstring)

# StringH
function p_stringh(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    loc = cursor_loc(s)
    startline, endline, str = s.doc.lit_strings[s.offset-1]
    s.offset += length(str) + (cst.fullspan - cst.span)

    lines = split(str, "\n")

    if length(lines) == 1
        t = FST(cst, loc[2], startline, startline, lines[1])
        t.typ = CSTParser.LITERAL
        return t
    end

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    t = FST(cst, loc[2] - 1)
    t.line_offset = loc[2]
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        tt = FST(
            CSTParser.LITERAL,
            ln,
            ln,
            sidx - 1,
            length(l),
            l,
            nothing,
            nothing,
            AllowNest,
            0,
            -1,
        )
        add_node!(t, tt, s)
    end
    t
end
p_stringh(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_stringh(DefaultStyle(style), cst, s)

# GlobalRefDoc (docstring)
function p_globalrefdoc(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    t.typ = CSTParser.GlobalRefDoc

    # cst[1] is empty and fullspan is 0 so we can skip it
    if cst[2].typ === CSTParser.LITERAL
        add_node!(t, p_literal(style, cst[2], s, from_docstring = true), s, max_padding = 0)
    elseif cst[2].typ == CSTParser.StringH
        add_node!(t, p_stringh(style, cst[2], s), s)
    end
    add_node!(t, pretty(style, cst[3], s), s, max_padding = 0)
    return t
end
p_globalrefdoc(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_globalrefdoc(DefaultStyle(style), cst, s)

# @doc "example"
function p_macrodoc(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, nspaces(s))
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    t.typ = CSTParser.GlobalRefDoc

    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    n = pretty(style, cst[3], s)
    join_lines = t.endline == n.startline
    join_lines && add_node!(t, Whitespace(1), s)
    add_node!(t, n, s, join_lines = join_lines, max_padding = 0)
    return t
end
p_macrodoc(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_macrodoc(DefaultStyle(style), cst, s)

# MacroCall
function p_macrocall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))
    has_closer = is_closer(cst.args[end])

    # !has_closer && length(t.nodes) > 1 && (t.typ = MacroBlock)
    !has_closer && (t.typ = MacroBlock)

    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if a.typ === CSTParser.MacroName
            if a.fullspan - a.span > 0 && length(cst) > 1
                add_node!(t, n, s, join_lines = true)
                add_node!(t, Whitespace(1), s)
            else
                # assumes the next argument is a brace of some sort
                add_node!(t, n, s, join_lines = true)
            end
        elseif is_opener(n) && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(n) && nest
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif a.fullspan - a.span > 0
            if has_closer
                add_node!(t, n, s, join_lines = true)
                if i < length(cst) - 1 && cst[i+1].typ !== CSTParser.Parameters
                    add_node!(t, Whitespace(1), s)
                end
            else
                padding = is_block(n) ? 0 : -1
                add_node!(t, n, s, join_lines = true, max_padding = padding)
                i < length(cst) && add_node!(t, Whitespace(1), s)
            end
        else
            if has_closer
                add_node!(t, n, s, join_lines = true)
            else
                padding = is_block(n) ? 0 : -1
                add_node!(t, n, s, join_lines = true, max_padding = padding)
            end
        end
    end
    # move placement of @ to the end
    #
    # @Module.macro -> Module.@macro
    t[1] = move_at_sign_to_the_end(t[1], s)
    t
end
p_macrocall(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_macrocall(DefaultStyle(style), cst, s)

# Block
# length Block is the length of the longest expr
function p_block(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    ignore_single_line = false,
    from_quote = false,
    join_body = false,
)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    single_line =
        ignore_single_line ? false : on_same_line(s, s.offset, s.offset + cst.span - 1)

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if from_quote && !single_line
            if i == 1 || CSTParser.is_comma(a)
                add_node!(t, n, s, join_lines = true)
            elseif CSTParser.is_comma(cst[i-1])
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, Semicolon(), s)
                add_node!(t, n, s, max_padding = 0)
            end
        elseif single_line
            if i == 1 || CSTParser.is_comma(a)
                add_node!(t, n, s, join_lines = true)
            elseif CSTParser.is_comma(cst[i-1])
                add_node!(t, Placeholder(1), s)
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, Semicolon(), s)
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            end
        else
            if i < length(cst) && CSTParser.is_comma(a) && is_punc(cst[i+1])
                add_node!(t, n, s, join_lines = true)
            elseif CSTParser.is_comma(a) && i != length(cst)
                add_node!(t, n, s, join_lines = true)
                join_body && add_node!(t, Placeholder(1), s)
            elseif join_body
                add_node!(t, n, s, join_lines = true)
            else
                add_node!(t, n, s, max_padding = 0)
            end
        end
    end
    t
end
p_block(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    ignore_single_line = false,
    from_quote = false,
    join_body = false,
) where {S<:AbstractStyle} = p_block(
    DefaultStyle(style),
    cst,
    s,
    ignore_single_line = ignore_single_line,
    from_quote = from_quote,
    join_body = join_body,
)

# Abstract
function p_abstract(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    t
end
p_abstract(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_abstract(DefaultStyle(style), cst, s)

# Primitive
function p_primitive(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[5], s), s, join_lines = true)
    t
end
p_primitive(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_primitive(DefaultStyle(style), cst, s)

# FunctionDef/Macro
function p_functiondef(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if length(cst) > 3
        if cst[3].fullspan == 0
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
        else
            s.indent += s.opts.indent
            n = pretty(style, cst[3], s, ignore_single_line = true)
            s.opts.always_use_return && prepend_return!(n, s)
            add_node!(t, n, s, max_padding = s.opts.indent)
            s.indent -= s.opts.indent
            add_node!(t, pretty(style, cst[4], s), s)
        end
    else
        # function stub, i.e. "function foo end"
        # this should be on one line
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    end
    t
end
p_functiondef(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_functiondef(DefaultStyle(style), cst, s)

@inline p_macro(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_functiondef(ds, cst, s)
p_macro(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_macro(DefaultStyle(style), cst, s)

# Struct
function p_struct(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if cst[3].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    else
        s.indent += s.opts.indent
        n = pretty(style, cst[3], s, ignore_single_line = true)
        if s.opts.annotate_untyped_fields_with_any
            annotate_typefields_with_any!(n, s)
        end
        add_node!(t, n, s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
        add_node!(t, pretty(style, cst[4], s), s)
    end
    t
end
p_struct(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_struct(DefaultStyle(style), cst, s)

# Mutable
function p_mutable(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    if cst[4].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[5], s), s, join_lines = true)
    else
        s.indent += s.opts.indent
        n = pretty(style, cst[4], s, ignore_single_line = true)
        if s.opts.annotate_untyped_fields_with_any
            annotate_typefields_with_any!(n, s)
        end
        add_node!(t, n, s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
        add_node!(t, pretty(style, cst[5], s), s)
    end
    t
end
p_mutable(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_mutable(DefaultStyle(style), cst, s)

# ModuleH/BareModule
function p_module(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if cst[3].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    else
        add_node!(t, pretty(style, cst[3], s), s, max_padding = 0)
        add_node!(t, pretty(style, cst[4], s), s)
    end
    t
end
p_module(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_module(DefaultStyle(style), cst, s)

@inline p_baremodule(style::DefaultStyle, cst::CSTParser.EXPR, s::State) =
    p_module(style, cst, s)
p_baremodule(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_baremodule(DefaultStyle(style), cst, s)

# Const/Local/Global/Return/Outer
function p_const(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if cst[2].fullspan != 0
        for a in cst.args[2:end]
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_const(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_const(DefaultStyle(style), cst, s)

@inline p_local(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_const(ds, cst, s)
p_local(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_local(DefaultStyle(style), cst, s)

@inline p_global(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_const(ds, cst, s)
p_global(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_global(DefaultStyle(style), cst, s)

@inline p_return(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_const(ds, cst, s)
p_return(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_return(DefaultStyle(style), cst, s)

@inline p_outer(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_const(ds, cst, s)
p_outer(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_outer(DefaultStyle(style), cst, s)

# TopLevel
function p_toplevel(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    for a in cst.args
        if a.kind === Tokens.NOTHING
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, max_padding = s.opts.indent)
        add_node!(t, Semicolon(), s)
    end
    t
end
p_toplevel(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_toplevel(DefaultStyle(style), cst, s)

# Begin
function p_begin(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if cst[2].fullspan == 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    else
        s.indent += s.opts.indent
        add_node!(
            t,
            pretty(style, cst[2], s, ignore_single_line = true),
            s,
            max_padding = s.opts.indent,
        )
        s.indent -= s.opts.indent
        add_node!(t, pretty(style, cst[3], s), s)
    end
    t
end
p_begin(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_begin(DefaultStyle(style), cst, s)

# Quote
function p_quote(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    if cst[1].kind === Tokens.QUOTE
        add_node!(t, pretty(style, cst[1], s), s)
        if cst[2].fullspan == 0
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
        else
            s.indent += s.opts.indent
            add_node!(
                t,
                pretty(style, cst[2], s, ignore_single_line = true),
                s,
                max_padding = s.opts.indent,
            )
            s.indent -= s.opts.indent
            add_node!(t, pretty(style, cst[3], s), s)
        end
    else
        for a in cst.args
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_quote(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_quote(DefaultStyle(style), cst, s)

# Let
#
# two forms:
#
# let var1 = value1, var2
#     body
# end
#
# y, back = let
#     body
# end
function p_let(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if length(cst.args) > 3
        add_node!(t, Whitespace(1), s)
        s.indent += s.opts.indent
        if cst[2].typ === CSTParser.Block
            add_node!(t, pretty(style, cst[2], s, join_body = true), s, join_lines = true)
        else
            add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
        end
        s.indent -= s.opts.indent

        idx = length(t.nodes)
        s.indent += s.opts.indent
        add_node!(
            t,
            pretty(style, cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.opts.indent,
        )
        s.indent -= s.opts.indent
        # Possible newline after args if nested to act as a separator
        # to the block body.
        if cst[2].typ === CSTParser.Block && t.nodes[end-2].typ !== NOTCODE
            add_node!(t.nodes[idx], Placeholder(0), s)
        end
        add_node!(t, pretty(style, cst.args[end], s), s)
    else
        s.indent += s.opts.indent
        add_node!(t, pretty(style, cst[2], s, ignore_single_line = true), s)
        s.indent -= s.opts.indent
        add_node!(t, pretty(style, cst.args[end], s), s)
    end
    t
end
p_let(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_let(DefaultStyle(style), cst, s)

# Transforms
#
# for i = iter body end
#
# =>
#
# for i in iter body end
#
# AND
#
# for i in 1:10 body end
#
# =>
#
# for i = 1:10 body end
#
# https://github.com/domluna/JuliaFormatter.jl/issues/34
function eq_to_in_normalization!(fst::FST, always_for_in::Bool, for_in_replacement::String)
    if fst.typ === CSTParser.BinaryOpCall
        idx = findfirst(n -> n.typ === CSTParser.OPERATOR, fst.nodes)
        idx === nothing && return
        op = fst[idx]

        if always_for_in && valid_for_in_op(op.val)
            op.val = for_in_replacement
            op.len = length(op.val)
            return
        end

        if op.val == "=" && !is_colon_op(fst[end])
            op.val = "in"
            op.len = length(op.val)
        elseif op.val == "in" && is_colon_op(fst[end])
            op.val = "="
            op.len = length(op.val)
        end
    elseif !is_leaf(fst)
        for n in fst.nodes
            eq_to_in_normalization!(n, always_for_in, for_in_replacement)
        end
    end
end

# For/While
function p_for(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    n = if cst[2].typ === CSTParser.Block
        s.indent += s.opts.indent
        n = pretty(style, cst[2], s, join_body = true)
        s.indent -= s.opts.indent
        n
    else
        pretty(style, cst[2], s)
    end

    cst[1].kind === Tokens.FOR &&
        eq_to_in_normalization!(n, s.opts.always_for_in, s.opts.for_in_replacement)
    add_node!(t, n, s, join_lines = true)

    idx = length(t.nodes)
    s.indent += s.opts.indent
    add_node!(
        t,
        pretty(style, cst[3], s, ignore_single_line = true),
        s,
        max_padding = s.opts.indent,
    )
    s.indent -= s.opts.indent

    # Possible newline after args if nested to act as a separator
    # to the block body.
    if cst[2].typ === CSTParser.Block && t.nodes[end-2].typ !== NOTCODE
        add_node!(t.nodes[idx], Placeholder(0), s)
    end
    add_node!(t, pretty(style, cst[4], s), s)
    t
end
p_for(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_for(DefaultStyle(style), cst, s)

@inline p_while(style::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_for(style, cst, s)
p_while(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_while(DefaultStyle(style), cst, s)

# Do
function p_do(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if cst[3].fullspan != 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    end
    if cst[4].typ === CSTParser.Block
        s.indent += s.opts.indent
        n = pretty(style, cst[4], s, ignore_single_line = true)
        s.opts.always_use_return && prepend_return!(n, s)
        add_node!(t, n, s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
    end
    add_node!(t, pretty(style, cst.args[end], s), s)
    t
end
p_do(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_do(DefaultStyle(style), cst, s)

# Try
function p_try(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    for a in cst.args
        if a.fullspan == 0 && a.typ !== CSTParser.Block
        elseif a.typ === CSTParser.KEYWORD
            add_node!(t, pretty(style, a, s), s, max_padding = 0)
        elseif a.typ === CSTParser.Block
            s.indent += s.opts.indent
            add_node!(
                t,
                pretty(style, a, s, ignore_single_line = true),
                s,
                max_padding = s.opts.indent,
            )
            s.indent -= s.opts.indent
        else
            len = length(t)
            add_node!(t, Whitespace(1), s)
            n = pretty(style, a, s)
            # "catch n"
            t.len = max(len, 5 + 1 + length(n))
            add_node!(t, n, s, join_lines = true, max_padding = 0)
        end
    end
    t
end
p_try(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_try(DefaultStyle(style), cst, s)

# If
function p_if(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    if cst[1].typ === CSTParser.KEYWORD && cst[1].kind === Tokens.IF
        add_node!(t, pretty(style, cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
        s.indent += s.opts.indent
        add_node!(
            t,
            pretty(style, cst[3], s, ignore_single_line = true),
            s,
            max_padding = s.opts.indent,
        )
        s.indent -= s.opts.indent

        len = length(t)
        if length(cst.args) > 4
            add_node!(t, pretty(style, cst[4], s), s, max_padding = 0)
            if cst[4].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1), s)
                n = pretty(style, cst[5], s)
                add_node!(t, n, s, join_lines = true)
                # "elseif n"
                t.len = max(len, length(n))
            else
                # ELSE KEYWORD
                s.indent += s.opts.indent
                add_node!(
                    t,
                    pretty(style, cst[5], s, ignore_single_line = true),
                    s,
                    max_padding = s.opts.indent,
                )
                s.indent -= s.opts.indent
            end
        end
        # END KEYWORD
        add_node!(t, pretty(style, cst.args[end], s), s)
    else
        # "cond" part of "elseif cond"
        t.len += 7
        add_node!(t, pretty(style, cst[1], s), s)

        s.indent += s.opts.indent
        add_node!(
            t,
            pretty(style, cst[2], s, ignore_single_line = true),
            s,
            max_padding = s.opts.indent,
        )
        s.indent -= s.opts.indent

        len = length(t)
        if length(cst.args) > 2
            # this either else or elseif keyword
            add_node!(t, pretty(style, cst[3], s), s, max_padding = 0)

            if cst[3].kind === Tokens.ELSEIF
                add_node!(t, Whitespace(1), s)
                n = pretty(style, cst[4], s)
                add_node!(t, n, s, join_lines = true)
                # "elseif n"
                t.len = max(len, length(n))
            else
                s.indent += s.opts.indent
                add_node!(
                    t,
                    pretty(style, cst[4], s, ignore_single_line = true),
                    s,
                    max_padding = s.opts.indent,
                )
                s.indent -= s.opts.indent
            end
        end
    end
    t
end
p_if(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_if(DefaultStyle(style), cst, s)

# ChainOpCall/Comparison
function p_chainopcall(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    # Check if there's a number literal on the LHS of a dot operator.
    # In this case we need to surround the dot operator with whitespace
    # in order to avoid ambiguity.
    for (i, a) in enumerate(cst)
        if a.typ === CSTParser.OPERATOR && a.dot && is_number(cst[i-1])
            nospace = false
            break
        end
    end

    nws = nospace ? 0 : 1
    for (i, a) in enumerate(cst)
        if a.typ === CSTParser.OPERATOR
            add_node!(t, Whitespace(nws), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            if nonest
                add_node!(t, Whitespace(nws), s)
            else
                add_node!(t, Placeholder(nws), s)
            end
        elseif is_opcall(a)
            add_node!(
                t,
                pretty(style, a, s, nospace = nospace, nonest = nonest),
                s,
                join_lines = true,
            )
        elseif i == length(cst) - 1 && is_punc(a) && is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_chainopcall(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
) where {S<:AbstractStyle} =
    p_chainopcall(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

@inline p_comparison(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
) = p_chainopcall(ds, cst, s, nonest = nonest, nospace = nospace)
p_comparison(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
) where {S<:AbstractStyle} =
    p_comparison(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

# ColonOpCall
function p_colonopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices
    for a in cst
        if is_opcall(a)
            n = pretty(style, a, s, nonest = true, nospace = nospace)
        else
            n = pretty(style, a, s)
        end

        if s.opts.whitespace_ops_in_indices && !is_leaf(n) && !is_iterable(n)
            paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, "(")
            add_node!(t, paren, s, join_lines = true)
            add_node!(t, n, s, join_lines = true)
            paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, ")")
            add_node!(t, paren, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_colonopcall(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_colonopcall(DefaultStyle(style), cst, s)

# Kw
function p_kw(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    exclamation = cst[1].typ === CSTParser.IDENTIFIER && endswith(cst[1].val, "!")
    opcall = (cst[3].typ === CSTParser.Call && cst[3][1].typ === CSTParser.OPERATOR)

    if !s.opts.whitespace_in_kwargs && exclamation
        n = pretty(style, cst[1], s)
        add_node!(
            t,
            FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, "("),
            s,
            join_lines = true,
        )
        add_node!(t, n, s, join_lines = true)
        add_node!(
            t,
            FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, ")"),
            s,
            join_lines = true,
        )
    else
        add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    end

    if s.opts.whitespace_in_kwargs
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
        add_node!(t, Whitespace(1), s)
    else
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    end

    if !s.opts.whitespace_in_kwargs && opcall
        n = pretty(style, cst[3], s)
        add_node!(
            t,
            FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, "("),
            s,
            join_lines = true,
        )
        add_node!(t, n, s, join_lines = true)
        add_node!(
            t,
            FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, ")"),
            s,
            join_lines = true,
        )
    else
        add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    end

    t
end
p_kw(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_kw(DefaultStyle(style), cst, s)

# BinaryOpCall
function p_binaryopcall(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    op = cst[2]
    nonest = nonest || op.kind === Tokens.COLON
    if cst.parent.typ === CSTParser.Curly &&
       op.kind in (Tokens.ISSUBTYPE, Tokens.ISSUPERTYPE) &&
       !s.opts.whitespace_typedefs
        nospace = true
    elseif op.kind === Tokens.COLON
        nospace = true
    end
    nospace_args = s.opts.whitespace_ops_in_indices ? false : nospace

    if is_opcall(cst[1])
        n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[1], s)
    end

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[1]) &&
       !is_iterable(cst[1])
        paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, "(")
        add_node!(t, paren, s)
        add_node!(t, n, s, join_lines = true)
        paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s)
    end

    nrhs = nest_rhs(cst)
    nrhs && (t.nest_behavior = AlwaysNest)
    nest = (nestable(style, cst) && !nonest) || nrhs

    if op.fullspan == 0
        # Do nothing - represents a binary op with no textual representation.
        # For example: `2a`, which is equivalent to `2 * a`.
    elseif op.kind === Tokens.EX_OR
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif (is_number(cst[1]) || op.kind === Tokens.CIRCUMFLEX_ACCENT) && op.dot
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif op.kind !== Tokens.IN && (
        nospace || (
            op.kind !== Tokens.ANON_FUNC && CSTParser.precedence(op) in (
                CSTParser.ColonOp,
                CSTParser.PowerOp,
                CSTParser.DeclarationOp,
                CSTParser.DotOp,
            )
        )
    )
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    else
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    end

    if is_opcall(cst[3])
        n = pretty(style, cst[3], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[3], s)
    end

    if op.kind === Tokens.COLON &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[3]) &&
       !is_iterable(cst[3])
        paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(t, n, s, join_lines = true)
        paren = FST(CSTParser.PUNCTUATION, -1, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s, join_lines = true)
    end

    if nest
        # for indent, will be converted to `indent` if needed
        insert!(t.nodes, length(t.nodes), Placeholder(0))
    end

    t
end
p_binaryopcall(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
) where {S<:AbstractStyle} =
    p_binaryopcall(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

# WhereOpCall
# A where B
function p_whereopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)

    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    args = get_args(cst.args[3:end])
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))
    add_braces =
        !CSTParser.is_lbrace(cst[3]) &&
        cst.parent.typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.Curly &&
        cst[3].typ !== CSTParser.BracesCat

    brace = FST(CSTParser.PUNCTUATION, -1, t.endline, t.endline, "{")
    add_braces && add_node!(t, brace, s, join_lines = true)

    nws = s.opts.whitespace_typedefs ? 1 : 0
    for (i, a) in enumerate(cst.args[3:end])
        if is_opener(a) && nest
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
            s.indent += s.opts.indent
        elseif is_closer(a) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            s.indent -= s.opts.indent
        elseif CSTParser.is_comma(a) && !is_punc(cst[i+3])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
        elseif a.typ === CSTParser.BinaryOpCall
            add_node!(
                t,
                pretty(style, a, s, nospace = !s.opts.whitespace_typedefs),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    brace = FST(CSTParser.PUNCTUATION, -1, t.endline, t.endline, "}")
    add_braces && add_node!(t, brace, s, join_lines = true)
    t
end
p_whereopcall(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_whereopcall(DefaultStyle(style), cst, s)

# Conditional
function p_conditionalopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    add_node!(t, pretty(style, cst[1], s), s)

    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true) # '?'
    add_node!(t, Placeholder(1), s)

    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)

    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true) # ':'
    add_node!(t, Placeholder(1), s)

    add_node!(t, pretty(style, cst[5], s), s, join_lines = true)

    t
end
p_conditionalopcall(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_conditionalopcall(DefaultStyle(style), cst, s)

# UnaryOpCall
function p_unaryopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; nospace = true)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    !nospace && add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    t
end
p_unaryopcall(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    nospace = true,
) where {S<:AbstractStyle} = p_unaryopcall(DefaultStyle(style), cst, s, nospace = nospace)

function p_curly(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))

    nws = s.opts.whitespace_typedefs ? 1 : 0
    if nest
        add_node!(t, Placeholder(0), s)
    end

    for (i, a) in enumerate(cst.args[3:end])
        if i + 2 == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(nws), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_curly(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_curly(DefaultStyle(style), cst, s)

function p_call(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))

    if nest
        add_node!(t, Placeholder(0), s)
    end

    for (i, a) in enumerate(cst.args[3:end])
        if i + 2 == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) - 3 && !is_punc(cst[i+3])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_call(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_call(DefaultStyle(style), cst, s)

# InvisBrackets
function p_invisbrackets(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nest = !is_iterable(cst[2]) && !nonest

    if is_block(cst[2]) || (cst[2].typ === CSTParser.Generator && is_block(cst[2][1]))
        t.nest_behavior = AlwaysNest
    end

    for (i, a) in enumerate(cst)
        if a.typ === CSTParser.Block
            add_node!(t, pretty(style, a, s, from_quote = true), s, join_lines = true)
        elseif is_opcall(a)
            n = pretty(style, a, s, nonest = nonest, nospace = nospace)
            add_node!(t, n, s, join_lines = true)
        elseif is_opener(a) && nest
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(a) && nest
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_invisbrackets(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
) where {S<:AbstractStyle} =
    p_invisbrackets(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

# TupleH
function p_tupleh(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))

    for (i, a) in enumerate(cst)
        n = if a.typ === CSTParser.BinaryOpCall && a[2].kind === Tokens.EQ
            p_kw(style, a, s)
        else
            pretty(style, a, s)
        end

        if is_opener(n) && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(n) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_tupleh(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_tupleh(DefaultStyle(style), cst, s)

# Braces
function p_braces(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_node(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if i == 1 && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif i == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_braces(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_braces(DefaultStyle(style), cst, s)

# BracesCat
function p_bracescat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_node(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if i == 1 && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif i == length(cst) && nest
            add_node!(t, TrailingSemicolon(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
            if i != length(cst) - 1
                add_node!(t, Semicolon(), s)
                add_node!(t, Placeholder(1), s)
            end
        end
    end
    t
end
p_bracescat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_bracescat(DefaultStyle(style), cst, s)

# Vect
function p_vect(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 2 && !(length(cst) == 3 && unnestable_node(cst[2]))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if i == 1 && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif i == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_vect(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_vect(DefaultStyle(style), cst, s)

# Comprehension
function p_comprehension(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    if is_block(cst[2]) || (cst[2].typ === CSTParser.Generator && is_block(cst[2][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    add_node!(t, Placeholder(0), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Placeholder(0), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    t
end
p_comprehension(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_comprehension(DefaultStyle(style), cst, s)

# TypedComprehension
function p_typedcomprehension(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    if is_block(cst[3]) || (cst[3].typ === CSTParser.Generator && is_block(cst[3][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Placeholder(0), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    add_node!(t, Placeholder(0), s)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    t
end
p_typedcomprehension(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_typedcomprehension(DefaultStyle(style), cst, s)

# Parameters
function p_parameters(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if i == length(cst) && CSTParser.is_comma(a)
            # do nothing
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            push!(t.nodes, n)
            push!(t.nodes, Placeholder(1))
        else
            push!(t.nodes, n)
        end
    end
    t
end
p_parameters(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_parameters(DefaultStyle(style), cst, s)

# Import, Export, Using
function p_import(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for (i, a) in enumerate(cst.args[2:end])
        if CSTParser.is_comma(a) || CSTParser.is_colon(a)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_import(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_import(DefaultStyle(style), cst, s)

@inline p_export(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_import(ds, cst, s)
p_export(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_export(DefaultStyle(style), cst, s)

@inline p_using(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_import(ds, cst, s)
p_using(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_using(DefaultStyle(style), cst, s)

# Ref
function p_ref(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    nest = length(cst) > 5 && !(length(cst) == 5 && unnestable_node(cst[3]))
    nospace = !s.opts.whitespace_ops_in_indices

    for (i, a) in enumerate(cst)
        if is_closer(a) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif is_opener(a) && nest
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif is_opcall(a)
            n = pretty(style, a, s, nonest = true, nospace = nospace)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_ref(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_ref(DefaultStyle(style), cst, s)

# Vcat/TypedVcat
function p_vcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    st = cst.typ === CSTParser.Vcat ? 1 : 2
    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        diff_line = t.endline != t.startline
        if is_opener(a) && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif !is_closer(a) && i > st
            add_node!(t, n, s, join_lines = true)
            if i != length(cst) - 1
                has_semicolon(s.doc, n.startline) &&
                    add_node!(t, InverseTrailingSemicolon(), s)
                add_node!(t, Placeholder(1), s)
                # Keep trailing semicolon if there's only one arg
            elseif n_args(cst) == 1
                add_node!(t, Semicolon(), s)
                add_node!(t, Placeholder(0), s)
            else
                add_node!(t, Placeholder(0), s)
            end
        else
            # If arguments are on different always nest
            diff_line && (t.nest_behavior = AlwaysNest)
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_vcat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_vcat(DefaultStyle(style), cst, s)

@inline p_typedvcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_vcat(ds, cst, s)
p_typedvcat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_typedvcat(DefaultStyle(style), cst, s)

# Hcat/TypedHcat
function p_hcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    st = cst.typ === CSTParser.Hcat ? 1 : 2
    for (i, a) in enumerate(cst)
        if i > st && i < length(cst) - 1
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_hcat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_hcat(DefaultStyle(style), cst, s)

@inline p_typedhcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_hcat(ds, cst, s)
p_typedhcat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_typedhcat(DefaultStyle(style), cst, s)

# Row
function p_row(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))

    for (i, a) in enumerate(cst)
        if is_opcall(a)
            add_node!(
                t,
                pretty(style, a, s, nospace = true, nonest = true),
                s,
                join_lines = true,
            )
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
        i < length(cst) && add_node!(t, Whitespace(1), s)
    end
    t.nest_behavior = NeverNest
    t
end
p_row(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_row(DefaultStyle(style), cst, s)

# Generator/Filter/Flatten
function p_generator(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(cst, nspaces(s))
    has_for_kw = false
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if a.typ === CSTParser.KEYWORD
            if a.kind === Tokens.FOR
                has_for_kw = true
            end

            # for keyword can only be on the following line
            # if this expression is within an iterable expression
            if a.kind === Tokens.FOR && parent_is(
                a,
                is_iterable,
                ignore = n -> is_gen(n) || n.typ === CSTParser.InvisBrackets,
            )
                add_node!(t, Placeholder(1), s)
            else
                add_node!(t, Whitespace(1), s)
            end

            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, n, s, join_lines = true)
        end

        has_for_kw &&
            eq_to_in_normalization!(n, s.opts.always_for_in, s.opts.for_in_replacement)
    end
    t
end
p_generator(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_generator(DefaultStyle(style), cst, s)

@inline p_filter(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_generator(ds, cst, s)
p_filter(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_filter(DefaultStyle(style), cst, s)

@inline p_flatten(ds::DefaultStyle, cst::CSTParser.EXPR, s::State) = p_generator(ds, cst, s)
p_flatten(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_flatten(DefaultStyle(style), cst, s)
