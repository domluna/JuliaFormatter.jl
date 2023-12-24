# Creates a _prettified_ version of a CST.
function pretty(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; kwargs...)
    style = getstyle(ds)
    if cst.head === :NONSTDIDENTIFIER
        return p_nonstdidentifier(style, cst, s)
    elseif CSTParser.isidentifier(cst)
        return p_identifier(style, cst, s)
    elseif CSTParser.isoperator(cst)
        return p_operator(style, cst, s)
    elseif CSTParser.ispunctuation(cst)
        return p_punctuation(style, cst, s)
    elseif CSTParser.iskeyword(cst)
        return p_keyword(style, cst, s)
    elseif cst.head === :as
        return p_as(style, cst, s)
    elseif cst.head === :string
        return p_stringh(style, cst, s)
    elseif CSTParser.isliteral(cst)
        return p_literal(style, cst, s)
    elseif cst.head === :block && length(cst) > 1 && cst[1].head == :BEGIN
        return p_begin(style, cst, s)
    elseif cst.head === :block
        return p_block(style, cst, s; kwargs...)
    elseif cst.head === :module
        return p_module(style, cst, s)
    elseif cst.head === :baremodule
        return p_baremodule(style, cst, s)
    elseif cst.head === :function
        return p_functiondef(style, cst, s)
    elseif cst.head === :macro
        return p_macro(style, cst, s)
    elseif cst.head === :struct && cst[1].head === :STRUCT
        return p_struct(style, cst, s)
    elseif cst.head === :struct && cst[1].head === :MUTABLE
        return p_mutable(style, cst, s)
    elseif cst.head === :abstract
        return p_abstract(style, cst, s)
    elseif cst.head === :primitive
        return p_primitive(style, cst, s)
    elseif cst.head === :for
        return p_for(style, cst, s)
    elseif cst.head === :while
        return p_while(style, cst, s)
    elseif cst.head === :do
        return p_do(style, cst, s)
    elseif is_if(cst)
        return p_if(style, cst, s)
    elseif cst.head === :try
        return p_try(style, cst, s)
    elseif cst.head === :toplevel
        return p_toplevel(style, cst, s)
    elseif cst.head === :quote
        return p_quote(style, cst, s)
    elseif cst.head === :let
        return p_let(style, cst, s)
    elseif cst.head === :vect
        return p_vect(style, cst, s)
    elseif cst.head === :comprehension
        return p_comprehension(style, cst, s)
    elseif cst.head === :typed_comprehension
        return p_typedcomprehension(style, cst, s)
    elseif cst.head === :braces
        return p_braces(style, cst, s)
    elseif cst.head === :bracescat
        return p_bracescat(style, cst, s)
    elseif cst.head === :tuple
        return p_tuple(style, cst, s)
    elseif cst.head === :brackets
        return p_invisbrackets(style, cst, s; kwargs...)
    elseif cst.head === :curly
        return p_curly(style, cst, s)
    elseif is_macrostr(cst)
        return p_macrostr(style, cst, s)
    elseif cst.head === :macrocall && cst[1].head == :globalrefdoc
        return p_globalrefdoc(style, cst, s)
    elseif cst.head === :macrocall && cst[1].head == :globalrefcmd
        return p_globalrefcmd(style, cst, s)
    elseif cst.head === :macrocall && is_macrodoc(cst)
        return p_macrodoc(style, cst, s)
    elseif cst.head === :macrocall
        return p_macrocall(style, cst, s)
    elseif CSTParser.ismacroname(cst)
        return p_macroname(style, cst, s)
    elseif cst.head === :where
        return p_whereopcall(style, cst, s)
    elseif is_colon_call(cst)
        return p_colonopcall(style, cst, s)
    elseif CSTParser.isconditional(cst)
        return p_conditionalopcall(style, cst, s)
    elseif is_binary(cst)
        return p_binaryopcall(style, cst, s; kwargs...)
    elseif is_unary(cst)
        return p_unaryopcall(style, cst, s; kwargs...)
    elseif is_chain(cst)
        return p_chainopcall(style, cst, s; kwargs...)
    elseif is_call(cst)
        return p_call(style, cst, s)
    elseif cst.head === :comparison
        return p_comparison(style, cst, s; kwargs...)
    elseif cst.head === :kw
        return p_kw(style, cst, s)
    elseif cst.head === :parameters
        return p_parameters(style, cst, s)
    elseif cst.head === :local
        return p_local(style, cst, s)
    elseif cst.head === :global
        return p_global(style, cst, s)
    elseif cst.head === :const
        return p_const(style, cst, s)
    elseif cst.head === :return
        return p_return(style, cst, s)
    elseif cst.head === :outer
        return p_outer(style, cst, s)
    elseif cst.head === :import
        return p_import(style, cst, s)
    elseif cst.head === :export
        return p_export(style, cst, s)
    elseif cst.head === :using
        return p_using(style, cst, s)
    elseif cst.head === :row
        return p_row(style, cst, s)
    elseif cst.head === :nrow
        return p_nrow(style, cst, s)
    elseif cst.head === :ncat
        return p_ncat(style, cst, s)
    elseif cst.head === :typed_ncat
        return p_typedncat(style, cst, s)
    elseif cst.head === :vcat
        return p_vcat(style, cst, s)
    elseif cst.head === :typed_vcat
        return p_typedvcat(style, cst, s)
    elseif cst.head === :hcat
        return p_hcat(style, cst, s)
    elseif cst.head === :typed_hcat
        return p_typedhcat(style, cst, s)
    elseif cst.head === :ref
        return p_ref(style, cst, s)
    elseif cst.head === :generator
        return p_generator(style, cst, s)
    elseif cst.head === :filter
        return p_filter(style, cst, s)
    elseif cst.head === :flatten
        return p_flatten(style, cst, s)
    elseif cst.head === :file
        return p_file(style, cst, s)
    elseif cst.head === :quotenode
        return p_quotenode(style, cst, s)
    end

    t = FST(Unknown, cst, nspaces(s))
    for a in cst
        if CSTParser.is_nothing(a)
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, join_lines = true)
    end
    t
end
pretty(style::S, cst::CSTParser.EXPR, s::State; kwargs...) where {S<:AbstractStyle} =
    pretty(DefaultStyle(style), cst, s; kwargs...)

function p_file(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(File, cst, nspaces(s))
    for a in cst
        if CSTParser.is_nothing(a)
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, join_lines = false, max_padding = 0)
    end
    t
end
p_file(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_file(DefaultStyle(style), cst, s)

@inline function p_nonstdidentifier(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(NonStdIdentifier, cst, nspaces(s))
    for a in cst.args::Vector{CSTParser.EXPR}
        add_node!(t, pretty(style, a, s), s, join_lines = true)
    end
    t
end
p_nonstdidentifier(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_nonstdidentifier(DefaultStyle(style), cst, s)

@inline function p_identifier(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    s.offset += length(cst.val::AbstractString) + (cst.fullspan - cst.span)
    FST(IDENTIFIER, loc[2], loc[1], loc[1], cst.val::AbstractString)
end
p_identifier(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_identifier(DefaultStyle(style), cst, s)

@inline function p_operator(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    s.offset += length(cst.val::AbstractString) + (cst.fullspan - cst.span)

    t = FST(OPERATOR, loc[2], loc[1], loc[1], cst.val::AbstractString)
    t.metadata = Metadata(tokenize(cst.val::AbstractString), CSTParser.isdotted(cst))
    return t
end
p_operator(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_operator(DefaultStyle(style), cst, s)

@inline function p_keyword(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    s.offset += cst.fullspan
    FST(KEYWORD, loc[2], loc[1], loc[1], cst.val::AbstractString)
end
p_keyword(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_keyword(DefaultStyle(style), cst, s)

@inline function p_punctuation(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    s.offset += cst.fullspan
    val = if cst.val === nothing && cst.head === :DOT
        "."
    else
        cst.val
    end
    FST(PUNCTUATION, loc[2], loc[1], loc[1], val::AbstractString)
end
p_punctuation(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_punctuation(DefaultStyle(style), cst, s)

struct FormatRule{T<:AbstractStyle}
    style::T
    opts::Options
end
format_text(text::AbstractString, fr::FormatRule) = format_text(text, fr.style, fr.opts)

function block_modifier(rule::FormatRule)
    Rule(1) do parser, block
        block.t isa CodeBlock || return
        language = block.t.info
        code = block.literal

        if startswith(language, r"@example|@repl|@eval|julia|{julia}|jldoctest")
            block.literal = if occursin(r"^julia> "m, code)
                doctests = IOBuffer()
                chunks = repl_splitter(code)
                for (i, (an_input, output)) in enumerate(chunks)
                    write(doctests, "julia> ")
                    for (j, line) in enumerate(split(format_text(an_input, rule), '\n'))
                        if j > 1
                            if line == ""
                                write(doctests, "\n")
                            else
                                write(doctests, "\n       ")
                            end
                        end
                        write(doctests, line)
                    end
                    write(doctests, '\n')
                    write(doctests, output)

                    if i < length(chunks)
                        if output == ""
                            write(doctests, "\n")
                        else
                            write(doctests, "\n\n")
                        end
                    end
                end
                write(doctests, '\n')
                String(take!(doctests))
            elseif occursin(r"\n+# output\n+", code)
                an_input, output = split(code, r"\n+# output\n+", limit = 2)
                string(
                    format_text(format_text(String(an_input), rule), rule),
                    "\n\n# output\n\n",
                    output,
                )
            else
                format_text(code, rule)
            end
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
                FrontMatterRule(),
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
        # don't write empty lines #667
        if !all(isspace, line)
            write(buf, indent)
            write(buf, line)
        end
    end
    write(buf, indent)
    write(buf, "\"\"\"")
    String(take!(buf))
end

function p_literal(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; from_docstring = false)
    loc = cursor_loc(s)
    if !is_str_or_cmd(cst)
        (val = cst.val) === nothing && return FST(LITERAL, loc[2], loc[1], loc[1], "")

        if cst.head === :FLOAT
            if (fidx = findlast(==('f'), val)) === nothing
                float_suffix = ""
            else
                float_suffix = val[fidx:end]
                val = val[1:fidx-1]
            end
            if findfirst(c -> c == 'e' || c == 'E', val) === nothing
                if (dotidx = findlast(==('.'), val)) === nothing
                    val *= s.opts.trailing_zero ? ".0" : ""  # append a trailing zero prior to the suffix
                elseif dotidx == length(val)
                    val *= s.opts.trailing_zero ? "0" : ""  # if a float literal ends in `.`, add trailing zero.
                elseif dotidx == 1
                    val = '0' * val  # leading zero
                elseif dotidx == 2 && (val[1] == '-' || val[1] == '+')
                    val = val[1] * '0' * val[2:end]  # leading zero on signed numbers
                end
            end
            val *= float_suffix
        end

        s.offset += length(cst.val::AbstractString) + (cst.fullspan - cst.span)
        return FST(LITERAL, loc[2], loc[1], loc[1], val)
    end

    # Strings are unescaped by CSTParser
    # to mimic Meta.parse which makes reproducing
    # the correct output from the LITERAL value problematic.
    # So we'll just look at the source directly!
    str_info = get(s.doc.lit_strings, s.offset - 1, nothing)

    # Tokenize treats the `ix` part of r"^(=?[^=]+)=(.*)$"ix as an
    # IDENTIFIER where as CSTParser parses it as a LITERAL.
    # An IDENTIFIER won't show up in the string literal lookup table.
    if str_info === nothing
        s.offset += length(cst.val::AbstractString) + (cst.fullspan - cst.span)
        return FST(LITERAL, loc[2], loc[1], loc[1], cst.val::AbstractString)
    end

    startline, endline, str = str_info
    s.offset += length(str) + (cst.fullspan - cst.span)

    if from_docstring && s.opts.format_docstrings
        str = format_docstring(ds, s, str)
    end

    lines = split(str, "\n")

    if length(lines) == 1
        return FST(LITERAL, loc[2], loc[1], loc[1], lines[1])
    end

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    t = FST(
        StringN,
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
        nothing,
    )
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        tt = FST(
            LITERAL,
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
            nothing,
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
        t = FST(LITERAL, loc[2], startline, startline, lines[1])
        return t
    end

    sidx = loc[2]
    for l in lines[2:end]
        fc = findfirst(c -> !isspace(c), l)
        if fc !== nothing
            sidx = min(sidx, fc)
        end
    end

    t = FST(StringN, cst, loc[2] - 1)
    t.line_offset = loc[2]
    for (i, l) in enumerate(lines)
        ln = startline + i - 1
        l = i == 1 ? l : l[sidx:end]
        tt = FST(
            LITERAL,
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
            nothing,
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
    t = FST(GlobalRefDoc, cst, nspaces(s))

    # cst[1] is :globalrefdoc
    # cst[2] is empty and fullspan is 0 so we can skip it
    if CSTParser.isliteral(cst[3])
        add_node!(t, p_literal(style, cst[3], s, from_docstring = true), s, max_padding = 0)
    elseif cst[3].head === :string
        add_node!(t, p_stringh(style, cst[3], s), s)
    end
    add_node!(t, pretty(style, cst[4], s), s, max_padding = 0)
    return t
end
p_globalrefdoc(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_globalrefdoc(DefaultStyle(style), cst, s)

function p_globalrefcmd(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    # cst[1] is :globalrefcmd
    # cst[2] is empty and fullspan is 0 so we can skip it
    return pretty(style, cst[3], s)
end
p_globalrefcmd(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_globalrefcmd(DefaultStyle(style), cst, s)

# @doc "example"
function p_macrodoc(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(GlobalRefDoc, cst, nspaces(s))

    # cst[2] is empty and fullspan is 0 so we can skip it
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)

    if length(cst) > 3
        n = pretty(style, cst[4], s)
        join_lines = t.endline == n.startline
        join_lines && add_node!(t, Whitespace(1), s)
        add_node!(t, n, s, join_lines = join_lines, max_padding = 0)
    end

    return t
end
p_macrodoc(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_macrodoc(DefaultStyle(style), cst, s)

function p_macrostr(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(MacroStr, cst, nspaces(s))

    for (i, a) in enumerate(cst)
        if CSTParser.is_nothing(a)
            s.offset += a.fullspan
            continue
        elseif is_macrostr_identifier(a)
            add_node!(t, p_macrostr_identifier(style, a, s), s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end

        if i > 2 && i < length(cst) && a.fullspan > a.span
            add_node!(t, Whitespace(1), s)
        end
    end

    return t
end
p_macrostr(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_macrostr(DefaultStyle(style), cst, s)

"""
This is a special prettifier to handle the case of string macros. As such it is not
part of [`pretty`](@ref).

```julia
format"hello"
```

The above "format" identifier is parsed by CSTParser as if the text is "@format_str".
This creates problems when we format without intervention:

 1. "@format_str" is printed instead of "format"
 2. The state offset is incorrect since the length of "@format_str" is greater than "format"
"""
function p_macrostr_identifier(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    loc = cursor_loc(s)
    idx = findfirst(==('_'), cst.val)
    val = cst.val::AbstractString
    val = val[2:prevind(val, idx::Int)]
    s.offset += length(val) + (cst.fullspan - cst.span)
    return FST(IDENTIFIER, loc[2], loc[1], loc[1], val)
end
p_macrostr_identifier(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_macrostr_identifier(DefaultStyle(style), cst, s)

# MacroCall
function p_macrocall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(MacroCall, cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))
    has_closer = is_closer(cst[end])

    !has_closer && (t.typ = MacroBlock)

    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(cst)
        if CSTParser.is_nothing(a)
            s.offset += a.fullspan
            continue
        end

        n = pretty(style, a, s)
        if CSTParser.ismacroname(a)
            add_node!(t, n, s, join_lines = true)
            if length(args) > 0
                loc = cursor_loc(s)
                if t[end].line_offset + length(t[end]) < loc[2]
                    add_node!(t, Whitespace(1), s)
                end
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
        elseif t.typ === MacroBlock
            if n.typ === MacroBlock && t[end].typ === WHITESPACE
                t[end] = Placeholder(length(t[end].val))
            end
            if has_closer
                add_node!(t, n, s, join_lines = true)
                if i < length(cst) - 1 && cst[i+1].head != :parameters
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

function p_macroname(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Macroname, cst, nspaces(s))
    for a in cst
        if CSTParser.is_nothing(a)
            s.offset += a.fullspan
            continue
        end
        add_node!(t, pretty(style, a, s), s, join_lines = true)
    end
    t
end
p_macroname(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_macroname(DefaultStyle(style), cst, s)

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
    t = FST(Block, cst, nspaces(s))

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

function p_block(ds::DefaultStyle, nodes::Vector{CSTParser.EXPR}, s::State)
    style = getstyle(ds)
    t = FST(Block, nspaces(s))

    for (i, a) in enumerate(nodes)
        n = pretty(style, a, s)
        if i < length(nodes) && CSTParser.is_comma(a) && is_punc(nodes[i+1])
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) && i != length(nodes)
            add_node!(t, n, s, join_lines = true)
        else
            add_node!(t, n, s, max_padding = 0)
        end
    end

    t
end
p_block(style::S, nodes::Vector{CSTParser.EXPR}, s::State) where {S<:AbstractStyle} =
    p_block(DefaultStyle(style), nodes, s)

# Abstract
function p_abstract(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Abstract, cst, nspaces(s))
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
    t = FST(Primitive, cst, nspaces(s))
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

# function/macro
function p_functiondef(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(FunctionN, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    if length(cst) > 3
        if cst[3].fullspan == 0
            n = pretty(style, cst[4], s)
            if s.opts.join_lines_based_on_source
                join_lines = t.endline == n.startline
                join_lines && (add_node!(t, Whitespace(1), s))
                add_node!(t, n, s, join_lines = join_lines)
            else
                add_node!(t, Whitespace(1), s)
                add_node!(t, n, s, join_lines = true)
            end
        else
            s.indent += s.opts.indent
            n = pretty(style, cst[3], s, ignore_single_line = true)
            s.opts.always_use_return && prepend_return!(n, s)
            add_node!(t, n, s, max_padding = s.opts.indent)
            s.indent -= s.opts.indent
            add_node!(t, pretty(style, cst[4], s), s)
        end
    else
        # function stub `function foo end`
        n = pretty(style, cst[3], s)
        if s.opts.join_lines_based_on_source
            join_lines = t.endline == n.startline
            join_lines && (add_node!(t, Whitespace(1), s))
            add_node!(t, n, s, join_lines = join_lines)
        else
            add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_functiondef(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_functiondef(DefaultStyle(style), cst, s)

function p_macro(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_functiondef(ds, cst, s)
    t.typ = Macro
    t
end
p_macro(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_macro(DefaultStyle(style), cst, s)

# struct
function p_struct(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Struct, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    if cst[4].fullspan == 0
        n = pretty(style, cst[5], s)
        if s.opts.join_lines_based_on_source
            join_lines = t.endline == n.startline
            join_lines && (add_node!(t, Whitespace(1), s))
            add_node!(t, n, s, join_lines = join_lines)
        else
            add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
        end
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
p_struct(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_struct(DefaultStyle(style), cst, s)

# mutable
function p_mutable(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Mutable, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[4], s), s, join_lines = true)
    if cst[5].fullspan == 0
        n = pretty(style, cst[6], s)
        if s.opts.join_lines_based_on_source
            join_lines = t.endline == n.startline
            join_lines && (add_node!(t, Whitespace(1), s))
            add_node!(t, n, s, join_lines = join_lines)
        else
            add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
        end
    else
        s.indent += s.opts.indent
        n = pretty(style, cst[5], s, ignore_single_line = true)
        if s.opts.annotate_untyped_fields_with_any
            annotate_typefields_with_any!(n, s)
        end
        add_node!(t, n, s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
        add_node!(t, pretty(style, cst[6], s), s)
    end
    t
end
p_mutable(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_mutable(DefaultStyle(style), cst, s)

# module/baremodule
function p_module(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(ModuleN, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    if cst[4].fullspan == 0
        n = pretty(style, cst[5], s)
        if s.opts.join_lines_based_on_source
            join_lines = t.endline == n.startline
            join_lines && (add_node!(t, Whitespace(1), s))
            add_node!(t, n, s, join_lines = join_lines)
        else
            add_node!(t, Whitespace(1), s)
            add_node!(t, n, s, join_lines = true)
        end
    else
        if s.opts.indent_submodule && parent_is(
            cst,
            n -> n !== nothing;
            ignore = n -> !(n.head === :module || n.head === :baremodule),
        )
            s.indent += s.opts.indent
            add_node!(t, pretty(style, cst[4], s), s, max_padding = s.opts.indent)
            s.indent -= s.opts.indent
        else
            add_node!(t, pretty(style, cst[4], s), s, max_padding = 0)
        end
        add_node!(t, pretty(style, cst[5], s), s)
    end
    t
end
p_module(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_module(DefaultStyle(style), cst, s)

function p_baremodule(style::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_module(style, cst, s)
    t.typ = BareModule
    t
end
p_baremodule(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_baremodule(DefaultStyle(style), cst, s)

# const/local/global/return/outer
function p_const(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Const, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if cst[2].fullspan != 0
        for i in 2:length(cst)
            a = cst[i]
            if !CSTParser.is_comma(a)
                add_node!(t, Whitespace(1), s)
            end
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_const(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_const(DefaultStyle(style), cst, s)

function p_local(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_const(ds, cst, s)
    t.typ = Local
    t
end
p_local(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_local(DefaultStyle(style), cst, s)

function p_global(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_const(ds, cst, s)
    t.typ = Global
    t
end
p_global(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_global(DefaultStyle(style), cst, s)

function p_return(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_const(ds, cst, s)
    t.typ = Return
    t
end
p_return(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_return(DefaultStyle(style), cst, s)

function p_outer(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_const(ds, cst, s)
    t.typ = Outer
    t
end
p_outer(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_outer(DefaultStyle(style), cst, s)

function p_toplevel(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(TopLevel, cst, nspaces(s))
    for a in cst
        if CSTParser.is_nothing(a)
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

# begin
function p_begin(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Begin, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if length(cst) == 2
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[end], s), s, join_lines = true)
    else
        s.indent += s.opts.indent
        nodes = CSTParser.EXPR[]
        for i in 2:length(cst)-1
            push!(nodes, cst[i])
        end
        add_node!(t, p_block(style, nodes, s), s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
        add_node!(t, pretty(style, cst[end], s), s)
    end
    t
end
p_begin(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_begin(DefaultStyle(style), cst, s)

# quote
function p_quote(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Quote, cst, nspaces(s))
    if cst[1].head === :QUOTE
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
        for a in cst
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_quote(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_quote(DefaultStyle(style), cst, s)

function p_quotenode(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Quotenode, cst, nspaces(s))
    for a in cst
        if CSTParser.is_nothing(a)
            s.offset += a.fullspan
            continue
        elseif is_macrostr_identifier(a)
            add_node!(t, p_macrostr_identifier(style, a, s), s, join_lines = true)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
p_quotenode(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_quotenode(DefaultStyle(style), cst, s)

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
    t = FST(Let, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if cst[2].fullspan == 0
        s.indent += s.opts.indent
        add_node!(t, pretty(style, cst[3], s, ignore_single_line = true), s)
        s.indent -= s.opts.indent
        add_node!(t, pretty(style, cst[end], s), s)
    else
        add_node!(t, Whitespace(1), s)
        s.indent += s.opts.indent
        if cst[2].head === :block
            add_node!(t, p_block(style, cst[2], s, join_body = true), s, join_lines = true)
        else
            add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
        end
        s.indent -= s.opts.indent

        idx = length(t.nodes::Vector{FST})
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
        if cst[2].head === :block && (t.nodes::Vector{FST})[end-2].typ !== NOTCODE
            add_node!((t.nodes::Vector{FST})[idx], Placeholder(0), s)
        end
        add_node!(t, pretty(style, cst[end], s), s)
    end
    t
end
p_let(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_let(DefaultStyle(style), cst, s)

# For/While
function p_for(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(For, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    n = if cst[2].head === :block
        s.indent += s.opts.indent
        n = p_block(style, cst[2], s, join_body = true)
        s.indent -= s.opts.indent
        n
    else
        pretty(style, cst[2], s)
    end

    cst[1].head === :FOR &&
        eq_to_in_normalization!(n, s.opts.always_for_in, s.opts.for_in_replacement)
    add_node!(t, n, s, join_lines = true)

    idx = length(t.nodes::Vector{FST})
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
    if cst[2].head === :block && (t.nodes::Vector{FST})[end-2].typ !== NOTCODE
        add_node!((t.nodes::Vector{FST})[idx], Placeholder(0), s)
    end
    add_node!(t, pretty(style, cst[4], s), s)
    t
end
p_for(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_for(DefaultStyle(style), cst, s)

function p_while(style::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_for(style, cst, s)
    t.typ = While
    t
end
p_while(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_while(DefaultStyle(style), cst, s)

# Do
function p_do(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Do, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    nodes = map(cst[3]) do n
        n
    end
    if nodes[1].fullspan != 0
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, nodes[1], s), s, join_lines = true)
    end
    if nodes[2].head === :block
        s.indent += s.opts.indent
        n = pretty(style, nodes[2], s, ignore_single_line = true)
        s.opts.always_use_return && prepend_return!(n, s)
        add_node!(t, n, s, max_padding = s.opts.indent)
        s.indent -= s.opts.indent
    end
    add_node!(t, pretty(style, cst[end], s), s)
    t
end
p_do(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_do(DefaultStyle(style), cst, s)

# Try
function p_try(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Try, cst, nspaces(s))
    for a in cst
        if a.fullspan == 0 && a.head !== :block
        elseif CSTParser.iskeyword(a)
            add_node!(t, pretty(style, a, s), s, max_padding = 0)
        elseif a.head === :block
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
    t = FST(If, cst, nspaces(s))
    if cst[1].head == :IF
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

        if length(cst) > 4
            len = length(t)
            if is_if(cst[4])
                n = pretty(style, cst[4], s)
                add_node!(t, n, s)
                t.len = max(len, length(n))
            else
                # ELSE KEYWORD
                add_node!(t, pretty(style, cst[4], s), s)
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
        add_node!(t, pretty(style, cst[end], s), s)
    elseif cst[1].head === :ELSEIF
        add_node!(t, pretty(style, cst[1], s), s, max_padding = 0)
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

        if length(cst) > 3
            len = length(t)
            if is_if(cst[4])
                n = pretty(style, cst[4], s)
                add_node!(t, n, s)
                t.len = max(len, length(n))
            else
                # ELSE keyword
                add_node!(t, pretty(style, cst[4], s), s, max_padding = 0)
                s.indent += s.opts.indent
                n = pretty(style, cst[5], s, ignore_single_line = true)
                add_node!(t, n, s, max_padding = s.opts.indent)
                s.indent -= s.opts.indent
            end
        end
    end
    t
end
p_if(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_if(DefaultStyle(style), cst, s)

# Chain/Comparison
function p_chainopcall(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(ds)
    t = FST(Chain, cst, nspaces(s))

    # Check if there's a number literal on the LHS of a dot operator.
    # In this case we need to surround the dot operator with whitespace
    # in order to avoid ambiguity.
    for (i, a) in enumerate(cst)
        if CSTParser.isoperator(a) && CSTParser.isdotted(a) && CSTParser.isnumber(cst[i-1])
            nospace = false
            break
        end
    end

    nws = nospace ? 0 : 1
    for (i, a) in enumerate(cst)
        if CSTParser.isoperator(a) && i == length(cst)
            #+a+1 is parsed as nodes [a, +, 1, +]
            continue
        elseif CSTParser.isoperator(a)
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

function p_comparison(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    t = p_chainopcall(ds, cst, s, nonest = nonest, nospace = nospace)
    t.typ = Comparison
    t
end
p_comparison(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
) where {S<:AbstractStyle} =
    p_comparison(DefaultStyle(style), cst, s, nonest = nonest, nospace = nospace)

# Colon
function p_colonopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; from_import = false)
    style = getstyle(ds)
    t = FST(Chain, cst, nspaces(s))
    nospace = !s.opts.whitespace_ops_in_indices
    for a in cst
        n = if is_opcall(a)
            pretty(style, a, s, nonest = true, nospace = nospace)
        else
            pretty(style, a, s)
        end

        if s.opts.whitespace_ops_in_indices && !is_leaf(n) && !is_iterable(n)
            paren = FST(PUNCTUATION, -1, n.startline, n.startline, "(")
            add_node!(t, paren, s, join_lines = true)
            add_node!(t, n, s, join_lines = true)
            paren = FST(PUNCTUATION, -1, n.startline, n.startline, ")")
            add_node!(t, paren, s, join_lines = true)
        else
            add_node!(t, n, s, join_lines = true)
        end

        if CSTParser.is_colon(a) && from_import
            add_node!(t, Whitespace(1), s)
        end
    end
    t
end
p_colonopcall(
    style::S,
    cst::CSTParser.EXPR,
    s::State;
    from_import = false,
) where {S<:AbstractStyle} =
    p_colonopcall(DefaultStyle(style), cst, s, from_import = from_import)

# Kw
function p_kw(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Kw, cst, nspaces(s))

    exclamation =
        CSTParser.isidentifier(cst[1]) &&
        !CSTParser.isnonstdid(cst[1]) &&
        endswith(cst[1].val, "!")

    if !s.opts.whitespace_in_kwargs && exclamation
        n = pretty(style, cst[1], s)
        add_node!(
            t,
            FST(PUNCTUATION, -1, n.startline, n.startline, "("),
            s,
            join_lines = true,
        )
        add_node!(t, n, s, join_lines = true)
        add_node!(
            t,
            FST(PUNCTUATION, -1, n.startline, n.startline, ")"),
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

    n = pretty(style, cst[3], s)
    opcall = n.typ === Call && n[1].typ === OPERATOR

    if !s.opts.whitespace_in_kwargs && opcall
        add_node!(
            t,
            FST(PUNCTUATION, -1, n.startline, n.startline, "("),
            s,
            join_lines = true,
        )
        add_node!(t, n, s, join_lines = true)
        add_node!(
            t,
            FST(PUNCTUATION, -1, n.startline, n.startline, ")"),
            s,
            join_lines = true,
        )
    else
        add_node!(t, n, s, join_lines = true)
    end

    t
end
p_kw(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_kw(DefaultStyle(style), cst, s)

# Radical operators were introduced in 1.7 which require no surrounding whitespace.
# https://github.com/domluna/JuliaFormatter.jl/issues/530
const RADICAL_OPS = Set(["√", "∛", "∜"])

function p_binaryopcall(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(ds)
    t = FST(Binary, cst, nspaces(s))
    op = cst[2]

    nonest = nonest || CSTParser.is_colon(op)

    if CSTParser.iscurly(cst.parent::CSTParser.EXPR) &&
       (op.val == "<:" || op.val == ">:") &&
       !s.opts.whitespace_typedefs
        nospace = true
    elseif CSTParser.is_colon(op)
        nospace = true
    end
    nospace_args = s.opts.whitespace_ops_in_indices ? false : nospace

    if is_opcall(cst[1])
        n = pretty(style, cst[1], s, nonest = nonest, nospace = nospace_args)
    else
        n = pretty(style, cst[1], s)
    end

    if CSTParser.is_colon(op) &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[1]) &&
       !is_iterable(cst[1])
        paren = FST(PUNCTUATION, -1, n.startline, n.startline, "(")
        add_node!(t, paren, s)
        add_node!(t, n, s, join_lines = true)
        paren = FST(PUNCTUATION, -1, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s)
    end

    nrhs = nest_rhs(cst)
    nrhs && (t.nest_behavior = AlwaysNest)
    nest = (is_binaryop_nestable(style, cst) && !nonest) || nrhs

    if op.fullspan == 0
        # Do nothing - represents a binary op with no textual representation.
        # For example: `2a`, which is equivalent to `2 * a`.
    elseif CSTParser.is_exor(op)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif (
        (CSTParser.isnumber(cst[1]) || is_circumflex_accent(op)) && CSTParser.isdotted(op)
    ) ||
           # 1 .. -2 (can be ., .., ..., etc)
           (CSTParser.isnumber(cst[3]) && startswith(cst[3].val, "-") && is_dot_op(cst))
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, op, s), s, join_lines = true)
        nest ? add_node!(t, Placeholder(1), s) : add_node!(t, Whitespace(1), s)
    elseif !(CSTParser.is_in(op) || CSTParser.is_elof(op) || is_isa(op)) && (
        nospace || (
            !CSTParser.is_anon_func(op) &&
            precedence(op) in (CSTParser.PowerOp, CSTParser.DeclarationOp, CSTParser.DotOp)
        )
    )
        add_node!(t, pretty(style, op, s), s, join_lines = true)
    elseif op.val in RADICAL_OPS
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

    if CSTParser.is_colon(op) &&
       s.opts.whitespace_ops_in_indices &&
       !is_leaf(cst[3]) &&
       !is_iterable(cst[3])
        paren = FST(PUNCTUATION, -1, n.startline, n.startline, "(")
        add_node!(t, paren, s, join_lines = true)
        add_node!(t, n, s, join_lines = true, override_join_lines_based_on_source = !nest)
        paren = FST(PUNCTUATION, -1, n.startline, n.startline, ")")
        add_node!(t, paren, s, join_lines = true)
    else
        add_node!(t, n, s, join_lines = true, override_join_lines_based_on_source = !nest)
    end

    if nest
        # for indent, will be converted to `indent` if needed
        insert!(t.nodes::Vector{FST}, length(t.nodes::Vector{FST}), Placeholder(0))
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

function p_whereopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Where, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)

    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))

    curly_ctx =
        cst.parent.head === :curly ||
        cst[3].head === :curly ||
        cst[3].head === :bracescat ||
        cst[3].head === :parameters

    add_braces =
        s.opts.surround_whereop_typeparameters && !curly_ctx && !CSTParser.is_lbrace(cst[3])

    bc = curly_ctx ? t : FST(BracesCat, nspaces(s))

    brace = FST(PUNCTUATION, -1, t.endline, t.endline, "{")
    add_braces && add_node!(bc, brace, s, join_lines = true)

    nws = s.opts.whitespace_typedefs ? 1 : 0

    for i in 3:length(cst)
        a = cst[i]
        if is_opener(a) && nest
            add_node!(bc, pretty(style, a, s), s, join_lines = true)
            add_node!(bc, Placeholder(0), s)
            s.indent += s.opts.indent
        elseif is_closer(a) && nest
            add_node!(bc, TrailingComma(), s)
            add_node!(bc, Placeholder(0), s)
            add_node!(bc, pretty(style, a, s), s, join_lines = true)
            s.indent -= s.opts.indent
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(bc, pretty(style, a, s), s, join_lines = true)
            add_node!(bc, Placeholder(nws), s)
        elseif is_binary(a)
            add_node!(
                bc,
                pretty(style, a, s, nospace = !s.opts.whitespace_typedefs),
                s,
                join_lines = true,
            )
        else
            n = pretty(style, a, s)
            add_node!(bc, n, s, join_lines = true)
        end
    end

    brace = FST(PUNCTUATION, -1, bc.endline, bc.endline, "}")
    add_braces && add_node!(bc, brace, s, join_lines = true)

    !curly_ctx && add_node!(t, bc, s, join_lines = true)

    t
end
p_whereopcall(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_whereopcall(DefaultStyle(style), cst, s)

function p_conditionalopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Conditional, cst, nspaces(s))

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

function p_unaryopcall(ds::DefaultStyle, cst::CSTParser.EXPR, s::State; nospace = true)
    style = getstyle(ds)
    t = FST(Unary, cst, nspaces(s))
    if length(cst) == 1
        head = cst.head::CSTParser.EXPR
        if head.fullspan != 0
            add_node!(t, pretty(style, head, s), s, join_lines = true)
        end
        add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    elseif CSTParser.isidentifier(cst[2]) && startswith(cst[2].val, "ᶜ")
        add_node!(t, pretty(style, cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    else
        add_node!(t, pretty(style, cst[1], s), s)
        !nospace && add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    end
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
    t = FST(Curly, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))

    nws = s.opts.whitespace_typedefs ? 1 : 0
    if nest
        add_node!(t, Placeholder(0), s)
    end

    for i in 3:length(cst)
        a = cst[i]
        if i == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
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
    t = FST(Call, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, pretty(style, cst[2], s), s, join_lines = true)

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))

    if nest
        add_node!(t, Placeholder(0), s)
    end

    for i in 3:length(cst)
        a = cst[i]
        if i == length(cst) && nest
            add_node!(t, TrailingComma(), s)
            add_node!(t, Placeholder(0), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end

    if s.opts.separate_kwargs_with_semicolon &&
       (!parent_is(cst, n -> is_function_or_macro_def(n) || n.head == :macrocall))
        separate_kwargs_with_semicolon!(t)
    end
    t
end
p_call(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_call(DefaultStyle(style), cst, s)

function p_invisbrackets(
    ds::DefaultStyle,
    cst::CSTParser.EXPR,
    s::State;
    nonest = false,
    nospace = false,
)
    style = getstyle(ds)
    t = FST(Brackets, cst, nspaces(s))
    nest = !is_iterable(cst[2]) && !nonest

    if is_block(cst[2]) || (cst[2].head === :generator && is_block(cst[2][1]))
        t.nest_behavior = AlwaysNest
    end

    add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    nest && add_node!(t, Placeholder(0), s)

    if cst[2].head === :block
        add_node!(t, pretty(style, cst[2], s, from_quote = true), s, join_lines = true)
    elseif is_opcall(cst[2])
        n = pretty(style, cst[2], s, nonest = nonest, nospace = nospace)
        add_node!(t, n, s, join_lines = true)
    else
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    end

    nest && add_node!(t, Placeholder(0), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
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

function p_tuple(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(TupleN, cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))

    for (i, a) in enumerate(cst)
        n = if is_binary(a) && a[2].val == "="
            p_kw(style, a, s)
        else
            pretty(style, a, s)
        end

        if is_opener(n) && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif is_closer(n) && nest
            # An odd case but this could occur if there are no keyword arguments.
            # In which case ";," is invalid syntax.
            #
            # no trailing comma since (arg) is semantically different from (arg,) !!!
            if t[end].typ !== SEMICOLON && length(args) > 1
                add_node!(t, TrailingComma(), s)
            end
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
p_tuple(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_tuple(DefaultStyle(style), cst, s)

function p_braces(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Braces, cst, nspaces(s))
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

function p_bracescat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(BracesCat, cst, nspaces(s))
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

function p_vect(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Vect, cst, nspaces(s))
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

function p_comprehension(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Comprehension, cst, nspaces(s))

    if is_block(cst[2]) || (cst[2].head === :generator && is_block(cst[2][1]))
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

function p_typedcomprehension(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(TypedComprehension, cst, nspaces(s))

    if is_block(cst[3]) || (cst[3].head === :generator && is_block(cst[3][1]))
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

function p_parameters(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Parameters, cst, nspaces(s))

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            push!(t.nodes::Vector{FST}, n)
            push!(t.nodes::Vector{FST}, Placeholder(1))
        else
            push!(t.nodes::Vector{FST}, n)
        end
    end
    t
end
p_parameters(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_parameters(DefaultStyle(style), cst, s)

function p_import(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Import, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    add_node!(t, Whitespace(1), s)

    for i in 2:length(cst)
        a = cst[i]
        if CSTParser.is_colon(a.head) || is_binary(a)
            nodes = collect(a)
            for n in nodes
                add_node!(t, pretty(style, n, s), s, join_lines = true)
                if CSTParser.is_comma(n) || CSTParser.is_colon(n)
                    add_node!(t, Placeholder(1), s)
                end
            end
        elseif CSTParser.is_func_call(a) == true && is_colon_call(a[1])
            # CSTParser.is_func_call can return nothing
            # https://github.com/julia-vscode/CSTParser.jl/issues/306
            n = p_colonopcall(style, a[1], s, from_import = true)
            add_node!(t, n, s, join_lines = true)
        elseif CSTParser.is_comma(a) || CSTParser.is_colon(a)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        else
            n = pretty(style, a, s)
            add_node!(t, n, s, join_lines = true)
        end
    end
    t
end
p_import(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_import(DefaultStyle(style), cst, s)

function p_export(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_import(ds, cst, s)
    t.typ = Export
    t
end
p_export(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_export(DefaultStyle(style), cst, s)

function p_using(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_import(ds, cst, s)
    t.typ = Using
    t
end
p_using(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_using(DefaultStyle(style), cst, s)

function p_as(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(As, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    add_node!(t, Whitespace(1), s)
    add_node!(t, p_keyword(style, cst[2], s), s, join_lines = true)
    # no nesting for the time being
    add_node!(t, Whitespace(1), s)
    add_node!(t, pretty(style, cst[3], s), s, join_lines = true)
    return t
end
p_as(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_as(DefaultStyle(style), cst, s)

function p_ref(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(RefN, cst, nspaces(s))
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

function p_vcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Vcat, cst, nspaces(s))
    st = cst.head === :vcat ? 1 : 2
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

            if i != length(cst) - 1 && has_semicolon(s.doc, n.startline)
                add_node!(t, InverseTrailingSemicolon(), s)
                add_node!(t, Placeholder(1), s)
                # Keep trailing semicolon if there's only one arg
            elseif length(args) == 1 && has_semicolon(s.doc, n.startline)
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

function p_typedvcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_vcat(ds, cst, s)
    t.typ = TypedVcat
    t
end
p_typedvcat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_typedvcat(DefaultStyle(style), cst, s)

function p_hcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Hcat, cst, nspaces(s))
    st = cst.head === :hcat ? 1 : 2
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

function p_typedhcat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_hcat(ds, cst, s)
    t.typ = TypedHcat
    t
end
p_typedhcat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_typedhcat(DefaultStyle(style), cst, s)

function p_ncat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Ncat, cst, nspaces(s))
    st = cst.head === :ncat ? 2 : 3
    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))
    n_semicolons = SEMICOLON_LOOKUP[cst[st].head]

    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        diff_line = t.endline != t.startline
        if i == st && length(args) == 0
            for c in 1:n_semicolons
                add_node!(t, Semicolon(), s)
            end
        elseif i == st
            continue
        elseif is_opener(a) && nest
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(0), s)
        elseif !is_closer(a) && i > st
            add_node!(t, n, s, join_lines = true)

            if i != length(cst) - 1
                for c in 1:n_semicolons
                    add_node!(t, Semicolon(), s)
                end
                add_node!(t, Placeholder(1), s)
                # Keep trailing semicolon if there's only one arg
            elseif length(args) == 1
                for _ in 1:n_semicolons
                    add_node!(t, Semicolon(), s)
                end
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
p_ncat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_ncat(DefaultStyle(style), cst, s)

function p_typedncat(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_ncat(ds, cst, s)
    t.typ = TypedNcat
    t
end
p_typedncat(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_typedncat(DefaultStyle(style), cst, s)

function p_row(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Row, cst, nspaces(s))

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

function p_nrow(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(NRow, cst, nspaces(s))

    n_semicolons = SEMICOLON_LOOKUP[cst[1].head]

    for (i, a) in enumerate(cst)
        i == 1 && continue
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
        if i < length(cst)
            for _ in 1:n_semicolons
                add_node!(t, Semicolon(), s)
            end
            add_node!(t, Whitespace(1), s)
        end
    end
    t.nest_behavior = NeverNest
    t
end
p_nrow(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_nrow(DefaultStyle(style), cst, s)

function p_generator(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ds)
    t = FST(Generator, cst, nspaces(s))
    has_for_kw = false
    for (i, a) in enumerate(cst)
        n = pretty(style, a, s)
        if CSTParser.iskeyword(a)
            if a.head === :FOR
                has_for_kw = true
            end

            # for keyword can only be on the following line
            # if this expression is within an iterable expression
            if a.head === :FOR &&
               parent_is(a, is_iterable; ignore = n -> is_gen(n) || n.head === :brackets)
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

function p_filter(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_generator(ds, cst, s)
    t.typ = Filter
    t
end
p_filter(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_filter(DefaultStyle(style), cst, s)

function p_flatten(ds::DefaultStyle, cst::CSTParser.EXPR, s::State)
    t = p_generator(ds, cst, s)
    t.typ = Flatten
    t
end
p_flatten(style::S, cst::CSTParser.EXPR, s::State) where {S<:AbstractStyle} =
    p_flatten(DefaultStyle(style), cst, s)
