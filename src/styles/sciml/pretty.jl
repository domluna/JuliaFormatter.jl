"""
    SciMLStyle()

Formatting style based on [SciMLStyle](https://github.com/SciML/SciMLStyle).

!!! note
    This style is still work-in-progress.

Configurable options with different defaults to [`DefaultStyle`](@ref) are:
- `whitespace_ops_in_indices` = true
- `remove_extra_newlines` = true
- `always_for_in` = true
- `whitespace_typedefs` = true,
- `normalize_line_endings` = "unix"
"""
struct SciMLStyle <: AbstractStyle
    innerstyle::AbstractStyle
end
SciMLStyle() = SciMLStyle(NoopStyle())

function options(style::SciMLStyle)
    return (;
        always_for_in = true,
        always_use_return = false,
        annotate_untyped_fields_with_any = true,
        conditional_to_if = false,
        import_to_using = false,
        join_lines_based_on_source = true,
        normalize_line_endings = "unix",
        pipe_to_function_call = false,
        remove_extra_newlines = true,
        short_to_long_function_def = false,
        long_to_short_function_def = false,
        whitespace_in_kwargs = true,
        whitespace_ops_in_indices = true,
        whitespace_typedefs = true,
        indent = 4,
        margin = 92,
        format_docstrings = false,
        align_struct_field = false,
        align_assignment = false,
        align_conditional = false,
        align_pair_arrow = false,
        align_matrix = false,
        trailing_comma = true,
        trailing_zero = true,
        indent_submodule = false,
        separate_kwargs_with_semicolon = false,
        surround_whereop_typeparameters = true,
        variable_call_indent = [],
        yas_style_nesting = false,
    )
end

function is_binaryop_nestable(::SciMLStyle, cst::CSTParser.EXPR)
    (CSTParser.defines_function(cst) || is_assignment(cst)) && return false
    ((cst[2]::CSTParser.EXPR).val in ("=>", "->", "in")) && return false
    return true
end

const CST_T = [CSTParser.EXPR]
const TUPLE_T = [CSTParser.EXPR, Vector{CSTParser.EXPR}]
for f in [
    :p_import,
    :p_using,
    :p_export,
    :p_vcat,
    :p_ncat,
    :p_typedvcat,
    :p_typedncat,
    :p_row,
    :p_nrow,
    :p_hcat,
    :p_comprehension,
    :p_typedcomprehension,
    :p_generator,
    :p_filter,
    :p_flatten,
]
    for T in CST_T
        @eval function $f(ss::SciMLStyle, cst::$T, s::State; kwargs...)
            style = getstyle(ss)
            $f(YASStyle(style), cst, s; kwargs...)
        end
    end
end

for f in [
    :p_tuple,
    :p_call,
    :p_curly,
    :p_ref,
    :p_braces,
    :p_vect,
    :p_parameters,
    :p_invisbrackets,
    :p_bracescat,
]
    Ts = f === :p_tuple ? TUPLE_T : CST_T
    for T in Ts
        @eval function $f(ss::SciMLStyle, cst::$T, s::State; kwargs...)
            style = getstyle(ss)
            if s.opts.yas_style_nesting
                $f(YASStyle(style), cst, s; kwargs...)
            else
                $f(DefaultStyle(style), cst, s; kwargs...)
            end
        end
    end
end

function p_begin(ss::SciMLStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ss)
    t = FST(Begin, cst, nspaces(s))
    add_node!(t, pretty(style, cst[1], s), s)
    if length(cst) == 2
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[end], s), s, join_lines = true)
    else
        stmts_idxs = 2:length(cst)-1
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

function p_macrocall(ys::SciMLStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(ys)
    t = FST(MacroCall, cst, nspaces(s))

    args = get_args(cst)
    nest = length(args) > 0 && !(length(args) == 1 && unnestable_node(args[1]))
    has_closer = is_closer(cst[end])

    !has_closer && (t.typ = MacroBlock)
    nospace = length(2:length(cst)-1) > 1

    # same as CSTParser.Call but whitespace sensitive
    for (i, a) in enumerate(cst)
        if CSTParser.is_nothing(a)
            s.offset += a.fullspan
            continue
        end

        # Yes:
        # `@parameters a=a b=b`
        #
        # No:
        # `@parameters a = a b = b`
        n = pretty(style, a, s; nospace = nospace)
        if CSTParser.ismacroname(a)
            add_node!(t, n, s, join_lines = true)
            if length(args) > 0
                loc = cursor_loc(s)
                if t[end].line_offset + length(t[end]) < loc[2]
                    add_node!(t, Whitespace(1), s)
                end
            end
        elseif CSTParser.is_comma(a) && i < length(cst) && !is_punc(cst[i+1])
            add_node!(t, n, s, join_lines = true)
            add_node!(t, Placeholder(1), s)
        elseif is_closer(n)
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif i > 1 && is_opener(cst[i-1])
            add_node!(
                t,
                n,
                s,
                join_lines = true,
                override_join_lines_based_on_source = true,
            )
        elseif t.typ === MacroBlock
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

function p_unaryopcall(ds::SciMLStyle, cst::CSTParser.EXPR, s::State; kwargs...)
    style = getstyle(ds)
    t = FST(Unary, cst, nspaces(s))
    if length(cst) == 1
        if cst.head.fullspan != 0
            add_node!(t, pretty(style, cst.head, s), s, join_lines = true)
        end
        add_node!(t, pretty(style, cst[1], s), s, join_lines = true)
    elseif CSTParser.isidentifier(cst[2]) && startswith(cst[2].val, "ᶜ")
        add_node!(t, pretty(style, cst[1], s), s)
        add_node!(t, Whitespace(1), s)
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    else
        add_node!(t, pretty(style, cst[1], s), s)
        add_node!(t, pretty(style, cst[2], s), s, join_lines = true)
    end
    t
end
