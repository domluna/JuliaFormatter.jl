"""
    format_md(
        text::AbstractString;
        indent::Int = 4,
        margin::Int = 92,
        style::AbstractStyle = DefaultStyle(),
        always_for_in::Bool = false,
        whitespace_typedefs::Bool = false,
        whitespace_ops_in_indices::Bool = false,
        remove_extra_newlines::Bool = false,
        import_to_using::Bool = false,
        pipe_to_function_call::Bool = false,
        short_to_long_function_def::Bool = false,
        always_use_return::Bool = false,
        whitespace_in_kwargs::Bool = true,
        annotate_untyped_fields_with_any::Bool = true,
        format_docstrings::Bool = false,
    )::String

Normalizes the Markdown source and formats Julia code blocks.

See [`format_text`](@ref) for description of formatting options.
"""
function format_md(
    text::AbstractString;
    indent::Int = 4,
    margin::Int = 92,
    style::AbstractStyle = DefaultStyle(),
    always_for_in::Bool = false,
    whitespace_typedefs::Bool = false,
    whitespace_ops_in_indices::Bool = false,
    remove_extra_newlines::Bool = false,
    import_to_using::Bool = false,
    pipe_to_function_call::Bool = false,
    short_to_long_function_def::Bool = false,
    always_use_return::Bool = false,
    whitespace_in_kwargs::Bool = true,
    annotate_untyped_fields_with_any::Bool = true,
    format_docstrings::Bool = false,
)
    isempty(text) && return text
    opts = Options(
        always_for_in = always_for_in,
        whitespace_typedefs = whitespace_typedefs,
        whitespace_ops_in_indices = whitespace_ops_in_indices,
        remove_extra_newlines = remove_extra_newlines,
        import_to_using = import_to_using,
        pipe_to_function_call = pipe_to_function_call,
        short_to_long_function_def = short_to_long_function_def,
        always_use_return = always_use_return,
        whitespace_in_kwargs = whitespace_in_kwargs,
        annotate_untyped_fields_with_any = annotate_untyped_fields_with_any,
        format_docstrings = format_docstrings,
    )
    formatted = markdown(enable!(
        Parser(),
        [
            AdmonitionRule(),
            FootnoteRule(),
            MathRule(),
            TableRule(),
            FormatRule(style, indent, margin, opts),
        ],
    )(
        text,
    ))
    return formatted
end
