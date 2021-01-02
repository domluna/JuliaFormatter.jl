"""
    format_md(text::AbstractString; style::AbstractStyle = DefaultStyle(), kwargs...)

Normalizes the Markdown source and formats Julia code blocks.

See [`format_text`](@ref) for description of formatting options.
"""
function format_md(text::AbstractString; style::AbstractStyle = DefaultStyle(), kwargs...)
    return format_md(text, style; kwargs...)
end

function format_md(text::AbstractString, style::AbstractStyle; kwargs...)
    isempty(text) && return text
    opts = Options(; merge(options(style), kwargs)...)
    return format_md(text, style, opts)
end

function format_md(text::AbstractString, style::AbstractStyle, opts::Options)
    markdown(
        enable!(
            Parser(),
            [
                AdmonitionRule(),
                FootnoteRule(),
                MathRule(),
                TableRule(),
                FormatRule(style, opts),
            ],
        )(
            text,
        ),
    )
end
