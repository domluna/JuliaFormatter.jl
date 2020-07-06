using JuliaFormatter
using JuliaFormatter: DefaultStyle, YASStyle, Options, CONFIG_FILE_NAME
using CSTParser
using Test

fmt1(s; i = 4, m = 80, kwargs...) =
    JuliaFormatter.format_text(s; kwargs..., indent = i, margin = m)
fmt1(s, i, m; kwargs...) = fmt1(s; kwargs..., i = i, m = m)

# Verifies formatting the formatted text
# results in the same output
function fmt(s; i = 4, m = 80, kwargs...)
    s1 = fmt1(s; kwargs..., i = i, m = m)
    return fmt1(s1; kwargs..., i = i, m = m)
end
fmt(s, i, m; kwargs...) = fmt(s; kwargs..., i = i, m = m)

function run_pretty(
    text::String,
    print_width::Int;
    opts = Options(),
    style = DefaultStyle(),
)
    d = JuliaFormatter.Document(text)
    s = JuliaFormatter.State(d, 4, print_width, opts)
    x = CSTParser.parse(text, true)
    t = JuliaFormatter.pretty(style, x, s)
    t
end

function run_nest(text::String, print_width::Int; opts = Options(), style = DefaultStyle())
    d = JuliaFormatter.Document(text)
    s = JuliaFormatter.State(d, 4, print_width, opts)
    x = CSTParser.parse(text, true)
    t = JuliaFormatter.pretty(style, x, s)
    JuliaFormatter.nest!(style, t, s)
    t, s
end

include("default_style.jl")
include("yas_style.jl")
include("options.jl")
include("config.jl")
