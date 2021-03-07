using JuliaFormatter
using JuliaFormatter: DefaultStyle, YASStyle, Options, options, CONFIG_FILE_NAME
using CSTParser
using Test

fmt1(s; i = 4, m = 80, kwargs...) =
    JuliaFormatter.format_text(s; kwargs..., indent = i, margin = m)
fmt1(s, i, m; kwargs...) = fmt1(s; kwargs..., i = i, m = m)

# Verifies formatting the formatted text
# results in the same output
function fmt(s; i = 4, m = 80, kwargs...)
    kws = merge(options(DefaultStyle()), kwargs)
    s1 = fmt1(s; kws..., i = i, m = m)
    return fmt1(s1; kws..., i = i, m = m)
end
fmt(s, i, m; kwargs...) = fmt(s; kwargs..., i = i, m = m)

yasfmt1(str) = fmt1(str; style = YASStyle(), options(DefaultStyle())...)
yasfmt(str, i = 4, m = 80; kwargs...) = fmt(str, i, m; style = YASStyle(), kwargs...)

bluefmt1(str) = fmt1(str; style = BlueStyle(), options(DefaultStyle())...)
bluefmt(str, i = 4, m = 80; kwargs...) = fmt(str, i, m; style = BlueStyle(), kwargs...)

function run_pretty(text::String; style = DefaultStyle(), opts = Options())
    d = JuliaFormatter.Document(text)
    s = JuliaFormatter.State(d, opts)
    x = CSTParser.parse(text, true)
    t = JuliaFormatter.pretty(style, x, s)
    t
end
run_pretty(text::String, margin::Int) = run_pretty(text, opts = Options(margin = margin))

function run_nest(text::String; opts = Options(), style = DefaultStyle())
    d = JuliaFormatter.Document(text)
    s = JuliaFormatter.State(d, opts)
    x = CSTParser.parse(text, true)
    t = JuliaFormatter.pretty(style, x, s)
    JuliaFormatter.nest!(style, t, s)
    t, s
end
run_nest(text::String, margin::Int) = run_nest(text, opts = Options(margin = margin))

@testset "JuliaFormatter" begin
    include("default_style.jl")
    include("yas_style.jl")
    include("blue_style.jl")
    include("issues.jl")
    include("options.jl")
    include("config.jl")
    include("document.jl")
    include("interface.jl")
end
