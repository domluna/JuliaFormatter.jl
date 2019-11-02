using Documenter, JuliaFormatter

makedocs(
    sitename = "JuliaFormatter",
    format = Documenter.HTML(prettyurls = true),
    modules = [JuliaFormatter],
    pages = [
        "Introduction" => "index.md",
        "Code Style" => "style.md",
        "Syntax Transforms" => "transforms.md",
    ],
)

deploydocs(
    repo = "github.com/domluna/JuliaFormatter.jl.git",
    target = "build",
    deps = nothing,
    make = nothing,
)
