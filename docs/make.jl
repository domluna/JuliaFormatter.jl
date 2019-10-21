using Documenter, JuliaFormatter

makedocs(
    sitename = "JuliaFormatter",
    modules = [JuliaFormatter],
    pages = [
        "Introduction" => "index.md",
        "Code Style" => "style.md",
        "Syntax Transforms" => "transforms.md",
    ],
)

deploydocs(repo = "github.com/domluna/JuliaFormatter.jl.git")
