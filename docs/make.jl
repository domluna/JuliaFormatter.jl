using Documenter, JuliaFormatter

makedocs(
    sitename = "JuliaFormatter",
    format = Documenter.HTML(prettyurls = true),
    modules = [JuliaFormatter],
    pages = [
        "Introduction" => "index.md",
        "How It Works" => "how_it_works.md",
        "Code Style" => "style.md",
        "Syntax Transforms" => "transforms.md",
    ],
)

deploydocs(repo = "github.com/domluna/JuliaFormatter.jl.git")
