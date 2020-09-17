@testset "Document" begin
    s = """
    \"""
    𝔽𝔽

    \"""
    struct A end
    """
    d = JuliaFormatter.Document(s)
    ranges = Dict(1 => 1:4, 2 => 5:7, 3 => 8:8, 4 => 9:12, 5 => 13:25, 6 => 26:25)
    @test ranges == d.line_to_range
end
