@testset "Document" begin
    s = """
    \"""
    𝔽𝔽
    \"""
    struct A end
    """
    d = JuliaFormatter.Document(s)
    ranges = Dict(1 => 1:4, 2 => 5:7, 3 => 8:11, 4 => 12:24, 5 => 25:24)
    @test ranges == d.line_to_range
end
