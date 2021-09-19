@testset "mean" begin
    n = 9
    @testset "Basic" begin
        test_rrule(mean, randn(n))
    end
    @testset "with dims kwargs" begin
        test_rrule(mean, randn(n); fkwargs=(; dims=1))
        test_rrule(mean, randn(n, 4); fkwargs=(; dims=2))
    end
end
