@testset "sort.jl" begin
    @testset "sort" begin
        a = rand(10)
        test_rrule(sort, a)
        test_rrule(sort, a; fkwargs=(; rev=true))
    end
    @testset "partialsort" begin
        a = rand(10)
        test_rrule(partialsort, a, 4)
        test_rrule(partialsort, a, 3:5)
        test_rrule(partialsort, a, 1:2:6)

        test_rrule(partialsort, a, 4; fkwargs=(; rev=true))
    end
end
