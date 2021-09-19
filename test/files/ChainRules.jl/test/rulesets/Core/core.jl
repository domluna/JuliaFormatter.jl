@testset "typeassert" begin
    test_rrule(typeassert, 1.1, Float64)
    test_frule(typeassert, 1.1, Float64)
end

@testset "ifelse" begin
    test_rrule(ifelse, true, 1.1, 2.0)
    test_frule(ifelse, false, 1.1, 2.0)

    test_rrule(ifelse, true, [1.1], [2.0]; check_inferred=false)
    test_frule(ifelse, false, [1.1], [2.0]; check_inferred=false)
end
