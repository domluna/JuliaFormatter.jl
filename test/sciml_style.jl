@testset "SciML Style" begin
    str = raw"""
       @noinline require_complete(m::Matching) = m.inv_match === nothing && throw(ArgumentError("Backwards matching not defined. `complete` the matching first."))
    """
    formatted_str = raw"""
    @noinline require_complete(m::Matching) = m.inv_match === nothing &&
                                              throw(ArgumentError("Backwards matching not defined. `complete` the matching first."))
    """
    @test format_text(str, SciMLStyle()) == formatted_str

    str = raw"""
    begin include("hi") end
    """
    formatted_str = raw"""
    begin
        include("hi")
    end
    """
    @test format_text(str, SciMLStyle()) == formatted_str

    str = raw"""
    begin include("hi"); 1 end
    """
    formatted_str = raw"""
    begin
        include("hi")
        1
    end
    """
    @test format_text(str, SciMLStyle()) == formatted_str

    str = raw"""
    BipartiteGraph(fadj::AbstractVector, badj::Union{AbstractVector,Integer} = maximum(maximum, fadj); metadata = nothing) = BipartiteGraph(mapreduce(length, +, fadj; init = 0), fadj, badj, metadata)
    """
    formatted_str = raw"""
    function BipartiteGraph(fadj::AbstractVector,
            badj::Union{AbstractVector, Integer} = maximum(maximum, fadj); metadata = nothing)
        BipartiteGraph(mapreduce(length, +, fadj; init = 0), fadj, badj, metadata)
    end
    """
    @test format_text(str, SciMLStyle()) == formatted_str

    str = raw"""
    @parameters a=b b=c
    """
    formatted_str = raw"""
    @parameters a=b b=c
    """
    @test format_text(str, SciMLStyle()) == formatted_str

    str = raw"""
    @parameters a=b
    """
    formatted_str = raw"""
    @parameters a = b
    """
    @test format_text(str, SciMLStyle()) == formatted_str

    str = raw"""
    function my_large_function(argument1, argument2,
                               argument3, argument4,
                               argument5, x, y, z)
        foo(x) + goo(y)
    end
    """

    formatted_str = raw"""
    function my_large_function(argument1, argument2,
            argument3, argument4,
            argument5, x, y, z)
        foo(x) + goo(y)
    end
    """
    @test format_text(str, SciMLStyle()) == formatted_str

    formatted_str_yas_nesting = raw"""
    function my_large_function(argument1, argument2,
                               argument3, argument4,
                               argument5, x, y, z)
        foo(x) + goo(y)
    end
    """
    @test format_text(str, SciMLStyle(), yas_style_nesting = true) ==
          formatted_str_yas_nesting

    str = raw"""
    Dict{Int, Int}(1 => 2,
        3 => 4)
    """

    # This should be valid with and without `Dict` in `variable_call_indent`
    @test format_text(str, SciMLStyle()) == str
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == str

    str = raw"""
    SVector(1.0,
        2.0)
    """

    # Test the same with different callers
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == str
    @test format_text(str, SciMLStyle(), variable_call_indent = ["SVector", "test2"]) == str

    str = raw"""
    Dict{Int, Int}(
    1 => 2,
               3 => 4)
    """

    formatted_str1 = raw"""
    Dict{Int, Int}(
        1 => 2,
        3 => 4)
    """

    formatted_str2 = raw"""
    Dict{Int, Int}(
        1 => 2,
        3 => 4)
    """

    # `variable_call_indent` keeps the line break and doesn't align
    @test format_text(str, SciMLStyle()) == formatted_str1
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == formatted_str2

    str = raw"""
    SVector(
    1.0,
               2.0)
    """

    formatted_str1 = raw"""
    SVector(
        1.0,
        2.0)
    """

    formatted_str2 = raw"""
    SVector(
        1.0,
        2.0)
    """

    # Test the same with different callers
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == formatted_str1
    @test format_text(str, SciMLStyle(), variable_call_indent = ["test", "SVector"]) ==
          formatted_str2

    str = raw"""
    Dict{Int, Int}(
        1 => 2,
        3 => 4,
    )
    """

    # appears on a different line in source
    formatted_str = raw"""
    Dict{Int, Int}(
        1 => 2,
        3 => 4
    )
    """

    formatted_str_yas_nesting = raw"""
    Dict{Int, Int}(1 => 2,
                   3 => 4)
    """

    # This is already valid with `variable_call_indent`
    @test format_text(str, SciMLStyle()) == formatted_str
    @test format_text(str, SciMLStyle(), yas_style_nesting = true) ==
          formatted_str_yas_nesting
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == formatted_str

    str = raw"""
    SomeLongerTypeThanJustString = String
    y = Dict{Int, SomeLongerTypeThanJustString}(1 => "some arbitrary string bla bla bla bla bla bla",
        2 => "another longer arbitrary string bla bla bla bla bla bla bla bla")
    """

    # over margin will nest
    formatted_str1 = raw"""
    SomeLongerTypeThanJustString = String
    y = Dict{Int, SomeLongerTypeThanJustString}(
        1 => "some arbitrary string bla bla bla bla bla bla",
        2 => "another longer arbitrary string bla bla bla bla bla bla bla bla")
    """

    formatted_str2 = raw"""
    SomeLongerTypeThanJustString = String
    y = Dict{Int, SomeLongerTypeThanJustString}(
        1 => "some arbitrary string bla bla bla bla bla bla",
        2 => "another longer arbitrary string bla bla bla bla bla bla bla bla")
    """

    # Here, `variable_call_indent` forces the line break because the line is too long
    @test format_text(str, SciMLStyle()) == formatted_str1
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == formatted_str2

    str = raw"""
    Dict{Int, Int}(
                   # Comment
                   1 => 2,
                   3 => 4)
    """

    formatted_str = raw"""
    Dict{Int, Int}(
        # Comment
        1 => 2,
        3 => 4)
    """

    # Test `variable_call_indent` with a comment in a separate line
    @test format_text(str, SciMLStyle()) == formatted_str
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == formatted_str

    str = raw"""
    SVector(
            # Comment
            1.0,
            2.0)
    """

    formatted_str = raw"""
    SVector(
        # Comment
        1.0,
        2.0)
    """

    # Test the same with different callers
    @test format_text(str, SciMLStyle()) == formatted_str
    @test format_text(str, SciMLStyle(), variable_call_indent = ["SVector"]) ==
          formatted_str

    str = raw"""
    Dict{Int, Int}(# Comment
                   1 => 2,
                   3 => 4)
    """

    formatted_str1 = raw"""
    Dict{Int, Int}(# Comment
        1 => 2,
        3 => 4)
    """

    formatted_str2 = raw"""
    Dict{Int, Int}(# Comment
        1 => 2,
        3 => 4)
    """

    # Test `variable_call_indent` with an inline comment after the opening parenthesis
    # With `variable_call_indent = false`, the comment will be eaten,
    # see https://github.com/domluna/JuliaFormatter.jl/issues/609
    @test format_text(str, SciMLStyle()) == formatted_str1
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == formatted_str2

    str = raw"""
    Dict{Int, Int}( # Comment
            # Comment
            1 => 2,
            # Another comment
            3 => 4)
    """

    formatted_str = raw"""
    Dict{Int, Int}( # Comment
        # Comment
        1 => 2,
        # Another comment
        3 => 4)
    """

    # Test `variable_call_indent` with both an inline comment after the opening parenthesis
    # and a comment in a separate line.
    @test format_text(str, SciMLStyle()) == formatted_str
    @test format_text(str, SciMLStyle(), variable_call_indent = ["Dict"]) == formatted_str

    str = raw"""
    SVector( # Comment
                # Comment
                1.0,
                # Another comment
                2.0)
    """

    formatted_str = raw"""
    SVector( # Comment
        # Comment
        1.0,
        # Another comment
        2.0)
    """

    # Test the same with different callers
    @test format_text(str, SciMLStyle(), variable_call_indent = ["test"]) == formatted_str
    @test format_text(str, SciMLStyle(), variable_call_indent = ["SVector", "test"]) ==
          formatted_str

    str = """
    function alg_cache(alg::FineRKN4, u, rate_prototype, ::Type{uEltypeNoUnits},
           ::Type{uBottomEltypeNoUnits}, ::Type{tTypeNoUnits}, uprev, uprev2, f, t,
          dt, reltol, p, calck, ::Val{true}) where {uEltypeNoUnits,
        uBottomEltypeNoUnits,tTypeNoUnits}

        reduced_rate_prototype = rate_prototype.x[2]
        tab = FineRKN4ConstantCache(constvalue(uBottomEltypeNoUnits), constvalue(tTypeNoUnits))
        k1 = zero(rate_prototype)
        k2 = zero(reduced_rate_prototype)
        k3 = zero(reduced_rate_prototype)
        k4 = zero(reduced_rate_prototype)
        k5 = zero(reduced_rate_prototype)
        k = zero(rate_prototype)
        utilde = zero(u)
        atmp = similar(u, uEltypeNoUnits)
        recursivefill!(atmp, false)
        tmp = zero(u)
        FineRKN4Cache(u, uprev, k1, k2, k3, k4, k5, k, utilde, tmp, atmp, tab)
    end"""

    formatted_str = """
    function alg_cache(alg::FineRKN4, u, rate_prototype, ::Type{uEltypeNoUnits},
            ::Type{uBottomEltypeNoUnits}, ::Type{tTypeNoUnits}, uprev, uprev2, f, t,
            dt, reltol, p, calck,
            ::Val{true}) where {uEltypeNoUnits,
            uBottomEltypeNoUnits, tTypeNoUnits}
        reduced_rate_prototype = rate_prototype.x[2]
        tab = FineRKN4ConstantCache(constvalue(uBottomEltypeNoUnits), constvalue(tTypeNoUnits))
        k1 = zero(rate_prototype)
        k2 = zero(reduced_rate_prototype)
        k3 = zero(reduced_rate_prototype)
        k4 = zero(reduced_rate_prototype)
        k5 = zero(reduced_rate_prototype)
        k = zero(rate_prototype)
        utilde = zero(u)
        atmp = similar(u, uEltypeNoUnits)
        recursivefill!(atmp, false)
        tmp = zero(u)
        FineRKN4Cache(u, uprev, k1, k2, k3, k4, k5, k, utilde, tmp, atmp, tab)
    end"""
    @test format_text(str, SciMLStyle()) == formatted_str

    str = """
    function SpatialMassActionJump(urates::A, srates::B, rs::S, ns::U, pmapper::V;
                                   scale_rates = true, useiszero = true,
                                   nocopy = false) where {A <: AVecOrNothing,
                                                          B <: AMatOrNothing, S, U, V}
        SpatialMassActionJump{A, B, S, U, V}(urates, srates, rs, ns, pmapper, scale_rates,
                                             useiszero, nocopy)
    end"""

    formatted_str = """
    function SpatialMassActionJump(urates::A, srates::B, rs::S, ns::U, pmapper::V;
            scale_rates = true, useiszero = true,
            nocopy = false) where {A <: AVecOrNothing,
            B <: AMatOrNothing, S, U, V}
        SpatialMassActionJump{A, B, S, U, V}(urates, srates, rs, ns, pmapper, scale_rates,
            useiszero, nocopy)
    end"""
    @test format_text(str, SciMLStyle()) == formatted_str

    str = """
    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx::SomePrettyLongTypeName{Foo}
    """

    # disallow_single_arg_nesting is true by defaultin sciml formatting options, so we don't want the line break
    @test format_text(str, SciMLStyle()) == str
    # With `yas_style_nesting=true`, we don't want the line break, as it will look like this:
    # xxxxx::SomePrettyLongTypeName{
    #                               Foo
    #                              }
    @test format_text(str, SciMLStyle(), yas_style_nesting = true) == str

    # https://github.com/SciML/JumpProcesses.jl/pull/362#discussion_r1375432275
    str_ = """
    function (p::CoevolveJumpAggregation{
        T,
        S,
        F1,
        F2,
    })(integrator::AbstractSSAIntegrator) where {T, S, F1, F2 <: Union{Tuple, Nothing}}
        body
    end"""
    str = """
    function (p::CoevolveJumpAggregation{
            T,
            S,
            F1,
            F2
    })(integrator::AbstractSSAIntegrator) where {
            T,
            S,
            F1,
            F2 <:
            Union{
                Tuple,
                Nothing}}
        body
    end"""
    @test format_text(str_, SciMLStyle(), margin = 1) == str

    str_ = """
    macro (p::CoevolveJumpAggregation{
        T,
        S,
        F1,
        F2,
    })(integrator::AbstractSSAIntegrator) where {T, S, F1, F2 <: Union{Tuple, Nothing}}
        body
    end"""
    str = """
    macro (p::CoevolveJumpAggregation{
            T,
            S,
            F1,
            F2
    })(integrator::AbstractSSAIntegrator) where {
            T,
            S,
            F1,
            F2 <:
            Union{
                Tuple,
                Nothing}}
        body
    end"""
    @test format_text(str_, SciMLStyle(), margin = 1) == str

    @testset "optimal nesting" begin
        @testset "function definition" begin
            str = """
            function foo(arg1, arg2, arg3, arg4, arg5)
                body
            end
            """

            fstr = """
            function foo(
                    arg1, arg2, arg3, arg4, arg5)
                body
            end
            """
            @test format_text(str, SciMLStyle(), margin = 41) == fstr
            @test format_text(str, SciMLStyle(), margin = 37) == fstr

            fstr = """
            function foo(arg1, arg2, arg3,
                    arg4, arg5)
                body
            end
            """
            @test format_text(str, SciMLStyle(), margin = 36) == fstr
            # should be 30? might be a unnesting off by 1 error
            @test format_text(str, SciMLStyle(), margin = 31) == fstr

            fstr = """
            function foo(
                    arg1, arg2, arg3,
                    arg4, arg5)
                body
            end
            """
            @test format_text(str, SciMLStyle(), margin = 29) == fstr
            @test format_text(str, SciMLStyle(), margin = 25) == fstr

            fstr = """
            function foo(
                    arg1, arg2,
                    arg3,
                    arg4, arg5)
                body
            end
            """
            @test format_text(str, SciMLStyle(), margin = 24) == fstr

            fstr = """
            function foo(
                    arg1,
                    arg2,
                    arg3,
                    arg4,
                    arg5)
                body
            end
            """
            @test format_text(str, SciMLStyle(), margin = 18) == fstr
        end

        @testset "vector definition" begin
            str = """
            test = [arg1, arg2, arg3, arg4, arg5]
            """

            # Fits within the margin
            @test format_text(str, SciMLStyle(), margin = 41) == str
            @test format_text(str, SciMLStyle(), margin = 37) == str

            fstr = """
            test = [
                arg1, arg2, arg3, arg4, arg5]
            """
            @test format_text(str, SciMLStyle(), margin = 36) == fstr
            @test format_text(str, SciMLStyle(), margin = 33) == fstr

            fstr = """
            test = [arg1, arg2, arg3,
                arg4, arg5]
            """
            @test format_text(str, SciMLStyle(), margin = 32) == fstr
            # should be 25? might be a unnesting off by 1 error
            @test format_text(str, SciMLStyle(), margin = 26) == fstr

            fstr = """
            test = [
                arg1, arg2, arg3,
                arg4, arg5]
            """
            @test format_text(str, SciMLStyle(), margin = 25) == fstr
            @test format_text(str, SciMLStyle(), margin = 21) == fstr

            fstr = """
            test = [arg1, arg2,
                arg3,
                arg4, arg5]
            """
            @test format_text(str, SciMLStyle(), margin = 20) == fstr
            # should be 19? might be a unnesting off by 1 error
            # @test format_text(str, SciMLStyle(), margin = 19) == fstr

            fstr = """
            test = [
                arg1, arg2,
                arg3,
                arg4, arg5]
            """
            @test format_text(str, SciMLStyle(), margin = 19) == fstr
            # should be 15? might be a unnesting off by 1 error
            @test format_text(str, SciMLStyle(), margin = 16) == fstr

            fstr = """
            test = [
                arg1, arg2,
                arg3,
                arg4,
                arg5]
            """
            @test format_text(str, SciMLStyle(), margin = 15) == fstr

            fstr = """
            test = [arg1,
                arg2,
                arg3,
                arg4,
                arg5]
            """
            @test format_text(str, SciMLStyle(), margin = 14) == fstr

            fstr = """
            test = [
                arg1,
                arg2,
                arg3,
                arg4,
                arg5]
            """
            @test format_text(str, SciMLStyle(), margin = 13) == fstr
        end
    end

    str = raw"""
    x = [
        1, 2, 3
    ]
    """

    # This should be valid with and without `yas_style_nesting`
    @test format_text(str, SciMLStyle()) == str
    @test format_text(str, SciMLStyle(), yas_style_nesting = true) == str
end
