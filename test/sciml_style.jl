@testset "SciML Style" begin
    str = raw"""
       @noinline require_complete(m::Matching) =
           m.inv_match === nothing && throw(ArgumentError("Backwards matching not defined. `complete` the matching first."))
    """
    formatted_str = raw"""
    @noinline function require_complete(m::Matching)
        m.inv_match === nothing &&
            throw(ArgumentError("Backwards matching not defined. `complete` the matching first."))
    end
    """
    @test format_text(str, SciMLStyle()) == formatted_str

    str = raw"""
    begin include("hi") end
    """
    @test format_text(str, SciMLStyle()) == str

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
                            badj::Union{AbstractVector, Integer} = maximum(maximum, fadj);
                            metadata = nothing)
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

    str = raw"""
    Dict{Int, Int}(1 => 2,
                   3 => 4)
    """

    # This should be valid with and without `variable_dict_indent`
    @test format_text(str, SciMLStyle()) == str
    @test format_text(str, SciMLStyle(), variable_dict_indent=true) == str

    str = raw"""
    Dict{Int, Int}(
    1 => 2,
               3 => 4)
    """

    formatted_str1 = raw"""
    Dict{Int, Int}(1 => 2,
                   3 => 4)
    """

    formatted_str2 = raw"""
    Dict{Int, Int}(
        1 => 2,
        3 => 4)
    """

    # `variable_dict_indent` keeps the line break and doesn't align
    @test format_text(str, SciMLStyle()) == formatted_str1
    @test format_text(str, SciMLStyle(), variable_dict_indent=true) == formatted_str2

    str = raw"""
    Dict{Int, Int}(
        1 => 2,
        3 => 4,
    )
    """

    formatted_str = raw"""
    Dict{Int, Int}(1 => 2,
                   3 => 4)
    """

    # This is already valid with `variable_dict_indent`
    @test format_text(str, SciMLStyle()) == formatted_str
    @test format_text(str, SciMLStyle(), variable_dict_indent=true) == str

    str = raw"""
    SomeLongerTypeThanJustString = String
    y = Dict{Int, SomeLongerTypeThanJustString}(1 => "some arbitrary string bla bla bla bla bla bla",
        2 => "another longer arbitrary string bla bla bla bla bla bla bla bla")
    """

    formatted_str1 = raw"""
    SomeLongerTypeThanJustString = String
    y = Dict{Int, SomeLongerTypeThanJustString}(1 => "some arbitrary string bla bla bla bla bla bla",
                                                2 => "another longer arbitrary string bla bla bla bla bla bla bla bla")
    """

    formatted_str2 = raw"""
    SomeLongerTypeThanJustString = String
    y = Dict{Int, SomeLongerTypeThanJustString}(
        1 => "some arbitrary string bla bla bla bla bla bla",
        2 => "another longer arbitrary string bla bla bla bla bla bla bla bla",
    )
    """

    # Here, `variable_dict_indent` forces the line break because the line is too long
    @test format_text(str, SciMLStyle()) == formatted_str1
    @test format_text(str, SciMLStyle(), variable_dict_indent=true) == formatted_str2

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

    # Test `variable_dict_indent` with a comment in a separate line
    @test format_text(str, SciMLStyle()) == str
    @test format_text(str, SciMLStyle(), variable_dict_indent=true) == formatted_str

    str = raw"""
    Dict{Int, Int}(# Comment
                   1 => 2,
                   3 => 4)
    """

    formatted_str1 = raw"""
    Dict{Int, Int}(1 => 2,
                   3 => 4)
    """

    formatted_str2 = raw"""
    Dict{Int, Int}(# Comment
        1 => 2,
        3 => 4)
    """

    # Test `variable_dict_indent` with an inline comment after the opening parenthesis
    # With `variable_dict_indent = false`, the comment will be eaten,
    # see https://github.com/domluna/JuliaFormatter.jl/issues/609
    @test format_text(str, SciMLStyle()) == formatted_str1
    @test format_text(str, SciMLStyle(), variable_dict_indent=true) == formatted_str2

    str = raw"""
    Dict{Int, Int}( # Comment
            # Comment
            1 => 2,
            # Another comment
            3 => 4)
    """

    formatted_str1 = raw"""
    Dict{Int, Int}( # Comment
                   # Comment
                   1 => 2,
                   # Another comment
                   3 => 4)
    """

    formatted_str2 = raw"""
    Dict{Int, Int}( # Comment
        # Comment
        1 => 2,
        # Another comment
        3 => 4)
    """

    # Test `variable_dict_indent` with both an inline comment after the opening parenthesis
    # and a comment in a separate line.
    @test format_text(str, SciMLStyle()) == formatted_str1
    @test format_text(str, SciMLStyle(), variable_dict_indent=true) == formatted_str2
end
