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
end
