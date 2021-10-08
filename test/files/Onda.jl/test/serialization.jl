using Test, Onda, Random, Dates

@testset "$(repr(name)) serializer" for (name, options) in [(:lpcm, nothing),
                                                            (Symbol("lpcm.zst"), Dict(:level => 2))]
    signal = Signal([:a, :b, :c], Nanosecond(0), Nanosecond(0), :unit, 0.25, -0.5, Int16,
                    50.5, name, options)
    samples = encode(Samples(signal, false, rand(MersenneTwister(1), 3, Int(50.5 * 10)))).data
    s = serializer(signal)
    bytes = serialize_lpcm(samples, s)
    name == :lpcm && @test bytes == reinterpret(UInt8, vec(samples))
    @test deserialize_lpcm(bytes, s) == samples
    @test deserialize_lpcm(IOBuffer(bytes), s) == samples
    io = IOBuffer()
    serialize_lpcm(io, samples, s)
    seekstart(io)
    @test take!(io) == bytes
end

@test_throws ArgumentError Onda.serializer_constructor_for_file_extension(Val(:extension))
@test_throws ErrorException Onda.register_file_extension_for_serializer(:extension, LPCM)
