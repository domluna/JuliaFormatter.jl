# This file demonstrates an implementation of FLAC support for Onda.jl. Note
# that it's a naive implementation - it just shells out and assumes you have
# the `flac` command line utility installed and available on your system.

using Onda, Test, Random, Dates

#####
##### FLAC
#####

"""
     FLAC(lpcm::LPCM; sample_rate, level=5)
     FLAC(signal::Signal; level=5)

Return a `FLAC<:AbstractLPCMSerializer` instance that represents the
FLAC format assumed for signal files with the ".flac" extension.

The `sample_rate` keyword argument corresponds to `flac`'s `--sample-rate` flag,
while `level` corresponds to `flac`'s `--compression-level` flag.

Note that FLAC is only applicable for signals where `1 <= signal.channel_count <= 8`
and `sizeof(signal.sample_type) in (1, 2)`. The corresponding `signal.file_options`
value may be either `nothing` or `Dict(:level => i)` where `0 <= i <= 8`.

See https://xiph.org/flac/ for details about FLAC.

See also: [`Zstd`](@ref)
"""
struct FLAC{S} <: Onda.AbstractLPCMSerializer
    lpcm::LPCM{S}
    sample_rate::Int
    level::Int
    function FLAC(lpcm::LPCM{S}; sample_rate, level=5) where {S}
        sizeof(S) in (1, 2) || throw(ArgumentError("bit depth must be 8 or 16"))
        1 <= lpcm.channel_count <= 8 ||
            throw(ArgumentError("channel count must be between 1 and 8"))
        return new{S}(lpcm, sample_rate, level)
    end
end

function FLAC(signal::Signal; kwargs...)
    return FLAC(LPCM(signal); sample_rate=signal.sample_rate, kwargs...)
end

Onda.serializer_constructor_for_file_extension(::Val{:flac}) = FLAC

function flac_raw_specification_flags(serializer::FLAC{S}) where {S}
    return (level="--compression-level-$(serializer.level)", endian="--endian=little",
            channels="--channels=$(serializer.lpcm.channel_count)",
            bps="--bps=$(sizeof(S) * 8)",
            sample_rate="--sample-rate=$(serializer.sample_rate)",
            is_signed=string("--sign=", S <: Signed ? "signed" : "unsigned"))
end

function Onda.deserialize_lpcm(io::IO, serializer::FLAC, args...)
    flags = flac_raw_specification_flags(serializer)
    command = pipeline(`flac - --totally-silent -d --force-raw-format $(flags.endian) $(flags.is_signed)`;
                       stdin=io)
    return open(lpcm_io -> deserialize_lpcm(lpcm_io, serializer.lpcm, args...), command,
                "r")
end

function Onda.serialize_lpcm(io::IO, samples::AbstractMatrix, serializer::FLAC)
    flags = flac_raw_specification_flags(serializer)
    command = pipeline(`flac --totally-silent $(flags) -`; stdout=io)
    return open(lpcm_io -> serialize_lpcm(lpcm_io, samples, serializer.lpcm), command, "w")
end

#####
##### tests
#####

if VERSION >= v"1.1.0"
    @testset "FLAC example" begin
        signal = Signal([:a, :b, :c], Nanosecond(0), Nanosecond(0), :unit, 0.25, 0.0, Int16,
                        50.0, :flac, Dict(:level => 2))
        samples = encode(Samples(signal, false, rand(MersenneTwister(1), 3, 50 * 10))).data
        s = serializer(signal)
        bytes = serialize_lpcm(samples, s)
        @test deserialize_lpcm(bytes, s) == samples
        @test deserialize_lpcm(IOBuffer(bytes), s) == samples
        io = IOBuffer()
        serialize_lpcm(io, samples, s)
        seekstart(io)
        @test take!(io) == bytes
    end
else
    @warn "This example may be broken on Julia versions lower than v1.1 due to https://github.com/JuliaLang/julia/issues/33117"
end
