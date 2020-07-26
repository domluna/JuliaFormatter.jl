#####
##### `AbstractLPCMSerializer`
#####

"""
    AbstractLPCMSerializer

A type whose subtypes support:

  - [`deserialize_lpcm`](@ref)
  - [`serialize_lpcm`](@ref)

All definitions of subtypes of the form `S<:AbstractLPCMSerializer` must also support
a constructor of the form `S(::Signal)` and overload `Onda.serializer_constructor_for_file_extension`
with the appropriate file extension.

See also: [`serializer`](@ref), [`LPCM`](@ref), [`LPCMZst`](@ref)
"""
abstract type AbstractLPCMSerializer end

"""
    Onda.serializer_constructor_for_file_extension(::Val{:extension_symbol})

Return a constructor of the form `S(::Signal)::AbstractLPCMSerializer`
corresponding to the provided extension.

This function should be overloaded for new `AbstractLPCMSerializer` subtypes.
"""
function serializer_constructor_for_file_extension(::Val{unknown}) where {unknown}
    return throw(ArgumentError("unknown file extension: $unknown"))
end

function register_file_extension_for_serializer(extension::Symbol,
                                                T::Type{<:AbstractLPCMSerializer})
    return error("""
                 `Onda.register_file_extension_for_serializer(ext, T)` is deprecated; instead, `AbstractLPCMSerializer`
                 authors should define `Onda.serializer_constructor_for_file_extension(::Val{ext}) = T`.
                 """)
end

"""
    serializer(signal::Signal; kwargs...)

Return `S(signal; kwargs...)` where `S` is the `AbstractLPCMSerializer` that
corresponds to `signal.file_extension` (as determined by the serializer author
via `serializer_constructor_for_file_extension`).

See also: [`deserialize_lpcm`](@ref), [`serialize_lpcm`](@ref)
"""
function serializer(signal::Signal; kwargs...)
    T = serializer_constructor_for_file_extension(Val(signal.file_extension))
    return T(signal; kwargs...)
end

"""
    deserialize_lpcm(bytes, serializer::AbstractLPCMSerializer)

Return a channels-by-timesteps `AbstractMatrix` of interleaved LPCM-encoded
sample data by deserializing the provided `bytes` from the given `serializer`.

Note that this operation may be performed in a zero-copy manner such that the
returned sample matrix directly aliases `bytes`.

This function is the inverse of the corresponding [`serialize_lpcm`](@ref)
method, i.e.:

```
serialize_lpcm(deserialize_lpcm(bytes, serializer), serializer) == bytes
```

    deserialize_lpcm(bytes, serializer::AbstractLPCMSerializer, sample_offset, sample_count)

Similar to `deserialize_lpcm(bytes, serializer)`, but deserialize only the segment requested
via `sample_offset` and `sample_count`.

    deserialize_lpcm(io::IO, serializer::AbstractLPCMSerializer[, sample_offset, sample_count])

Similar to the corresponding `deserialize_lpcm(bytes, ...)` methods, but the bytes
to be deserialized are read directly from `io`.

If `sample_offset`/`sample_count` is provided and `io`/`serializer` support
seeking, implementations of this method may read only the bytes required to
extract the requested segment instead of reading the entire stream.
"""
function deserialize_lpcm end

"""
    serialize_lpcm(samples::AbstractMatrix, serializer::AbstractLPCMSerializer)

Return the `AbstractVector{UInt8}` of bytes that results from serializing `samples`
to the given `serializer`, where `samples` is a channels-by-timesteps matrix of
interleaved LPCM-encoded sample data.

Note that this operation may be performed in a zero-copy manner such that the
returned `AbstractVector{UInt8}` directly aliases `samples`.

This function is the inverse of the corresponding [`deserialize_lpcm`](@ref)
method, i.e.:

```
deserialize_lpcm(serialize_lpcm(samples, serializer), serializer) == samples
```

    serialize_lpcm(io::IO, samples::AbstractMatrix, serializer::AbstractLPCMSerializer)

Similar to the corresponding `serialize_lpcm(samples, serializer)` method, but serializes
directly to `io`.
"""
function serialize_lpcm end

#####
##### fallbacks/utilities
#####

function deserialize_lpcm(bytes, serializer::AbstractLPCMSerializer, args...)
    return deserialize_lpcm(IOBuffer(bytes), serializer, args...)
end

function serialize_lpcm(samples::AbstractMatrix, serializer::AbstractLPCMSerializer)
    io = IOBuffer()
    serialize_lpcm(io, samples, serializer)
    return resize!(io.data, io.size)
end

jump(io::IO, n) = (read(io, n); nothing)
jump(io::IOStream, n) = (skip(io, n); nothing)
jump(io::IOBuffer, n) = ((io.seekable ? skip(io, n) : read(io, n)); nothing)

unsafe_vec_uint8(x::AbstractVector{UInt8}) = convert(Vector{UInt8}, x)
function unsafe_vec_uint8(x::Base.ReinterpretArray{UInt8,1})
    return unsafe_wrap(Vector{UInt8}, pointer(x), length(x))
end

#####
##### `LPCM`
#####

const LPCM_SAMPLE_TYPE_UNION = Union{Int8,Int16,Int32,Int64,UInt8,UInt16,UInt32,UInt64}

"""
    LPCM{S}(channel_count)
    LPCM(signal::Signal)

Return a `LPCM<:AbstractLPCMSerializer` instance corresponding to Onda's default
interleaved LPCM format assumed for signal files with the ".lpcm" extension.

`S` corresponds to `signal.sample_type`, while `channel_count` corresponds to
`signal.channel_names`.

Note that bytes (de)serialized via this serializer are little-endian per the
Onda specification.
"""
struct LPCM{S<:LPCM_SAMPLE_TYPE_UNION} <: AbstractLPCMSerializer
    channel_count::Int
end

LPCM(signal::Signal) = LPCM{signal.sample_type}(length(signal.channel_names))

serializer_constructor_for_file_extension(::Val{:lpcm}) = LPCM

function deserialize_lpcm(bytes, serializer::LPCM{S}) where {S}
    sample_count = Int(length(bytes) / sizeof(S) / serializer.channel_count)
    return deserialize_lpcm(bytes, serializer, 0, sample_count)
end

function deserialize_lpcm(bytes, serializer::LPCM{S}, sample_offset, sample_count) where {S}
    i = (serializer.channel_count * sample_offset) + 1
    j = serializer.channel_count * (sample_offset + sample_count)
    return reshape(view(reinterpret(S, bytes), i:j),
                   (serializer.channel_count, sample_count))
end

function deserialize_lpcm(io::IO, serializer::LPCM)
    return deserialize_lpcm(read(io), serializer)
end

function deserialize_lpcm(io::IO, serializer::LPCM{S}, sample_offset,
                          sample_count) where {S}
    bytes_per_sample = sizeof(S) * serializer.channel_count
    jump(io, bytes_per_sample * sample_offset)
    return deserialize_lpcm(read(io, bytes_per_sample * sample_count), serializer)
end

function _validate_lpcm_samples(samples::AbstractMatrix{S}, serializer::LPCM{S}) where {S}
    serializer.channel_count == size(samples, 1) && return nothing
    return throw(ArgumentError("`samples` row count does not match expected channel count"))
end

function serialize_lpcm(io::IO, samples::AbstractMatrix, serializer::LPCM)
    _validate_lpcm_samples(samples, serializer)
    return write(io, samples)
end

function serialize_lpcm(samples::Matrix, serializer::LPCM)
    _validate_lpcm_samples(samples, serializer)
    return reinterpret(UInt8, vec(samples))
end

#####
##### `LPCMZst`
#####

"""
    LPCMZst(lpcm::LPCM; level=3)
    LPCMZst(signal::Signal; level=3)

Return a `LPCMZst<:AbstractLPCMSerializer` instance that corresponds to
Onda's default interleaved LPCM format compressed by `zstd`. This serializer
is assumed for signal files with the ".lpcm.zst" extension.

The `level` keyword argument sets the same compression level parameter as the
corresponding flag documented by the `zstd` command line utility.

See https://facebook.github.io/zstd/ for details about `zstd`.
"""
struct LPCMZst{S} <: AbstractLPCMSerializer
    lpcm::LPCM{S}
    level::Int
    LPCMZst(lpcm::LPCM{S}; level=3) where {S} = new{S}(lpcm, level)
end

LPCMZst(signal::Signal; kwargs...) = LPCMZst(LPCM(signal); kwargs...)

serializer_constructor_for_file_extension(::Val{Symbol("lpcm.zst")}) = LPCMZst

function deserialize_lpcm(bytes, serializer::LPCMZst, args...)
    bytes = unsafe_vec_uint8(bytes)
    return deserialize_lpcm(zstd_decompress(bytes), serializer.lpcm, args...)
end

function deserialize_lpcm(io::IO, serializer::LPCMZst, args...)
    reader = io -> deserialize_lpcm(io, serializer.lpcm, args...)
    return zstd_decompress(reader, io)
end

function serialize_lpcm(samples::AbstractMatrix, serializer::LPCMZst)
    bytes = serialize_lpcm(samples, serializer.lpcm)
    return zstd_compress(unsafe_vec_uint8(bytes), serializer.level)
end

function serialize_lpcm(io::IO, samples::AbstractMatrix, serializer::LPCMZst)
    writer = io -> serialize_lpcm(io, samples, serializer.lpcm)
    return zstd_compress(writer, io, serializer.level)
end
