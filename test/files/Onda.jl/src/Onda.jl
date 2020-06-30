module Onda

using UUIDs, Dates, Random
using MsgPack
using TranscodingStreams
using CodecZstd

const ONDA_FORMAT_VERSION = v"0.3"

#####
##### utilities
#####

function is_supported_onda_format_version(v::VersionNumber)
    onda_major, onda_minor = ONDA_FORMAT_VERSION.major, ONDA_FORMAT_VERSION.minor
    return onda_major == v.major && (onda_major != 0 || onda_minor == v.minor)
end

const ALPHANUMERIC_SNAKE_CASE_CHARACTERS = Char['_', '0':'9'..., 'a':'z'...]

function is_lower_snake_case_alphanumeric(x::AbstractString, also_allow=())
    return !startswith(x, '_') &&
           !endswith(x, '_') &&
           all(i -> i in ALPHANUMERIC_SNAKE_CASE_CHARACTERS || i in also_allow, x)
end

function zstd_compress(bytes::Vector{UInt8}, level=3)
    compressor = ZstdCompressor(; level=level)
    TranscodingStreams.initialize(compressor)
    compressed_bytes = transcode(compressor, bytes)
    TranscodingStreams.finalize(compressor)
    return compressed_bytes
end

function zstd_compress(writer, io::IO, level=3)
    stream = ZstdCompressorStream(io; level=level)
    result = writer(stream)
    # write `TranscodingStreams.TOKEN_END` instead of calling `close` since
    # `close` closes the underlying `io`, and we don't want to do that
    write(stream, TranscodingStreams.TOKEN_END)
    flush(stream)
    return result
end

zstd_decompress(bytes::Vector{UInt8}) = transcode(ZstdDecompressor, bytes)

zstd_decompress(reader, io::IO) = reader(ZstdDecompressorStream(io))

#####
##### includes/exports
#####

include("timespans.jl")
export AbstractTimeSpan, TimeSpan, contains, overlaps, shortest_timespan_containing,
       index_from_time, time_from_index, duration

include("recordings.jl")
export Recording, Signal, signal_from_template, Annotation, annotate!, span, sizeof_samples

include("serialization.jl")
export AbstractLPCMSerializer, serializer, deserialize_lpcm, serialize_lpcm, LPCM, LPCMZst

include("samples.jl")
export Samples, encode, encode!, decode, decode!, channel, channel_count, sample_count

include("dataset.jl")
export Dataset, samples_path, create_recording!, set_span!, load, store!, delete!,
       save_recordings_file

include("printing.jl")

#####
##### upgrades/deprecations
#####

@deprecate set_duration!(dataset, uuid, duration) begin
    r = dataset.recordings[uuid]
    set_span!(r, TimeSpan(Nanosecond(0), duration))
    r
end

"""
    Onda.upgrade_onda_format_from_v0_2_to_v0_3!(path, combine_annotation_key_value)

Upgrade the Onda v0.2 dataset at `path` to a Onda v0.3 dataset, returning the
upgraded `Dataset`. This upgrade process overwrites `path/recordings.msgpack.zst`
with a v0.3-compliant version of this file; for safety's sake, the old v0.2 file
is preserved at `path/old.recordings.msgpack.zst.backup`.

A couple of the Onda v0.2 -> v0.3 changes require some special handling:

  - The `custom` field was removed from recording objects. This function thus writes out
    a file at `path/recordings_custom.msgpack.zst` that contains a map of UUIDs to
    corresponding recordings' `custom` values before deleting the `custom` field. This
    file can be deserialized via `MsgPack.unpack(Onda.zstd_decompress(read("recordings_custom.msgpack.zst")))`.

  - Annotations no longer have a `key` field. Thus, each annotation's existing `key` and `value`
    fields are combined into the single new `value` field via the provided callback
    `combine_annotation_key_value(annotation_key, annotation_value)`.
"""
function upgrade_onda_format_from_v0_2_to_v0_3!(path, combine_annotation_key_value)
    file_path = joinpath(path, "recordings.msgpack.zst")
    bytes = zstd_decompress(read(file_path))
    mv(file_path, joinpath(path, "old.recordings.msgpack.zst.backup"))
    io = IOBuffer(bytes)
    read(io, UInt8) == 0x92 || error("corrupt recordings.msgpack.zst")
    header = MsgPack.unpack(io, Header)
    v"0.2" <= header.onda_format_version < v"0.3" ||
        error("unsupported original onda_format_version: $(header.onda_format_version)")
    recordings = MsgPack.unpack(io, Dict{UUID,Any})
    customs = Dict{UUID,Any}(uuid => recording["custom"]
                             for (uuid, recording) in recordings)
    write(joinpath(path, "recordings_custom.msgpack.zst"),
          zstd_compress(MsgPack.pack(customs)))
    for (uuid, recording) in recordings
        signal_stop_nanosecond = recording["duration_in_nanoseconds"]
        for signal in values(recording["signals"])
            signal["start_nanosecond"] = 0
            signal["stop_nanosecond"] = signal_stop_nanosecond
            signal["sample_offset_in_unit"] = 0.0
            signal["sample_rate"] = float(signal["sample_rate"])
        end
        for annotation in recording["annotations"]
            annotation["value"] = combine_annotation_key_value(annotation["key"],
                                                               annotation["value"])
            delete!(annotation, "key")
        end
        delete!(recording, "duration_in_nanoseconds")
        delete!(recording, "custom")
    end
    fixed_recordings = MsgPack.unpack(MsgPack.pack(recordings), Dict{UUID,Recording})
    dataset = Dataset(path, Header(v"0.3.0", true), fixed_recordings)
    save_recordings_file(dataset)
    return dataset
end

end # module
