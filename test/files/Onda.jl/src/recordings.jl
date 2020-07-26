#####
##### MsgPack conversions for Base types
#####

MsgPack.msgpack_type(::Type{Nanosecond}) = MsgPack.IntegerType()
MsgPack.from_msgpack(::Type{Nanosecond}, x::Integer) = Nanosecond(x)
MsgPack.to_msgpack(::MsgPack.IntegerType, x::Nanosecond) = x.value

MsgPack.msgpack_type(::Type{VersionNumber}) = MsgPack.StringType()
MsgPack.from_msgpack(::Type{VersionNumber}, x::String) = VersionNumber(x[2:end])
MsgPack.to_msgpack(::MsgPack.StringType, x::VersionNumber) = string('v', x)

MsgPack.msgpack_type(::Type{UUID}) = MsgPack.StringType()
MsgPack.from_msgpack(::Type{UUID}, x::String) = UUID(x)
MsgPack.to_msgpack(::MsgPack.StringType, x::UUID) = string(x)

MsgPack.msgpack_type(::Type{DataType}) = MsgPack.StringType()
function MsgPack.from_msgpack(::Type{DataType}, x::String)
    return julia_type_from_onda_sample_type(x)
end
function MsgPack.to_msgpack(::MsgPack.StringType, T::DataType)
    return onda_sample_type_from_julia_type(T)
end

#####
##### Julia DataType <--> Onda `sample_type` string
#####

function julia_type_from_onda_sample_type(t::AbstractString)
    t == "int8" && return Int8
    t == "int16" && return Int16
    t == "int32" && return Int32
    t == "int64" && return Int64
    t == "uint8" && return UInt8
    t == "uint16" && return UInt16
    t == "uint32" && return UInt32
    t == "uint64" && return UInt64
    return error("sample type ", t, " is not supported by Onda")
end

function onda_sample_type_from_julia_type(T::Type)
    T === Int8 && return "int8"
    T === Int16 && return "int16"
    T === Int32 && return "int32"
    T === Int64 && return "int64"
    T === UInt8 && return "uint8"
    T === UInt16 && return "uint16"
    T === UInt32 && return "uint32"
    T === UInt64 && return "uint64"
    return error("sample type ", T, " is not supported by Onda")
end

#####
##### annotations
#####

"""
    Annotation <: AbstractTimeSpan

A type representing an individual Onda annotation object. Instances contain
the following fields, following the Onda specification for annotation objects:

  - `value::String`
  - `start_nanosecond::Nanosecond`
  - `stop_nanosecond::Nanosecond`
"""
struct Annotation <: AbstractTimeSpan
    value::String
    start_nanosecond::Nanosecond
    stop_nanosecond::Nanosecond
    function Annotation(value::AbstractString, start::Nanosecond, stop::Nanosecond)
        _validate_timespan(start, stop)
        return new(value, start, stop)
    end
end

MsgPack.msgpack_type(::Type{Annotation}) = MsgPack.StructType()

function Annotation(value, span::AbstractTimeSpan)
    return Annotation(value, first(span), last(span))
end

Base.first(annotation::Annotation) = annotation.start_nanosecond

Base.last(annotation::Annotation) = annotation.stop_nanosecond

#####
##### signals
#####

"""
    Signal

A type representing an individual Onda signal object. Instances contain
the following fields, following the Onda specification for signal objects:

  - `channel_names::Vector{Symbol}`
  - `start_nanosecond::Nanosecond`
  - `stop_nanosecond::Nanosecond`
  - `sample_unit::Symbol`
  - `sample_resolution_in_unit::Float64`
  - `sample_offset_in_unit::Float64`
  - `sample_type::DataType`
  - `sample_rate::Float64`
  - `file_extension::Symbol`
  - `file_options::Union{Nothing,Dict{Symbol,Any}}`
"""
Base.@kwdef struct Signal
    channel_names::Vector{Symbol}
    start_nanosecond::Nanosecond
    stop_nanosecond::Nanosecond
    sample_unit::Symbol
    sample_resolution_in_unit::Float64
    sample_offset_in_unit::Float64
    sample_type::DataType
    sample_rate::Float64
    file_extension::Symbol
    file_options::Union{Nothing,Dict{Symbol,Any}}
    function Signal(channel_names, start_nanosecond, stop_nanosecond, sample_unit,
                    sample_resolution_in_unit, sample_offset_in_unit, sample_type,
                    sample_rate, file_extension, file_options)
        _validate_timespan(start_nanosecond, stop_nanosecond)
        return new(channel_names, start_nanosecond, stop_nanosecond, sample_unit,
                   sample_resolution_in_unit, sample_offset_in_unit, sample_type,
                   sample_rate, file_extension, file_options)
    end
end

function Base.:(==)(a::Signal, b::Signal)
    return all(name -> getfield(a, name) == getfield(b, name), fieldnames(Signal))
end

MsgPack.msgpack_type(::Type{Signal}) = MsgPack.StructType()

function is_valid(signal::Signal)
    return is_lower_snake_case_alphanumeric(string(signal.sample_unit)) &&
           all(n -> is_lower_snake_case_alphanumeric(string(n), ('-', '.')),
               signal.channel_names) &&
           onda_sample_type_from_julia_type(signal.sample_type) isa AbstractString
end

function file_option(signal::Signal, name, default)
    signal.file_options isa Dict && return get(signal.file_options, name, default)
    return default
end

"""
    signal_from_template(signal::Signal;
                         channel_names=signal.channel_names,
                         start_nanosecond=signal.start_nanosecond,
                         stop_nanosecond=signal.stop_nanosecond,
                         sample_unit=signal.sample_unit,
                         sample_resolution_in_unit=signal.sample_resolution_in_unit,
                         sample_offset_in_unit=signal.sample_offset_in_unit,
                         sample_type=signal.sample_type,
                         sample_rate=signal.sample_rate,
                         file_extension=signal.file_extension,
                         file_options=signal.file_options)

Return a `Signal` where each field is mapped to the corresponding keyword argument.
"""
function signal_from_template(signal::Signal; channel_names=signal.channel_names,
                              start_nanosecond=signal.start_nanosecond,
                              stop_nanosecond=signal.stop_nanosecond,
                              sample_unit=signal.sample_unit,
                              sample_resolution_in_unit=signal.sample_resolution_in_unit,
                              sample_offset_in_unit=signal.sample_offset_in_unit,
                              sample_type=signal.sample_type,
                              sample_rate=signal.sample_rate,
                              file_extension=signal.file_extension,
                              file_options=signal.file_options)
    return Signal(channel_names, start_nanosecond, stop_nanosecond, sample_unit,
                  sample_resolution_in_unit, sample_offset_in_unit, sample_type,
                  sample_rate, file_extension, file_options)
end

"""
    channel(signal::Signal, name::Symbol)

Return `i` where `signal.channel_names[i] == name`.
"""
function channel(signal::Signal, name::Symbol)
    return findfirst(isequal(name), signal.channel_names)
end

"""
    channel(signal::Signal, i::Integer)

Return `signal.channel_names[i]`.
"""
channel(signal::Signal, i::Integer) = signal.channel_names[i]

"""
    channel_count(signal::Signal)

Return `length(signal.channel_names)`.
"""
channel_count(signal::Signal) = length(signal.channel_names)

"""
    span(signal::Signal)

Return `TimeSpan(signal.start_nanosecond, signal.stop_nanosecond)`.
"""
span(signal::Signal) = TimeSpan(signal.start_nanosecond, signal.stop_nanosecond)

"""
    duration(signal::Signal)

Return `duration(span(signal))`.
"""
duration(signal::Signal) = duration(span(signal))

"""
    sample_count(signal::Signal)

Return the number of multichannel samples that fit within `duration(signal)`
given `signal.sample_rate`.
"""
function sample_count(signal::Signal)
    return index_from_time(signal.sample_rate, duration(signal)) - 1
end

"""
    sizeof_samples(signal::Signal)

Returns the expected size (in bytes) of the encoded `Samples` object corresponding
to the entirety of `signal`:

    sample_count(signal) * channel_count(signal) * sizeof(signal.sample_type)
"""
function sizeof_samples(signal::Signal)
    return sample_count(signal) * channel_count(signal) * sizeof(signal.sample_type)
end

#####
##### recordings
#####

"""
    Recording

A type representing an individual Onda recording object. Instances contain
the following fields, following the Onda specification for recording objects:

  - `signals::Dict{Symbol,Signal}`
  - `annotations::Set{Annotation}`
"""
struct Recording
    signals::Dict{Symbol,Signal}
    annotations::Set{Annotation}
end

function Base.:(==)(a::Recording, b::Recording)
    return all(name -> getfield(a, name) == getfield(b, name), fieldnames(Recording))
end

MsgPack.msgpack_type(::Type{Recording}) = MsgPack.StructType()

"""
    annotate!(recording::Recording, annotation::Annotation)

Returns `push!(recording.annotations, annotation)`.
"""
function annotate!(recording::Recording, annotation::Annotation)
    return push!(recording.annotations, annotation)
end

"""
    duration(recording::Recording)

Returns `maximum(s -> s.stop_nanosecond, values(recording.signals))`; throws an
`ArgumentError` if `recording.signals` is empty.
"""
function duration(recording::Recording)
    isempty(recording.signals) &&
        throw(ArgumentError("`duration(recording)` is not defined if `isempty(recording.signals)`"))
    return maximum(s -> s.stop_nanosecond, values(recording.signals))
end

"""
    set_span!(recording::Recording, name::Symbol, span::AbstractTimeSpan)

Replace `recording.signals[name]` with a copy that has the `start_nanosecond`
and `start_nanosecond` fields set to match the provided `span`. Returns the
newly constructed `Signal` instance.
"""
function set_span!(recording::Recording, name::Symbol, span::AbstractTimeSpan)
    signal = signal_from_template(recording.signals[name]; start_nanosecond=first(span),
                                  stop_nanosecond=last(span))
    recording.signals[name] = signal
    return signal
end

"""
    set_span!(recording::Recording, span::TimeSpan)

Return `Dict(name => set_span!(recording, name, span) for name in keys(recording.signals))`
"""
function set_span!(recording::Recording, span::AbstractTimeSpan)
    return Dict(name => set_span!(recording, name, span)
                for name in keys(recording.signals))
end

#####
##### reading/writing `recordings.msgpack.zst`
#####

struct Header
    onda_format_version::VersionNumber
    ordered_keys::Bool
end

MsgPack.msgpack_type(::Type{Header}) = MsgPack.StructType()

function read_recordings_file(path)
    file_path = joinpath(path, "recordings.msgpack.zst")
    bytes = zstd_decompress(read(file_path))
    io = IOBuffer(bytes)
    # `0x92` is the MessagePack byte prefix for 2-element array
    read(io, UInt8) == 0x92 || error("recordings.msgpack.zst has bad byte prefix")
    header = MsgPack.unpack(io, Header)
    if !is_supported_onda_format_version(header.onda_format_version)
        @warn("attempting to load `Dataset` with unsupported Onda version",
              supported = ONDA_FORMAT_VERSION, attempting = header.onda_format_version)
        @warn("consider upgrading old datasets via `Onda.upgrade_onda_format_from_v0_2_to_v0_3!`")
    end
    strict = header.ordered_keys ? (Recording,) : ()
    recordings = MsgPack.unpack(io, Dict{UUID,Recording}; strict=strict)
    return header, recordings
end

function write_recordings_file(path, header::Header, recordings::Dict{UUID,Recording})
    file_path = joinpath(path, "recordings.msgpack.zst")
    backup_file_path = joinpath(path, "_recordings.msgpack.zst.backup")
    isfile(file_path) && mv(file_path, backup_file_path)
    io = IOBuffer()
    MsgPack.pack(io, [header, recordings])
    bytes = zstd_compress(resize!(io.data, io.size))
    write(file_path, bytes)
    rm(backup_file_path; force=true)
    return nothing
end
