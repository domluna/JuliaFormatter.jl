module PProf

export pprof

using Profile
using ProtoBuf
using OrderedCollections

include(joinpath("..", "lib", "perftools.jl"))

import .perftools.profiles: ValueType, Sample, Function,
                            Location, Line
const PProfile = perftools.profiles.Profile

"""
    _enter!(dict::OrderedDict{T, Int}, key::T) where T

Resolves from `key` to the index (zero-based) in the dict.
Useful for the Strings table
"""
function _enter!(dict::OrderedDict{T, Int}, key::T) where T
    if haskey(dict, key)
        return dict[key]
    else
        l = length(dict)
        dict[key] = l
        return l
    end
end

using Base.StackTraces: lookup, StackFrame

# TODO:
# - Mappings
# - Understand what Sample.value[0] is supposed to be
# - Check that we add Locations in the right order.
# - Tests!

"""
    pprof(; outfile = "profile.pb.gz", drop_frames = "", keep_frames = "")

Fetches and converts `Profile` data to the `pprof` format.

# Arguments:
- `out::String`: Filename for output.
- `drop_frames`: frames with function_name fully matching regexp string will be dropped from the samples,
                 along with their successors.
- `keep_frames`: frames with function_name fully matching regexp string will be kept, even if it matches drop_functions.
"""
function pprof(;out::AbstractString = "profile.pb.gz",
                drop_frames::Union{Nothing, AbstractString} = nothing,
                keep_frames::Union{Nothing, AbstractString} = nothing)
    data   = copy(Profile.fetch())
    period = ccall(:jl_profile_delay_nsec, UInt64, ())

    string_table = OrderedDict{AbstractString, Int}()
    enter!(string) = _enter!(string_table, string)
    ValueType!(_type, unit) = ValueType(_type = enter!(_type), unit = enter!(unit))

    # Setup:
    enter!("")  # NOTE: pprof requires first entry to be ""
    # Functions need a uid, we'll use the pointer for the method instance
    funcs = Dict{UInt64, Function}()
    locs  = Dict{UInt64, Location}()

    sample_type = [
        ValueType!("events",      "count"), # Mandatory
        ValueType!("stack_depth", "count")
    ]

    prof = PProfile(
        sample = [], location = [], _function = [],
        mapping = [], string_table = [], sample_type = sample_type,
        period = period, period_type = ValueType!("cpu", "ns")
    )

    if drop_frames !== nothing
        prof.drop_frames = enter!(drop_frames)
    end
    if keep_frames !== nothing
        prof.keep_frames = enter!(keep_frames)
    end

    # start decoding backtraces
    location_id = Vector{eltype(data)}()
    lastwaszero = true

    for ip in data
        # ip == 0x0 is the sentinel value for finishing a backtrace, therefore finising a sample
        if ip == 0
            # Avoid creating empty samples
            if lastwaszero
                @assert length(location_id) == 0
                continue
            end

            # End of sample
            value = [
                1,                   # events
                length(location_id), # stack_depth
            ]
            push!(prof.sample, Sample(;location_id = location_id, value = value))
            location_id = Vector{eltype(data)}()
            lastwaszero = true
            continue
        end
        lastwaszero = false

        # A backtrace consists of a set of IP (Instruction Pointers), each IP points
        # a single line of code and `litrace` has the necessary information to decode
        # that IP to a specific frame (or set of frames, if inlining occured).

        push!(location_id, ip)
        # if we have already seen this IP avoid decoding it again
        haskey(locs, ip) && continue

        # Decode the IP into information about this stack frame (or frames given inlining)
        location = Location(;id = ip, address = ip, line=[])
        for frame in lookup(ip)
            # ip 0 is reserved
            frame.pointer == 0 && continue

            push!(location.line, Line(function_id = frame.pointer, line = frame.line))
            # Known function
            haskey(funcs, frame.pointer) && continue

            # Store the function in our functions dict
            funcProto = Function()
            funcProto.id = frame.pointer
            if frame.linfo !== nothing && frame.linfo isa Core.MethodInstance
                linfo = frame.linfo::Core.MethodInstance
                meth = linfo.def
                file = Base.find_source_file(string(meth.file))
                funcProto.name       = enter!(string(meth.module, ".", meth.name))
                funcProto.filename   = enter!(file)
                funcProto.start_line = convert(Int64, meth.line)
            else
                # frame.linfo either nothing or CodeInfo, either way fallback
                file = Base.find_source_file(string(frame.file))
                file_repr = file == nothing ? "nothing" : file
                funcProto.name = enter!(string(frame.func))
                funcProto.filename = enter!(file_repr)
                funcProto.start_line = convert(Int64, frame.line) # TODO: Get start_line properly
            end
            funcProto.system_name = funcProto.name
            funcs[frame.pointer] = funcProto
        end
        locs[ip] = location
    end

    # Build Profile
    prof.string_table = collect(keys(string_table))
    prof._function = collect(values(funcs))
    prof.location  = collect(values(locs))

    # Write to
    open(out, "w") do io
        writeproto(io, prof)
    end

    out
end

end # module
