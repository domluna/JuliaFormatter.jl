module PProf

export pprof, @pprof

using Profile
using ProtoBuf
using OrderedCollections

using Profile: clear

"""
    PProf.clear()

Alias for `Profile.clear()`
"""
clear

# Load in `deps.jl`, complaining if it does not exist
const depsjl_path = joinpath(@__DIR__, "..", "deps", "deps.jl")
if !isfile(depsjl_path)
    error(
        "PProf not installed properly, run Pkg.build(\"PProf\"), restart Julia and try again",
    )
end
include(depsjl_path)

# Module initialization function
function __init__()
    # Always check your dependencies from `deps.jl`
    check_deps()
end


include(joinpath("..", "lib", "perftools.jl"))

import .perftools.profiles: ValueType, Sample, Function, Location, Line
const PProfile = perftools.profiles.Profile

const proc = Ref{Union{Base.Process,Nothing}}(nothing)

"""
    _enter!(dict::OrderedDict{T, Int64}, key::T) where T

Resolves from `key` to the index (zero-based) in the dict.
Useful for the Strings table

NOTE: We must use Int64 throughout this package (regardless of system word-size) b/c the
proto file specifies 64-bit integers.
"""
function _enter!(dict::OrderedDict{T,Int64}, key::T) where {T}
    if haskey(dict, key)
        return dict[key]
    else
        l = Int64(length(dict))
        dict[key] = l
        return l
    end
end

using Base.StackTraces: lookup, StackFrame

# TODO:
# - Mappings

"""
    pprof(data, period;
            web = true, webhost = "localhost", webport = 57599,
            out = "profile.pb.gz", from_c = true, drop_frames = "", keep_frames = "")

Fetches the collected `Profile` data, exports to the `pprof` format, and (optionally) opens
a `pprof` web-server for interactively viewing the results.

If `web=true`, the web-server is opened in the background. Re-running `pprof()` will refresh
the web-server to use the new output.

If you manually edit the output file, `PProf.refresh()` will refresh the server without
overwriting the output file. `PProf.kill()` will kill the server.

# Arguments:

  - `data::Vector{UInt}`: The data provided by `Profile.fetch` \[optional\].
  - `period::UInt64`: The sampling period in nanoseconds \[optional\].

# Keyword Arguments

  - `web::Bool`: Whether to launch the `go tool pprof` interactive webserver for viewing results.
  - `webhost::AbstractString`: If using `web`, which host to launch the webserver on.
  - `webport::Integer`: If using `web`, which port to launch the webserver on.
  - `out::String`: Filename for output.
  - `from_c::Bool`: If `false`, exclude frames that come from from\_c. Defaults to `true`.
  - `drop_frames`: frames with function\_name fully matching regexp string will be dropped from the samples,
    along with their successors.
  - `keep_frames`: frames with function\_name fully matching regexp string will be kept, even if it matches drop\_functions.
"""
function pprof(
    data::Union{Nothing,Vector{UInt}} = nothing,
    period::Union{Nothing,UInt64} = nothing;
    web::Bool = true,
    webhost::AbstractString = "localhost",
    webport::Integer = 57599,
    out::AbstractString = "profile.pb.gz",
    from_c::Bool = true,
    drop_frames::Union{Nothing,AbstractString} = nothing,
    keep_frames::Union{Nothing,AbstractString} = nothing,
)
    if data === nothing
        data = copy(Profile.fetch())
    end
    if period === nothing
        period = ccall(:jl_profile_delay_nsec, UInt64, ())
    end

    string_table = OrderedDict{AbstractString,Int64}()
    enter!(string) = _enter!(string_table, string)
    enter!(::Nothing) = _enter!(string_table, "nothing")
    ValueType!(_type, unit) = ValueType(_type = enter!(_type), unit = enter!(unit))

    # Setup:
    enter!("")  # NOTE: pprof requires first entry to be ""
    # Functions need a uid, we'll use the pointer for the method instance
    seen_funcs = Set{UInt64}()
    funcs = Dict{UInt64,Function}()

    seen_locs = Set{UInt64}()
    locs = Dict{UInt64,Location}()
    locs_from_c = Dict{UInt64,Bool}()

    sample_type = [
        ValueType!("events", "count"), # Mandatory
        ValueType!("stack_depth", "count"),
    ]

    prof = PProfile(
        sample = [],
        location = [],
        _function = [],
        mapping = [],
        string_table = [],
        sample_type = sample_type,
        default_sample_type = 1, # events
        period = period,
        period_type = ValueType!("cpu", "nanoseconds"),
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
            push!(prof.sample, Sample(; location_id = location_id, value = value))
            location_id = Vector{eltype(data)}()
            lastwaszero = true
            continue
        end
        lastwaszero = false

        # A backtrace consists of a set of IP (Instruction Pointers), each IP points
        # a single line of code and `litrace` has the necessary information to decode
        # that IP to a specific frame (or set of frames, if inlining occured).

        # if we have already seen this IP avoid decoding it again
        if ip in seen_locs
            # Only keep C frames if from_c=true
            if (from_c || !locs_from_c[ip])
                push!(location_id, ip)
            end
            continue
        end
        push!(seen_locs, ip)

        # Decode the IP into information about this stack frame (or frames given inlining)
        location = Location(; id = ip, address = ip, line = [])
        location_from_c = true
        for frame in lookup(ip)
            # ip 0 is reserved
            frame.pointer == 0 && continue
            # if any of the frames is not from_c the entire location is not from_c
            location_from_c &= frame.from_c

            push!(location.line, Line(function_id = frame.pointer, line = frame.line))
            # Known function
            frame.pointer in seen_funcs && continue
            push!(seen_funcs, frame.pointer)

            # Store the function in our functions dict
            funcProto = Function()
            funcProto.id = frame.pointer
            file = nothing
            if frame.linfo !== nothing && frame.linfo isa Core.MethodInstance
                linfo = frame.linfo::Core.MethodInstance
                meth = linfo.def
                file = string(meth.file)
                funcProto.name = enter!(string(meth.module, ".", meth.name))
                funcProto.start_line = convert(Int64, meth.line)
            else
                # frame.linfo either nothing or CodeInfo, either way fallback
                file = string(frame.file)
                funcProto.name = enter!(string(frame.func))
                funcProto.start_line = convert(Int64, frame.line) # TODO: Get start_line properly
            end
            file = Base.find_source_file(file)
            funcProto.filename = enter!(file)
            funcProto.system_name = funcProto.name
            # Only keep C functions if from_c=true
            if (from_c || !frame.from_c)
                funcs[frame.pointer] = funcProto
            end
        end
        locs_from_c[ip] = location_from_c
        # Only keep C frames if from_c=true
        if (from_c || !location_from_c)
            locs[ip] = location
            push!(location_id, ip)
        end
    end

    # Build Profile
    prof.string_table = collect(keys(string_table))
    # If from_c=false funcs and locs should NOT contain C functions
    prof._function = collect(values(funcs))
    prof.location = collect(values(locs))

    # Write to disk
    open(out, "w") do io
        writeproto(io, prof)
    end

    if web
        refresh(webhost = webhost, webport = webport, file = out)
    end

    out
end

"""
    refresh(; webhost = "localhost", webport = 57599, file = "profile.pb.gz")

Start or restart the go pprof webserver.

  - `webhost::AbstractString`: Which host to launch the webserver on.
  - `webport::Integer`: Which port to launch the webserver on.
  - `file::String`: Profile file to open.
"""
function refresh(;
    webhost::AbstractString = "localhost",
    webport::Integer = 57599,
    file::AbstractString = "profile.pb.gz",
)

    if proc[] === nothing
        # The first time, register an atexit hook to kill the web server.
        atexit(PProf.kill)
    else
        # On subsequent calls, restart the pprof web server.
        Base.kill(proc[])
    end

    proc[] = open(pipeline(`$go_pprof -http=$webhost:$webport $file`))
end

"""
    pprof_kill()

Kills the pprof server if running.
"""
function kill()
    if proc[] !== nothing
        Base.kill(proc[])
        proc[] = nothing
    end
end


"""
    @pprof ex

Profiles the expression using `@profile` and starts or restarts `pprof`.
"""
macro pprof(ex)
    esc(quote
        $Profile.@profile $ex
        $(@__MODULE__).pprof()
    end)
end

end # module
