#####
##### `show` methods
#####

function Base.show(io::IO, w::TimeSpan)
    start_string = format_duration(first(w))
    stop_string = format_duration(last(w))
    return print(io, "TimeSpan(", start_string, ", ", stop_string, ')')
end

function Base.show(io::IO, samples::Samples)
    return if get(io, :compact, false)
        print(io, "Samples(", summary(samples.data), ')')
    else
        duration_in_seconds = size(samples.data, 2) / samples.signal.sample_rate
        duration_in_nanoseconds = round(Int, duration_in_seconds * 1_000_000_000)
        println(io, "Samples (", format_duration(duration_in_nanoseconds), "):")
        println(io, "  signal.channel_names: ",
                channel_names_string(samples.signal.channel_names))
        println(io, "  signal.start_nanosecond: ", samples.signal.start_nanosecond, " (",
                format_duration(samples.signal.start_nanosecond), ")")
        println(io, "  signal.stop_nanosecond: ", samples.signal.stop_nanosecond, " (",
                format_duration(samples.signal.stop_nanosecond), ")")
        println(io, "  signal.sample_unit: ", repr(samples.signal.sample_unit))
        println(io, "  signal.sample_resolution_in_unit: ",
                samples.signal.sample_resolution_in_unit)
        println(io, "  signal.sample_offset_in_unit: ",
                samples.signal.sample_offset_in_unit)
        println(io, "  signal.sample_type: ", samples.signal.sample_type)
        println(io, "  signal.sample_rate: ", samples.signal.sample_rate, " Hz")
        println(io, "  signal.file_extension: ", repr(samples.signal.file_extension))
        println(io, "  signal.file_options: ", repr(samples.signal.file_options))
        println(io, "  encoded: ", samples.encoded)
        println(io, "  data:")
        show(io, "text/plain", samples.data)
    end
end

function Base.show(io::IO, signal::Signal)
    return if get(io, :compact, false)
        print(io, "Signal(", channel_names_string(signal.channel_names), ")")
    else
        println(io, "Signal:")
        println(io, "  channel_names: ", channel_names_string(signal.channel_names))
        println(io, "  start_nanosecond: ", signal.start_nanosecond, " (",
                format_duration(signal.start_nanosecond), ")")
        println(io, "  stop_nanosecond: ", signal.stop_nanosecond, " (",
                format_duration(signal.stop_nanosecond), ")")
        println(io, "  sample_unit: ", repr(signal.sample_unit))
        println(io, "  sample_resolution_in_unit: ", signal.sample_resolution_in_unit)
        println(io, "  sample_offset_in_unit: ", signal.sample_offset_in_unit)
        println(io, "  sample_type: ", signal.sample_type)
        println(io, "  sample_rate: ", signal.sample_rate, " Hz")
        println(io, "  file_extension: ", repr(signal.file_extension))
        print(io, "  file_options: ", repr(signal.file_options))
    end
end

function Base.show(io::IO, recording::Recording)
    return if get(io, :compact, false)
        duration_string = isempty(recording.signals) ? "<no signals>" :
                          format_duration(duration(recording))
        print(io, "Recording(", duration_string, ')')
    else
        if isempty(recording.signals)
            duration_string = "<no signals>"
        else
            duration_string = string(format_duration(duration(recording)), "; ",
                                     duration(recording).value / 1_000_000_000, " seconds")
        end
        println(io, "Recording (", duration_string, ')')
        println(io, "  signals:")
        compact_io = IOContext(io, :compact => true)
        for (name, signal) in recording.signals
            println(compact_io, "    :", name, " => ", signal)
        end
        print(io, "  annotations: (", length(recording.annotations), " total)")
    end
end

function Base.show(io::IO, dataset::Dataset)
    return print(io, "Dataset(", dataset.path, ", ", length(dataset.recordings),
                 " recordings)")
end

#####
##### utilities
#####

function channel_names_string(channel_names)
    return string('[', join(map(repr, channel_names), ", "), ']')
end

function nanosecond_to_periods(ns::Integer)
    μs, ns = divrem(ns, 1000)
    ms, μs = divrem(μs, 1000)
    s, ms = divrem(ms, 1000)
    m, s = divrem(s, 60)
    hr, m = divrem(m, 60)
    return (hr, m, s, ms, μs, ns)
end

format_duration(t::Period) = format_duration(convert(Nanosecond, t).value)

function format_duration(ns::Integer)
    hr, m, s, ms, μs, ns = nanosecond_to_periods(ns)
    hr = lpad(hr, 2, '0')
    m = lpad(m, 2, '0')
    s = lpad(s, 2, '0')
    ms = lpad(ms, 3, '0')
    μs = lpad(μs, 3, '0')
    ns = lpad(ns, 3, '0')
    return string(hr, ':', m, ':', s, '.', ms, μs, ns)
end
