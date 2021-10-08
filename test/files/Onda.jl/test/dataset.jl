using Test, Onda, Dates, MsgPack

@testset "round trip" begin
    mktempdir() do root
        # generate a test dataset
        dataset = Dataset(joinpath(root, "test"); create=true)
        @test dataset isa Dataset
        @test isdir(dataset.path)
        @test isdir(joinpath(dataset.path, "samples"))
        duration_in_seconds = Second(10)
        duration_in_nanoseconds = Nanosecond(duration_in_seconds)
        uuid, recording = create_recording!(dataset)
        Ts = (UInt8, UInt16, UInt32, UInt64, Int8, Int16, Int32, Int64)
        sample_rate = 50.5
        signals = Dict(Symbol(:x, i) => Signal(Symbol.([:a, :b, :c], i), Nanosecond(0),
                                               duration_in_nanoseconds, Symbol(:unit, i),
                                               0.25, i, T, sample_rate, Symbol("lpcm.zst"),
                                               nothing) for (i, T) in enumerate(Ts))
        samples = Dict(k => Samples(v, true, rand(v.sample_type, 3, sample_count(v)))
                       for (k, v) in signals)
        for (name, s) in samples
            @test channel_count(s) == length(s.signal.channel_names)
            @test channel_count(s.signal) == length(s.signal.channel_names)
            @test sample_count(s) == size(s.data, 2)
            @test sizeof_samples(s.signal) == sizeof(s.data)
            @test encode(s) === s
            tmp = similar(s.data)
            encode!(tmp, s)
            @test tmp == s.data
            d = decode(s)
            @test decode(d) === d
            tmp = similar(d.data)
            decode!(tmp, d)
            @test tmp == d.data
            tmp = similar(d.data)
            decode!(tmp, s)
            @test tmp == d.data
            tmp = similar(d.data)
            decode!(tmp, s.signal.sample_resolution_in_unit, s.signal.sample_offset_in_unit,
                    s.data)
            @test tmp == d.data
            @test d.data == (s.data .* s.signal.sample_resolution_in_unit .+
                             s.signal.sample_offset_in_unit)
            if sizeof(s.signal.sample_type) >= 8
                # decoding from 64-bit to floating point is fairly lossy
                tmp = similar(s.data)
                @test isapprox(encode(d, nothing).data, s.data; rtol=10)
                encode!(tmp, d, nothing)
                @test isapprox(tmp, s.data; rtol=10)
                @test isapprox(encode(d, missing).data, s.data; rtol=10)
                encode!(tmp, d, missing)
                @test isapprox(tmp, s.data; rtol=10)
            else
                tmp = similar(s.data)
                encode!(tmp, d, nothing)
                @test tmp == s.data
                @test encode(d, nothing).data == s.data
                encode!(tmp, d, missing)
                @test isapprox(tmp, s.data; rtol=1)
                @test isapprox(encode(d, missing).data, s.data; rtol=1)
            end
            chs = s.signal.channel_names
            i = 93
            @test s[:, i].data == s.data[:, i:i]
            t = Onda.time_from_index(s.signal.sample_rate, i)
            t2 = t + Nanosecond(Second(3))
            j = Onda.index_from_time(s.signal.sample_rate, t2) - 1
            for ch_inds in (:, 1:2, 2:3, 1:3, [3, 1], [2, 3, 1], [1, 2, 3])
                @test s[chs[ch_inds], t].data == s[ch_inds, i].data
                @test s[chs[ch_inds], TimeSpan(t, t2)].data == s.data[ch_inds, i:j]
                @test s[chs[ch_inds], i:j].data == s.data[ch_inds, i:j]
                @test s[ch_inds, t].data == s[ch_inds, i].data
                @test s[ch_inds, TimeSpan(t, t2)].data == s.data[ch_inds, i:j]
                @test s[ch_inds, i:j].data == s.data[ch_inds, i:j]
            end
            @test size(s[:, TimeSpan(0, Second(1))].data, 2) == floor(s.signal.sample_rate)
            for i in 1:length(chs)
                @test channel(s, chs[i]) == i
                @test channel(s, i) == chs[i]
                @test channel(s.signal, chs[i]) == i
                @test channel(s.signal, i) == chs[i]
            end
            @test duration(s) == duration_in_seconds
            @test s[:, TimeSpan(0, duration(s))].data == s.data
            store!(dataset, uuid, name, s)
        end
        save_recordings_file(dataset)

        # read back in the test dataset, add some annotations
        old_dataset = dataset
        dataset = Dataset(joinpath(root, "test"))
        @test length(dataset.recordings) == 1
        uuid, recording = first(dataset.recordings)
        x1 = load(dataset, uuid, :x1)
        @test x1.signal == signals[:x1]
        xs = load(dataset, uuid, (:x3, :x2))
        @test xs[:x3].signal == signals[:x3]
        @test xs[:x2].signal == signals[:x2]
        xs = load(dataset, uuid)
        span = TimeSpan(Second(1), Second(2))
        xs_span = load(dataset, uuid, span)
        for (name, s) in samples
            xi = xs[name]
            @test xi.signal == signals[name]
            @test xi.encoded
            @test xi.data == s.data
            @test xi[:, span].data == xs_span[name].data
        end
        for i in 1:3
            annotate!(recording,
                      Annotation("value_$i", Nanosecond(i),
                                 Nanosecond(i + rand(1:1000000))))
        end
        save_recordings_file(dataset)

        # read back in annotations
        old_uuid = uuid
        old_recording = recording
        old_dataset = dataset
        dataset = Dataset(joinpath(root, "test"))
        uuid, recording = first(dataset.recordings)
        @test old_recording == recording
        delete!(dataset.recordings, uuid)
        uuid, recording = create_recording!(dataset)
        foreach(x -> annotate!(recording, x), old_recording.annotations)
        foreach(x -> store!(dataset, uuid, x, load(old_dataset, old_uuid, x)),
                keys(old_recording.signals))
        merge!(dataset, old_dataset; only_recordings=true)
        @test length(dataset.recordings) == 2
        r1 = dataset.recordings[old_uuid]
        @test r1 == old_recording
        r2 = dataset.recordings[uuid]
        @test r2 == recording
        @test old_uuid != uuid
        @test r1.signals == r2.signals
        @test r1.annotations == r2.annotations

        old_duration = duration(r2)
        new_duration = old_duration + Nanosecond(1)
        r2signals = set_span!(r2, TimeSpan(Nanosecond(0), new_duration))
        @test keys(r2signals) == keys(r2.signals)
        @test all(duration.(values(r2signals)) .== new_duration)
        set_span!(r2, TimeSpan(Nanosecond(0), old_duration))

        r = dataset.recordings[uuid]
        original_signals_length = length(r.signals)
        signal_name, signal = first(r.signals)
        signal_samples = load(dataset, uuid, signal_name)
        signal_samples_path = samples_path(dataset, uuid, signal_name)
        delete!(dataset, uuid, signal_name)
        @test r === dataset.recordings[uuid]
        @test length(r.signals) == (original_signals_length - 1)
        @test !haskey(r.signals, signal_name)
        @test !isfile(signal_samples_path)
        store!(dataset, uuid, signal_name, signal_samples)

        # read back everything, but without assuming an order on the metadata
        dataset = Dataset(joinpath(root, "test"))
        Onda.write_recordings_file(dataset.path,
                                   Onda.Header(dataset.header.onda_format_version, false),
                                   dataset.recordings)
        dataset = Dataset(joinpath(root, "test"))
        @test Dict(old_uuid => old_recording) == dataset.recordings
        delete!(dataset, old_uuid)
        save_recordings_file(dataset)

        # read back the dataset that should now be empty
        dataset = Dataset(joinpath(root, "test"))
        @test isempty(dataset.recordings)
        @test !isdir(joinpath(dataset.path, "samples", string(old_uuid)))

        # make sure samples directory is appropriately created if not present
        no_samples_path = joinpath(root, "no_samples_dir.onda")
        mkdir(no_samples_path)
        cp(joinpath(dataset.path, "recordings.msgpack.zst"),
           joinpath(no_samples_path, "recordings.msgpack.zst"))
        Dataset(no_samples_path; create=false)
        return @test isdir(joinpath(no_samples_path, "samples"))
    end
end

@testset "Error conditions" begin
    mktempdir() do root
        mkdir(joinpath(root, "i_exist.onda"))
        touch(joinpath(root, "i_exist.onda", "memes"))
        @test_throws ArgumentError Dataset(joinpath(root, "i_exist.onda"); create=true)

        dataset = Dataset(joinpath(root, "okay.onda"); create=true)
        uuid, recording = create_recording!(dataset)
        signal = Signal([:a], Nanosecond(0), Nanosecond(Second(10)), :mv, 0.25, 0.0, Int8,
                        100, Symbol("lpcm.zst"), nothing)
        @test_throws DimensionMismatch Samples(signal, true, rand(Int8, 2, 10))
        @test_throws ArgumentError Samples(signal, true, rand(Float32, 1, 10))
        samples = Samples(signal, true, rand(Int8, 1, 10 * 100))
        @test_throws ArgumentError store!(dataset, uuid, Symbol("***HI***"), samples)
        store!(dataset, uuid, :name_okay, samples)
        @test_throws ArgumentError store!(dataset, uuid, :name_okay, samples;
                                          overwrite=false)

        @test_throws ArgumentError Annotation("hi", Nanosecond(20), Nanosecond(4))

        mkdir(joinpath(root, "other.onda"))
        other = Dataset(joinpath(root, "other.onda"); create=true)  # Using existing empty directory
        create_recording!(other, uuid)
        @test_throws ArgumentError create_recording!(other, uuid)
        store!(other, uuid, :cool_stuff, samples)
        @test_throws ErrorException merge!(dataset, other; only_recordings=false)
        return @test_throws ArgumentError merge!(dataset, other; only_recordings=true)
    end
end

@testset "upgrade_onda_format_from_v0_2_to_v0_3!" begin
    mktempdir() do new_path
        old_path = joinpath(@__DIR__, "old_test_v0_2.onda")
        cp(old_path, new_path; force=true)
        dataset = Onda.upgrade_onda_format_from_v0_2_to_v0_3!(new_path,
                                                              (k, v) -> string(k, '.', v))
        @test dataset.path == new_path
        @test dataset.header.onda_format_version == v"0.3.0"
        @test dataset.header.ordered_keys
        old_recordings = MsgPack.unpack(Onda.zstd_decompress(read(joinpath(old_path,
                                                                           "recordings.msgpack.zst"))))[2]
        new_customs = MsgPack.unpack(Onda.zstd_decompress(read(joinpath(new_path,
                                                                        "recordings_custom.msgpack.zst"))))
        @test length(dataset.recordings) == 1
        @test length(new_customs) == 1
        uuid = first(keys(dataset.recordings))
        recording = first(values(dataset.recordings))
        old_recording = first(values(old_recordings))
        @test string(uuid) == first(keys(new_customs)) == first(keys(old_recordings))
        @test first(values(new_customs)) == old_recording["custom"]
        sorted_annotations = sort(collect(recording.annotations); by=first)
        sorted_old_annotations = sort(old_recording["annotations"];
                                      by=(x -> x["start_nanosecond"]))
        @test length(sorted_annotations) == length(sorted_old_annotations)
        for (ann, old_ann) in zip(sorted_annotations, sorted_old_annotations)
            @test ann.value == string(old_ann["key"], '.', old_ann["value"])
            @test ann.start_nanosecond.value == old_ann["start_nanosecond"]
            @test ann.stop_nanosecond.value == old_ann["stop_nanosecond"]
        end
        old_signals = old_recording["signals"]
        @test keys(recording.signals) == Set(Symbol.(keys(old_signals)))
        return for (signal_name, signal) in recording.signals
            old_signal = old_signals[string(signal_name)]
            @test signal.channel_names == Symbol.(old_signal["channel_names"])
            @test signal.start_nanosecond == Nanosecond(0)
            @test signal.stop_nanosecond ==
                  Nanosecond(old_recording["duration_in_nanoseconds"])
            @test signal.sample_unit == Symbol(old_signal["sample_unit"])
            @test signal.sample_resolution_in_unit ==
                  old_signal["sample_resolution_in_unit"]
            @test signal.sample_offset_in_unit == 0.0
            @test signal.sample_type ==
                  Onda.julia_type_from_onda_sample_type(old_signal["sample_type"])
            @test signal.sample_rate == old_signal["sample_rate"]
            @test signal.file_extension == Symbol(old_signal["file_extension"])
            @test signal.file_options == old_signal["file_options"]
        end
    end
end
