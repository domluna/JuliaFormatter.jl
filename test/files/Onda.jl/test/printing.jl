using Test, Onda, Dates, Random, UUIDs

@testset "pretty printing" begin
    @test repr(TimeSpan(6149872364198, 123412345678910)) ==
          "TimeSpan(01:42:29.872364198, 34:16:52.345678910)"

    signal = Signal([:a, :b, Symbol("c-d")], Nanosecond(3), Nanosecond(Second(12345)),
                    :unit, 0.25, -0.5, Int16, 50.2, Symbol("lpcm.zst"), nothing)
    @test sprint(show, signal; context=(:compact => true)) ==
          "Signal([:a, :b, Symbol(\"c-d\")])"
    @test sprint(show, signal) == """
                                  Signal:
                                    channel_names: [:a, :b, Symbol(\"c-d\")]
                                    start_nanosecond: 3 nanoseconds (00:00:00.000000003)
                                    stop_nanosecond: 12345000000000 nanoseconds (03:25:45.000000000)
                                    sample_unit: :unit
                                    sample_resolution_in_unit: 0.25
                                    sample_offset_in_unit: -0.5
                                    sample_type: Int16
                                    sample_rate: 50.2 Hz
                                    file_extension: Symbol(\"lpcm.zst\")
                                    file_options: nothing"""

    samples = Samples(signal, true,
                      rand(Random.MersenneTwister(0), signal.sample_type, 3, 5))
    @test sprint(show, samples; context=(:compact => true)) == "Samples(3×5 Array{Int16,2})"
    @test sprint(show, samples) == """
                                   Samples (00:00:00.099601594):
                                     signal.channel_names: [:a, :b, Symbol(\"c-d\")]
                                     signal.start_nanosecond: 3 nanoseconds (00:00:00.000000003)
                                     signal.stop_nanosecond: 12345000000000 nanoseconds (03:25:45.000000000)
                                     signal.sample_unit: :unit
                                     signal.sample_resolution_in_unit: 0.25
                                     signal.sample_offset_in_unit: -0.5
                                     signal.sample_type: Int16
                                     signal.sample_rate: 50.2 Hz
                                     signal.file_extension: Symbol(\"lpcm.zst\")
                                     signal.file_options: nothing
                                     encoded: true
                                     data:
                                   3×5 Array{Int16,2}:
                                    20032  4760  27427  -20758   24287
                                    14240  5037   5598   -5888   21784
                                    16885   600  20880  -32493  -19305"""
    annotations = Set(Annotation("val", TimeSpan(i, i + 1)) for i in 1:10)
    recording = Recording(Dict(:test => signal), annotations)
    @test sprint(show, recording) == """
                                     Recording (03:25:45.000000000; 12345.0 seconds)
                                       signals:
                                         :test => Signal([:a, :b, Symbol(\"c-d\")])
                                       annotations: (10 total)"""
    mktempdir() do root
        dataset = Dataset(joinpath(root, "test.onda"); create=true)
        return @test sprint(show, dataset) == "Dataset($(dataset.path), 0 recordings)"
    end
end
