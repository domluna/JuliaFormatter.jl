# This file provides an introductory tour of Onda.jl by generating, storing,
# and loading a toy Onda dataset. Run lines in the REPL to inspect output at
# each step! Tests are littered throughout to demonstrate functionality in a
# concrete manner, and so that we can ensure examples stay updated as the
# package evolves.

# NOTE: It's helpful to read https://github.com/beacon-biosignals/OndaFormat
# before and/or alongside the completion of this tour.

using Onda, Dates, Test

###############################################################################
###############################################################################
###############################################################################
# Let's start by defining some `Signal` instances to play with. A `Signal` instance
# describes a multichannel, LPCM-encodable signal as defined by the Onda format
# specification; this type corresponds directly to the signal object defined
# by the specification.

eeg_signal = Signal(;
                    channel_names=[:fp1, :f3, :c3, :p3, :f7, :t3, :t5, :o1, :fz, :cz, :pz,
                                   :fp2, :f4, :c4, :p4, :f8, :t4, :t6, :o2],
                    start_nanosecond=Nanosecond(0), stop_nanosecond=Nanosecond(Second(20)),
                    sample_unit=:microvolts, sample_resolution_in_unit=0.25,
                    sample_offset_in_unit=0.0, sample_type=Int16, sample_rate=256.0, # Hz
                    file_extension=:lpcm, file_options=nothing)

ecg_signal = signal_from_template(eeg_signal; channel_names=[:avl, :avr],
                                  file_extension=Symbol("lpcm.zst"))

spo2_signal = Signal(; channel_names=[:spo2], start_nanosecond=Nanosecond(Second(3)),
                     stop_nanosecond=Nanosecond(Second(17)), sample_unit=:percentage,
                     sample_resolution_in_unit=(100 / typemax(UInt8)),
                     sample_offset_in_unit=0.0, sample_type=UInt8, sample_rate=20.5, # Hz
                     file_extension=:lpcm, file_options=nothing)

###############################################################################
###############################################################################
###############################################################################
# Next, we'll generate some fake sample data for each of our signals. Here, we'll
# be working with the `Samples` type. This type wraps a `Signal` and a corresponding
# matrix of interleaved LPCM-encoded (or decoded) sample data. In this matrix,
# the rows correspond to channels and the columns correspond to timesteps.

# We'll use this function to generate the actual dummy data for our signals. As
# an aside: The hypothetical person from which these hypothetical signals were
# hypothetically recorded must be experiencing some pretty crazy pathologies if
# their EEG/ECG are just saw waves...
function saws(signal)
    return [(j + i) % 100 * signal.sample_resolution_in_unit
            for i in 1:channel_count(signal), j in 1:sample_count(signal)]
end

# The second argument in the `Samples` constructor is a `Bool` that specifies if
# the data is in its encoded representation. Here, we construct our signals as
# "decoded" (i.e. in actual units, though for this toy example it doesn't really
# matter) and then "encode" them according to the specified:
eeg = encode(Samples(eeg_signal, false, saws(eeg_signal)))
ecg = encode(Samples(ecg_signal, false, saws(ecg_signal)))
spo2 = encode(Samples(spo2_signal, false, saws(spo2_signal)))

# Here are some basic functions for examining `Samples` instances:
@test sample_count(eeg) == sample_count(eeg_signal) == 20 * eeg_signal.sample_rate
@test channel_count(eeg) == channel_count(eeg_signal) == 19
@test channel(eeg, :f3) == channel(eeg_signal, :f3) == 2
@test channel(eeg, 2) == channel(eeg_signal, 2) == :f3
@test duration(eeg) == duration(span(eeg_signal)) == Second(20)

# Here are some basic indexing examples using `getindex` and `view` wherein
# channel names and sample-rate-agnostic `TimeSpan`s are employed as indices:
slice_span = TimeSpan(Second(3), Second(9))
span_range = index_from_time(eeg.signal.sample_rate, slice_span)
@test eeg[:, slice_span].data == view(eeg, :, span_range).data
@test eeg[:f3, :].data == view(eeg, 2, :).data
@test eeg[:f3, 1:10].data == view(eeg, 2, 1:10).data
@test eeg[:f3, slice_span].data == view(eeg, 2, span_range).data
@test eeg[[:f3, :c3, :p3], 1:10].data == view(eeg, 2:4, 1:10).data
@test eeg[[:c3, 4, :f3], slice_span].data == view(eeg, [3, 4, 2], span_range).data

# NOTE: Keep in mind that `duration(samples.signal)` is not generally equivalent
# to `duration(samples)`; the former is the duration of the original signal in
# the context of its parent recording, whereas the latter is the actual duration
# of `samples.data` given `signal.sample_rate`. This is similarly true for the
# `sample_count` function for the same reason!
eeg_slice = eeg[:, slice_span]
@test duration(eeg_slice) == duration(slice_span)
@test duration(eeg_slice) != duration(eeg_signal)
@test sample_count(eeg_slice) == length(span_range)
@test sample_count(eeg_slice) != sample_count(eeg_signal)

# NOTE: `Samples` is not an `AbstractArray` subtype; this special indexing
# behavior is only defined for convenient data manipulation. It is thus fine
# to access the sample data matrix directly via the `data` field if you need
# to manipulate the matrix directly or pass it to downstream computations.

###############################################################################
###############################################################################
###############################################################################
# Now that we have some actual sample data for some actual signals, let's write
# it all out as an individual recording to an Onda dataset.

root = mktempdir() # this will be deleted when the Julia process exits

# Create a `Dataset` instance. This is a thin wrapper around an `example.onda`
# directory that helps us to easily interface the Onda dataset in a compliant
# manner.
dataset = Dataset(joinpath(root, "example.onda"); create=true)

# Create a `Recording` instance within `dataset`. This object corresponds
# directly to the recording MessagePack object defined by the specification.
# NOTE: Importantly, `create_recording!` adds `uuid => recording` to the
# `dataset.recordings` dictionary before returning the pair, such that the
# `recording` variable we assign here references the same `Recording` instance
# stored within `dataset`.
uuid, recording = create_recording!(dataset)

# Store our signals/samples for the recording in our `dataset`. This both serializes
# sample data to disk and adds the signal metadata to the recording stored in
# `dataset.recordings[uuid]` (which, for us, happens to be `recording`).
store!(dataset, uuid, :eeg, eeg)
store!(dataset, uuid, :ecg, ecg)
store!(dataset, uuid, :spo2, spo2)

# Add a single `Annotation` to `recording`. An `Annotation` is simply a key-value
# pair with an associated `TimeSpan`; you can basically put any strings you'd
# like in there. For example, Beacon Biosignals stores JSON snippets in
# annotations. Here, let's just go the simple route and pretend we found an
# epileptiform spike in our EEG/ECG/SpO2 recording:
spike_annotation = Annotation("epileptiform_spike", TimeSpan(Millisecond(1500), Second(2)))
annotate!(recording, spike_annotation)

# You can add as many annotations as you'd like to a recording. Just keep in mind
# that the annotation list is a `Set`, so duplicates will be ignored:
annotate!(recording, spike_annotation)
@test length(recording.annotations) == 1

# Since our hypothetical subject already has hypothetical epilepsy, let's give
# them hypothetical narcolepsy as well by annotating sleep stages over insanely
# short 2 second epochs across the entire recording:
for (i, t) in enumerate(2:2:(Second(duration(recording)).value))
    stage = rand(["awake", "nrem1", "nrem2", "nrem3", "rem"])
    ann = Annotation(stage, TimeSpan(Second(t - 2), Second(t)))
    annotate!(recording, ann)
end

# Finally, we save `dataset.recordings` to the `recordings.msgpack.zst` file
# specified by the Onda format. NOTE: If you don't call this function, your
# changes to `dataset.recordings` will not be saved!
save_recordings_file(dataset)

###############################################################################
###############################################################################
###############################################################################
# We have a dataset! At this point, let's pretend that we weren't the ones that
# wrote out this dataset; instead, we'll pretend our colleague passed it off to
# us, and we have to load it up to check for spikes.

dataset = Dataset(joinpath(root, "example.onda"))
@test length(dataset.recordings) == 1
uuid, recording = first(dataset.recordings)

# Grab the first spike annotation we see...
spike_annotation = first(ann
                         for ann in recording.annotations
                         if ann.value == "epileptiform_spike")

# ...and load that segment of the EEG from disk as a `Samples` instance!
spike_segment = load(dataset, uuid, :eeg, spike_annotation)

# The above invocation of `load` allows a segment of a signal (as specified by
# the last argument) to be read/deserialized from disk without reading the full
# signal file. It actually works with `Annotation`s because it accepts any
# `AbstractTimeSpan`, and `Annotation <: AbstractTimeSpan`:
@test TimeSpan(spike_annotation) ==
      TimeSpan(first(spike_annotation), last(spike_annotation))
@test spike_segment.data == load(dataset, uuid, :eeg)[:, spike_annotation].data
@test spike_segment.data == load(dataset, uuid, :eeg, TimeSpan(spike_annotation)).data

# NOTE: `load(..., span)` may still have to read the entire signal from disk if
# the signal's file format doesn't support seek access. For this reason - and
# simply to avoid filesystem overhead - it is often better to load the whole
# signal first if you're going to be accessing a bunch of its segments.

# Welp, looks like a spike to me! Let's leave an annotation to confirm we
# checked it. Remember - `spike_annotation isa AbstractTimeSpan`, so we can
# generally pass it wherever we'd pass a `TimeSpan` object:
annotate!(recording, Annotation("confirmed_spike_by_me", spike_annotation))

# ...and, finally, of course, let's save our annotation!
save_recordings_file(dataset)
