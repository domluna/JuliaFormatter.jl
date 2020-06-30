"""
    AbstractTimeSpan

A type repesenting a continuous, inclusive span between two points in time.

All subtypes of `AbstractTimeSpan` must implement:

  - `first(::AbstractTimeSpan)::Nanosecond`: return the first nanosecond contained in `span`
  - `last(::AbstractTimeSpan)::Nanosecond`: return the last nanosecond contained in `span`

For convenience, many Onda functions that accept `AbstractTimeSpan` values also accept
`Dates.Period` values.

See also: [`TimeSpan`](@ref)
"""
abstract type AbstractTimeSpan end

Base.first(t::Period) = convert(Nanosecond, t)

Base.last(t::Period) = convert(Nanosecond, t)

function _validate_timespan(first::Nanosecond, last::Nanosecond)
    if first > last
        throw(ArgumentError("start of time span should precede end, got $first and $last"))
    end
    return nothing
end

#####
##### `TimeSpan`
#####

"""
    TimeSpan(first, last)

Return `TimeSpan(Nanosecond(first), Nanosecond(last))::AbstractTimeSpan`.

See also: [`AbstractTimeSpan`](@ref)
"""
struct TimeSpan <: AbstractTimeSpan
    first::Nanosecond
    last::Nanosecond
    function TimeSpan(first::Nanosecond, last::Nanosecond)
        _validate_timespan(first, last)
        return new(first, last)
    end
    TimeSpan(first, last) = TimeSpan(Nanosecond(first), Nanosecond(last))
end

"""
    TimeSpan(x)

Return `TimeSpan(first(x), last(x))`.

See also: [`AbstractTimeSpan`](@ref)
"""
TimeSpan(x) = TimeSpan(first(x), last(x))

Base.first(span::TimeSpan) = span.first

Base.last(span::TimeSpan) = span.last

#####
##### `AbstractTimeSpan` Utilities
#####

"""
    contains(a, b)

Return `true` if the timespan `b` lies entirely within the timespan `a`, return `false` otherwise.
"""
function contains(a, b)
    a, b = TimeSpan(a), TimeSpan(b)
    return first(a) <= first(b) && last(a) >= last(b)
end

"""
    overlaps(a, b)

Return `true` if the timespan `a` and the timespan `b` overlap, return `false` otherwise.
"""
function overlaps(a, b)
    a, b = TimeSpan(a), TimeSpan(b)
    starts_earlier, starts_later = ifelse(first(b) > first(a), (a, b), (b, a))
    return last(starts_earlier) >= first(starts_later)
end

"""
    shortest_timespan_containing(spans)

Return the shortest possible `TimeSpan` containing all timespans in `spans`.

`spans` is assumed to be an iterable of timespans.
"""
function shortest_timespan_containing(spans)
    isempty(spans) && throw(ArgumentError("input iterator must be nonempty"))
    lo, hi = Nanosecond(typemax(Int64)), Nanosecond(0)
    for span in spans
        lo = min(first(span), lo)
        hi = max(last(span), hi)
    end
    return TimeSpan(lo, hi)
end

"""
    duration(span)

Return the duration of `span` as a `Period`.

For `span::AbstractTimeSpan`, this is equivalent to `last(span) - first(span)`.

For `span::Period`, this function is the identity.
"""
duration(t::AbstractTimeSpan) = last(t) - first(t)
duration(t::Period) = t

nanoseconds_per_sample(sample_rate) = inv(sample_rate) * 1_000_000_000

"""
    index_from_time(sample_rate, sample_time)

Given `sample_rate` in Hz, return the integer index of the most recent sample
taken at `sample_time`. Note that `sample_time` must be non-negative and support
`convert(Nanosecond, sample_time)`.

Examples:

```
julia> index_from_time(1, Second(0))
1

julia> index_from_time(1, Second(1))
2

julia> index_from_time(100, Millisecond(999))
100

julia> index_from_time(100, Millisecond(1000))
101
```
"""
function index_from_time(sample_rate, sample_time)
    time_in_nanoseconds = convert(Nanosecond, sample_time).value
    time_in_nanoseconds >= 0 ||
        throw(ArgumentError("`sample_time` must be >= 0 nanoseconds"))
    ns_per_sample = nanoseconds_per_sample(sample_rate)
    return floor(Int, time_in_nanoseconds / ns_per_sample) + 1
end

"""
    index_from_time(sample_rate, span::AbstractTimeSpan)

Return the `UnitRange` of indices corresponding to `span` given `sample_rate` in Hz:

```
julia> index_from_time(100, TimeSpan(Second(0), Second(1)))
1:100

julia> index_from_time(100, TimeSpan(Second(1), Second(1)))
101:101

julia> index_from_time(100, TimeSpan(Second(3), Second(6)))
301:600
```
"""
function index_from_time(sample_rate, span::AbstractTimeSpan)
    i = index_from_time(sample_rate, first(span))
    j = index_from_time(sample_rate, last(span))
    j = i == j ? j : (j - 1)
    return i:j
end

"""
    time_from_index(sample_rate, sample_index)

Given `sample_rate` in Hz and assuming `sample_index > 0`, return the earliest
`Nanosecond` containing `sample_index`.

Examples:

```
julia> time_from_index(1, 1)
0 nanoseconds

julia> time_from_index(1, 2)
1000000000 nanoseconds

julia> time_from_index(100, 100)
990000000 nanoseconds

julia> time_from_index(100, 101)
1000000000 nanoseconds
```
"""
function time_from_index(sample_rate, index)
    index > 0 || throw(ArgumentError("`index` must be > 0"))
    return Nanosecond(ceil(Int, (index - 1) * nanoseconds_per_sample(sample_rate)))
end

"""
    time_from_index(sample_rate, sample_range::AbstractUnitRange)

Return the `TimeSpan` corresponding to `sample_range` given `sample_rate` in Hz:

```
julia> time_from_index(100, 1:100)
TimeSpan(0 nanoseconds, 1000000000 nanoseconds)

julia> time_from_index(100, 101:101)
TimeSpan(1000000000 nanoseconds, 1000000000 nanoseconds)

julia> time_from_index(100, 301:600)
TimeSpan(3000000000 nanoseconds, 6000000000 nanoseconds)
```
"""
function time_from_index(sample_rate, sample_range::AbstractUnitRange)
    i, j = first(sample_range), last(sample_range)
    j = j == i ? j : j + 1
    return TimeSpan(time_from_index(sample_rate, i), time_from_index(sample_rate, j))
end
