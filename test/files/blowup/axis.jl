function ticks_and_labels end

module Formatters
using Showoff

function scientific(ticks::AbstractVector)
    return Showoff.showoff(ticks, :scientific)
end

function plain(ticks::AbstractVector)
    try
        Showoff.showoff(ticks, :plain)
    catch e
        Base.showerror(stderr, e)
        println("with ticks: ", ticks)
        String["-Inf", "Inf"]
    end
end

end
using .Formatters

"""
    $(SIGNATURES)

Plots a 2-dimensional axis.

## Attributes
$(ATTRIBUTES)
"""
@recipe(Axis2D) do scene
    return Attributes(; visible=true, showgrid=true, showticks=true, showtickmarks=true, padding=0.0,
                      ticks=Attributes(; ranges_labels=(automatic, automatic), formatter=Formatters.plain,
                                       gap=1, title_gap=3, linewidth=(1, 1),
                                       linecolor=((:black, 0.4), (:black, 0.4)), linestyle=(nothing, nothing),
                                       textcolor=(:black, :black), textsize=(5, 5), rotation=(0.0, 0.0),
                                       align=((:center, :top), (:right, :center)),
                                       font=lift(dim2, theme(scene, :font))),
                      tickmarks=Attributes(; length=(1.0, 1.0), linewidth=(1, 1),
                                           linecolor=((:black, 0.4), (:black, 0.2)),
                                           linestyle=(nothing, nothing)),
                      grid=Attributes(; linewidth=(0.5, 0.5), linecolor=((:black, 0.3), (:black, 0.3)),
                                      linestyle=(nothing, nothing)),
                      frame=Attributes(; linewidth=1.0, linecolor=:black, linestyle=nothing,
                                       axis_position=nothing, axis_arrow=false, arrow_size=2.5,
                                       frames=((false, false), (false, false))),
                      names=Attributes(; axisnames=("x", "y"), title=nothing, textcolor=(:black, :black),
                                       textsize=(6, 6), rotation=(0.0, -1.5pi),
                                       align=((:center, :top), (:center, :bottom)),
                                       font=lift(dim2, theme(scene, :font))))
end

"""
    $(SIGNATURES)

Plots a 3-dimensional Axis.

## Attributes
$(ATTRIBUTES)
"""
@recipe(Axis3D) do scene
    q1 = qrotation(Vec3f0(1, 0, 0), -0.5f0 * pi)
    q2 = qrotation(Vec3f0(0, 0, 1), 1.0f0 * pi)
    tickrotations3d = (qrotation(Vec3f0(0, 0, 1), -1.5pi), q2, qrotation(Vec3f0(1, 0, 0), -0.5pi) * q2)
    axisnames_rotation3d = tickrotations3d
    tickalign3d = ((:left, :center), # x axis
                   (:right, :center), # y axis
                   (:right, :center))
    axisnames_align3d = tickalign3d
    tick_color = RGBAf0(0.5, 0.5, 0.5, 0.6)
    grid_color = RGBAf0(0.5, 0.5, 0.5, 0.4)
    grid_thickness = 1
    gridthickness = ntuple(x -> 1.0f0, Val(3))
    tsize = 5 # in percent
    return Attributes(; visible=true, showticks=(true, true, true), showaxis=(true, true, true),
                      showgrid=(true, true, true), scale=Vec3f0(1), padding=0.1,
                      names=Attributes(; axisnames=("x", "y", "z"), textcolor=(:black, :black, :black),
                                       rotation=axisnames_rotation3d, textsize=(6.0, 6.0, 6.0),
                                       align=axisnames_align3d, font=lift(dim3, theme(scene, :font)), gap=3),
                      ticks=Attributes(; ranges_labels=(automatic, automatic), formatter=Formatters.plain,
                                       textcolor=(tick_color, tick_color, tick_color),
                                       rotation=tickrotations3d, textsize=(tsize, tsize, tsize),
                                       align=tickalign3d, gap=3, font=lift(dim3, theme(scene, :font))),
                      frame=Attributes(; linecolor=(grid_color, grid_color, grid_color),
                                       linewidth=(grid_thickness, grid_thickness, grid_thickness),
                                       axiscolor=(:black, :black, :black)))
end

isaxis(x) = false
isaxis(x::Union{Axis2D,Axis3D}) = true

const Limits{N} = NTuple{N,Tuple{Number,Number}}

function default_ticks(limits::Limits, ticks, scale_func::Function)
    return default_ticks.(limits, (ticks,), scale_func)
end

# function default_ticks(limits::Limits, ticks::Tuple, scale_func::Function)
#     default_ticks.(limits, ticks, scale_func)
# end
function default_ticks(limits::Tuple{Number,Number}, ticks, scale_func::Function)
    return default_ticks(limits..., ticks, scale_func)
end

function default_ticks(lmin::Number, lmax::Number, ticks::AbstractVector{<:Number}, scale_func::Function)
    return scale_func.((filter(t -> lmin <= t <= lmax, ticks)))
end
function default_ticks(lmin::Number, lmax::Number, ::Automatic, scale_func::Function)
    # scale the limits
    scaled_ticks, mini, maxi = optimize_ticks(scale_func(lmin), scale_func(lmax); k_min=4, # minimum number of ticks # minimum number of ticks
                                              k_max=8)
    length(scaled_ticks) == 1 && isnan(scaled_ticks[1]) && return [-Inf, Inf]
    return scaled_ticks
end

function default_ticks(lmin::Number, lmax::Number, ticks::Integer, scale_func=identity)
    scaled_ticks, mini, maxi = optimize_ticks(scale_func(lmin), scale_func(lmax); k_min=ticks, # minimum number of ticks # minimum number of ticks
                                              k_max=ticks, # maximum number of ticks # maximum number of ticks
                                              k_ideal=ticks,
                                              # `strict_span = false` rewards cases where the span of the
                                              # chosen  ticks is not too much bigger than amin - amax:
                                              strict_span=false)
    return scaled_ticks
end

function default_ticks(x::Automatic, limits::Tuple, n)
    return default_ticks(limits, n, identity)
end

function default_ticks(ticks::Tuple, limits::Tuple, n::Tuple)
    return default_ticks.(ticks, (limits,), n)
end

default_ticks(ticks::Tuple, limits::Limits, n) = default_ticks.(ticks, limits, (n,))

default_ticks(ticks::Tuple, limits::Limits, n::Tuple) = default_ticks.(ticks, limits, n)

default_ticks(ticks::AbstractVector{<:Number}, limits, n) = ticks

function default_labels(x::NTuple{N,Any}, formatter::Function) where {N}
    return default_labels.(x, formatter)
end

function default_labels(x::AbstractVector, y::AbstractVector, formatter::Function=Formatters.plain)
    return default_labels.((x, y), formatter)
end

function default_labels(ticks::AbstractVector, formatter::Function=Formatters.plain)
    if applicable(formatter, ticks)
        return formatter(ticks) # takes the whole array
    elseif applicable(formatter, first(ticks))
        return formatter.(ticks)
    else
        error("Formatting function $(formatter) is neither applicable to $(typeof(ticks)) nor $(eltype(ticks)).")
    end
end

default_labels(x::Automatic, ranges, formatter) = default_labels(ranges, formatter)
default_labels(x::Tuple, ranges::Tuple, formatter) = default_labels.(x, ranges, (formatter,))
default_labels(x::Tuple, ranges, formatter) = default_labels.(x, (ranges,), (formatter,))
default_labels(x::AbstractVector{<:AbstractString}, ranges, formatter::Function) = x
default_labels(x::AbstractVector{<:AbstractString}, ranges::AbstractVector, formatter::Function) = x

function convert_arguments(::Type{<:Axis2D}, limits::Rect)
    e = extrema(limits)
    return (((e[1][1], e[2][1]), (e[1][2], e[2][2])),)
end

function convert_arguments(::Type{<:Axis3D}, limits::Rect)
    e = (minimum(limits), maximum(limits))
    return (((e[1][1], e[2][1]), (e[1][2], e[2][2]), (e[1][3], e[2][3])),)
end

a_length(x::AbstractVector) = length(x)
a_length(x::Automatic) = x

function calculated_attributes!(::Type{<:Union{Axis2D,Axis3D}}, plot)
    ticks = plot.ticks
    args = (plot.padding, plot[1], ticks.ranges, ticks.labels, ticks.formatter)
    ticks[:ranges_labels] = lift(args...) do pad, lims, ranges, labels, formatter
        limit_widths = map(x -> x[2] - x[1], lims)
        pad = (limit_widths .* pad)
        # pad the drawn limits and use them as the ranges
        lim_pad = map((lim, p) -> (lim[1] - p, lim[2] + p), lims, pad)
        num_ticks = labels === automatic ? automatic : a_length.(labels)
        ranges = default_ticks(ranges, lim_pad, num_ticks)
        labels = default_labels(labels, ranges, formatter)
        return (ranges, labels)
    end
    return
end

function draw_ticks(textbuffer, dim, origin, ticks, linewidth, linecolor, linestyle, textcolor, textsize,
                    rotation, align, font)
    for (tick, str) in ticks
        pos = ntuple(i -> i != dim ? origin[i] : tick, Val(2))
        push!(textbuffer, str, Point(pos); rotation=rotation[dim], textsize=textsize[dim], align=align[dim],
              color=textcolor[dim], font=font[dim])
    end
end

function draw_tickmarks(linebuffer, dim, origin, ticks, dir::NTuple{N}, linewidth, linecolor, linestyle,
                        textcolor, textsize, rotation, align, font) where {N}
    for (tick, str) in ticks
        pos = ntuple(i -> i != dim ? origin[i] : tick, Val(2))
        posf0 = Point2f0(pos)
        dirf0 = Pointf0{N}(dir)
        append!(linebuffer, [posf0, posf0 .+ dirf0]; color=linecolor[dim], linewidth=linewidth[dim])
    end
end

function draw_grid(linebuffer, dim, origin, ticks, dir::NTuple{N}, linewidth, linecolor, linestyle) where {N}
    dirf0 = Pointf0{N}(dir)
    for (tick, str) in ticks
        tup = ntuple(i -> i != dim ? origin[i] : tick, Val(N))
        posf0 = Pointf0{N}(tup)
        append!(linebuffer, [posf0, posf0 .+ dirf0]; color=linecolor[dim], linewidth=linewidth[dim])
    end
end

function draw_frame(linebuffer, limits::NTuple{N,Any}, linewidth, linecolor, linestyle, axis_position,
                    axis_arrow, arrow_size, frames) where {N}
    mini = minimum.(limits)
    maxi = maximum.(limits)
    rect = Rect(Vec(mini), Vec(maxi .- mini))
    origin = Vec{N}(0.0)
    if (origin in rect) && axis_position == :origin
        for i in 1:N
            start = unit(Point{N,Float32}, i) * Float32(mini[i])
            to = unit(Point{N,Float32}, i) * Float32(maxi[i])
            if false #axis_arrow
                arrows(scene, [start, to]; linewidth=linewidth, linecolor=linecolor, linestyle=linestyle,
                       arrowsize=arrow_size)
            else
                append!(linebuffer, [start, to]; linewidth=linewidth, color=linecolor)
            end
        end
    end
    limit_widths = maxi .- mini
    frames = convert_attribute(frames, key"frames"())
    for side in 1:2
        from = Point{N,Float32}(getindex.(limits, side))
        # if axis is drawn at origin, and we draw frame from origin,
        # we already did this
        if !(from == origin && axis_position == :origin)
            for otherside in 1:2
                for dim in 1:N
                    if frames[N - dim + 1][3 - otherside]
                        p = ntuple(i -> i == dim ? limits[i][otherside] : limits[i][side], Val(N))
                        to = Point{N,Float32}(p)
                        append!(linebuffer, [from, to]; linewidth=linewidth, color=linecolor)
                    end
                end
            end
        end
    end
end

function draw_titles(textbuffer, xticks, yticks, origin, limit_widths, tickfont, tick_size, tick_gap,
                     tick_title_gap, axis_labels, textcolor, textsize, rotation, align, font, title)
    tickspace_x = maximum(map(yticks) do tick
                              str = last(tick)
                              tick_bb = text_bb(str, to_font(tickfont[2]), tick_size[2])
                              return widths(tick_bb)[1]
                          end)

    tickspace_y = widths(text_bb(last(first(xticks)), to_font(tickfont[1]), tick_size[1]))[2]

    model_inv = inv(transformationmatrix(textbuffer)[])

    tickspace = transform(model_inv, (tickspace_x, tickspace_y))
    title_start = origin .- (tick_gap .+ tickspace .+ tick_title_gap)
    half_width = origin .+ (limit_widths ./ 2.0)

    posx = (half_width[1], title_start[2])
    posy = (title_start[1], half_width[2])
    positions = (posx, posy)
    for i in 1:2
        if !isempty(axis_labels[i])
            push!(textbuffer, axis_labels[i], positions[i]; textsize=textsize[i], align=align[i],
                  rotation=rotation[i], color=textcolor[i], font=font[i])
        end
    end
    if title !== nothing
        # TODO give title own text attributes
        push!(textbuffer, title, (half_width[1], (origin .+ (limit_widths))[2]); textsize=textsize[1],
              align=(:center, :top), rotation=rotation[1], color=textcolor[1], font=font[1])
    end
end

function ticks_and_labels(x)
    st, s = extrema(x)
    r = LinRange(st, s, 4)
    return zip(r, string.(round.(r, 4)))
end

function transform(model::Mat4, x::T) where {T}
    x4d = to_ndim(Vec4f0, x, 0.0)
    return to_ndim(T, model * x4d, 0.0)
end
un_transform(model::Mat4, x) = transform(inv(model), x)

to2tuple(x) = ntuple(i -> x, Val(2))
to2tuple(x::Tuple{<:Any,<:Any}) = x

function draw_axis2d(textbuffer, frame_linebuffer, grid_linebuffer, tickmarks_linebuffer, m, padding, limits,
                     xyrange_labels, showgrid, showticks, showtickmarks,
                     # grid attributes
                     g_linewidth, g_linecolor, g_linestyle,

                     # tick attributes
                     t_linewidth, t_linecolor, t_linestyle, t_textcolor, t_textsize, t_rotation, t_align,
                     t_font, t_gap, t_title_gap,

                     #tickmark attribures
                     tm_linewidth, tm_linecolor, tm_linestyle, tm_length,

                     # frame attributes
                     f_linewidth, f_linecolor, f_linestyle, f_axis_position, f_axis_arrow, f_arrow_size,
                     f_frames,

                     # title / axis name attributes
                     ti_labels, ti_textcolor, ti_textsize, ti_rotation, ti_align, ti_font, ti_title)
    xyrange, labels = xyrange_labels
    start!(textbuffer)
    start!(frame_linebuffer)
    foreach(start!, grid_linebuffer)
    foreach(start!, tickmarks_linebuffer)
    # limits = limits Vec2f0(padding)
    # limits ((xmin, xmax), (ymin, ymax))
    limit_widths = map(x -> x[2] - x[1], limits)
    pad = (limit_widths .* to2tuple(padding))
    # pad the drawn limits
    limits = map((lim, p) -> (lim[1] - p, lim[2] + p), limits, pad)

    # recalculate widths
    limit_widths = map(x -> x[2] - x[1], limits)

    % = mean(limit_widths) / 100 # percentage
    xyticks = zip.(xyrange, labels)
    model_inv = inv(transformationmatrix(textbuffer)[])

    ti_textsize = ti_textsize .* %
    t_textsize = t_textsize .* %
    t_gap = transform(model_inv, to2tuple(t_gap .* %))
    tm_length = transform(model_inv, to2tuple(tm_length .* %))
    t_title_gap = transform(model_inv, to2tuple(t_title_gap .* %))

    origin = first.(limits)
    dirs = ((0.0, Float64(limit_widths[2])), (Float64(limit_widths[1]), 0.0))
    foreach(1:2, dirs, xyticks) do dim, dir, ticks
        if showgrid[dim]
            draw_grid(grid_linebuffer[dim], dim, origin, ticks, dir, g_linewidth, g_linecolor, g_linestyle)
        end
    end
    tm_offsets = ((0.0, Float64(tm_length[2])), (Float64(tm_length[1]), Float64(0.0)))
    foreach(1:2, tm_offsets, xyticks) do dim, offset, ticks
        if showtickmarks[dim]
            draw_tickmarks(tickmarks_linebuffer[dim], dim, origin .- offset, ticks, offset, t_linewidth,
                           t_linecolor, t_linestyle, t_textcolor, t_textsize, t_rotation, t_align, t_font)
        end
    end
    t_offsets = ((0.0, Float64(t_gap[2] + tm_length[2])), (Float64(t_gap[1] + tm_length[1]), Float64(0.0)))
    foreach(1:2, t_offsets, xyticks) do dim, offset, ticks
        if showticks[dim]
            draw_ticks(textbuffer, dim, origin .- offset, ticks, t_linewidth, t_linecolor, t_linestyle,
                       t_textcolor, t_textsize, t_rotation, t_align, t_font)
        end
    end

    draw_frame(frame_linebuffer, limits, f_linewidth, f_linecolor, f_linestyle, f_axis_position, f_axis_arrow,
               f_arrow_size, f_frames)

    draw_titles(textbuffer, xyticks..., origin, limit_widths, t_font, t_textsize, t_gap, t_title_gap,
                ti_labels, ti_textcolor, ti_textsize, ti_rotation, ti_align, ti_font, ti_title)
    finish!(textbuffer)
    finish!(frame_linebuffer)
    foreach(finish!, grid_linebuffer)
    foreach(finish!, tickmarks_linebuffer)
    return
end

# for axis, we don't want to have plot!(scene, args) called on it, so we need to overload it directly
function plot!(scene::SceneLike, ::Type{<:Axis2D}, attributes::Attributes, args...)
    # create "empty" plot type - empty meaning containing no plots, just attributes + arguments
    cplot = Axis2D(scene, attributes, args)
    # Disable any non linear transform for the axis plot!
    cplot.transformation.transform_func[] = identity
    g_keys = (:linewidth, :linecolor, :linestyle)
    f_keys = (:linewidth, :linecolor, :linestyle, :axis_position, :axis_arrow, :arrow_size, :frames)
    t_keys = (:linewidth, :linecolor, :linestyle, :textcolor, :textsize, :rotation, :align, :font, :gap,
              :title_gap)
    tm_keys = (:linewidth, :linecolor, :linestyle, :length)
    ti_keys = (:axisnames, :textcolor, :textsize, :rotation, :align, :font, :title)

    g_args = getindex.(cplot.grid, g_keys)
    f_args = getindex.(cplot.frame, f_keys)
    t_args = getindex.(cplot.ticks, t_keys)
    tm_args = getindex.(cplot.tickmarks, tm_keys)
    ti_args = getindex.(cplot.names, ti_keys)

    textbuffer = TextBuffer(cplot, Point{2})

    grid_linebuffer = Node((LinesegmentBuffer(cplot, Point{2}; transparency=true,
                                              linestyle=lift(first, cplot[:grid, :linestyle])),
                            LinesegmentBuffer(cplot, Point{2}; transparency=true,
                                              linestyle=lift(last, cplot[:grid, :linestyle]))))

    frame_linebuffer = Node(LinesegmentBuffer(cplot, Point{2}; transparency=true,
                                              linestyle=cplot.frame.linestyle))
    tickmarks_linebuffer = Node((LinesegmentBuffer(cplot, Point{2}; transparency=true,
                                                   linestyle=lift(first, cplot[:tickmarks, :linestyle])),
                                 LinesegmentBuffer(cplot, Point{2}; transparency=true,
                                                   linestyle=lift(last, cplot[:tickmarks, :linestyle]))))
    map_once(draw_axis2d, Node(textbuffer), frame_linebuffer, grid_linebuffer, tickmarks_linebuffer,
             transformationmatrix(scene), cplot.padding, cplot[1], cplot.ticks.ranges_labels,
             lift.((dim2,), (cplot.showgrid, cplot.showticks, cplot.showtickmarks))..., g_args..., t_args...,
             tm_args..., f_args..., ti_args...)
    push!(scene, cplot)
    return cplot
end

function labelposition(ranges, dim, dir, tgap, origin::StaticVector{N}) where {N}
    a, b = extrema(ranges[dim])
    whalf = Float32(((b - a) / 2))
    halfaxis = unit(Point{N,Float32}, dim) .* whalf

    return origin .+ (halfaxis .+ (normalize(dir) * tgap))
end

_widths(x::Tuple{<:Number,<:Number}) = x[2] - x[1]
_widths(x) = Float32(maximum(x) - minimum(x))

to3tuple(x::Tuple{Any}) = (x[1], x[1], x[1])
to3tuple(x::Tuple{Any,Any}) = (x[1], x[2], x[2])
to3tuple(x::Tuple{Any,Any,Any}) = x
to3tuple(x) = ntuple(i -> x, Val(3))

function draw_axis3d(textbuffer, linebuffer, limits, ranges_labels, args...)
    # make sure we extend all args to 3D
    ranges, ticklabels = ranges_labels
    args3d = to3tuple.(args)
    (showaxis, showticks, showgrid, axisnames, axisnames_color, axisnames_size, axisrotation, axisalign, axisnames_font, titlegap, gridcolors, gridthickness, axiscolors, ttextcolor, trotation, ttextsize, talign, tfont, tgap) = args3d # splat to names

    N = 3
    start!(textbuffer)
    start!(linebuffer)
    mini, maxi = first.(limits), last.(limits)

    origin = Point{N,Float32}(min.(mini, first.(ranges)))
    limit_widths = max.(last.(ranges), maxi) .- origin
    % = minimum(limit_widths) / 100 # percentage
    ttextsize = (%) .* ttextsize
    axisnames_size = (%) .* axisnames_size

    titlegap = (%) .* titlegap
    tgap = (%) .* tgap
    for i in 1:N
        axis_vec = unit(Point{N,Float32}, i)
        width = Float32(limit_widths[i])
        stop = origin .+ (width .* axis_vec)
        if showaxis[i]
            append!(linebuffer, [origin, stop]; color=axiscolors[i], linewidth=1.5f0)
        end
        if showticks[i]
            range = ranges[i]
            j = mod1(i + 1, N)
            tickdir = unit(Point{N,Float32}, j)
            tickdir, offset2 = if i != 2
                tickdir = unit(Vec{N,Float32}, j)
                tickdir, Float32(limit_widths[j] + tgap[i]) * tickdir
            else
                tickdir = unit(Vec{N,Float32}, 1)
                tickdir, Float32(limit_widths[1] + tgap[i]) * tickdir
            end
            for (j, tick) in enumerate(range)
                labels = ticklabels[i]
                if length(labels) >= j
                    str = labels[j]
                    if !isempty(str)
                        startpos = (origin .+ ((Float32(tick - origin[i]) * axis_vec)) .+ offset2)
                        push!(textbuffer, str, startpos; color=ttextcolor[i], rotation=trotation[i],
                              textsize=ttextsize[i], align=talign[i], font=tfont[i])
                    end
                end
            end
            if !isempty(axisnames[i])
                tick_widths = if length(ticklabels[i]) >= 3
                    widths(text_bb(ticklabels[i][end - 1], to_font(tfont[i]), ttextsize[i]))[1]
                else
                    0.0f0
                end
                pos = (labelposition(ranges, i, tickdir, titlegap[i] + tick_widths, origin) .+ offset2)
                push!(textbuffer, to_latex(axisnames[i]), pos; textsize=axisnames_size[i],
                      color=axisnames_color[i], rotation=axisrotation[i], align=axisalign[i],
                      font=axisnames_font[i])
            end
        end
        if showgrid[i]
            c = gridcolors[i]
            thickness = gridthickness[i]
            for _j in (i + 1):(i + N - 1)
                j = mod1(_j, N)
                dir = unit(Point{N,Float32}, j)
                range = ranges[j]
                for tick in range
                    offset = Float32(tick - origin[j]) * dir
                    append!(linebuffer, [origin .+ offset, stop .+ offset]; color=c, linewidth=thickness)
                end
            end
        end
        finish!(textbuffer)
        finish!(linebuffer)
    end
    return
end

function plot!(scene::SceneLike, ::Type{<:Axis3D}, attributes::Attributes, args...)
    axis = Axis3D(scene, attributes, args)
    # Disable any non linear transform for the axis plot!
    axis.transformation.transform_func[] = identity
    textbuffer = TextBuffer(axis, Point{3}; transparency=true)
    linebuffer = LinesegmentBuffer(axis, Point{3}; transparency=true)

    tstyle, ticks, frame = to_value.(getindex.(axis, (:names, :ticks, :frame)))
    titlevals = getindex.(tstyle, (:axisnames, :textcolor, :textsize, :rotation, :align, :font, :gap))
    framevals = getindex.(frame, (:linecolor, :linewidth, :axiscolor))
    tvals = getindex.(ticks, (:textcolor, :rotation, :textsize, :align, :font, :gap))
    args = (getindex.(axis, (:showaxis, :showticks, :showgrid))..., titlevals..., framevals..., tvals...)
    map_once(draw_axis3d, Node(textbuffer), Node(linebuffer), axis[1], axis.ticks.ranges_labels, args...)
    push!(scene, axis)
    return axis
end
