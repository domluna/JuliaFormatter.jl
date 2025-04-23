function not_implemented_for(x)
    return error("Not implemented for $(x). You might want to put:  `using Makie` into your code!")
end

Attributes(x::AbstractPlot) = x.attributes

default_theme(scene, T) = Attributes()

function default_theme(scene)
    return Attributes(; color=theme(scene, :color), linewidth=1, transformation=automatic, model=automatic,
                      visible=true, transparency=false, overdraw=false, ambient=Vec3f0(0.55),
                      diffuse=Vec3f0(0.4), specular=Vec3f0(0.2), shininess=32.0f0, lightposition=:eyeposition,
                      nan_color=RGBAf0(0, 0, 0, 0), ssao=false)
end

"""
    `calculated_attributes!(trait::Type{<: AbstractPlot}, plot)`
trait version of calculated_attributes
"""
calculated_attributes!(trait, plot) = nothing

"""
    `calculated_attributes!(plot::AbstractPlot)`
Fill in values that can only be calculated when we have all other attributes filled
"""
calculated_attributes!(plot::T) where {T} = calculated_attributes!(T, plot)

"""
    image(x, y, image)
    image(image)

Plots an image on range `x, y` (defaults to dimensions).

## Attributes
$(ATTRIBUTES)
"""
@recipe(Image, x, y, image) do scene
    return Attributes(; default_theme(scene)..., colormap=[:black, :white], interpolate=true, fxaa=false,
                      lowclip=nothing, highclip=nothing)
end

# could be implemented via image, but might be optimized specifically by the backend
"""
    heatmap(x, y, values)
    heatmap(values)

Plots a heatmap as an image on `x, y` (defaults to interpretation as dimensions).

## Attributes
$(ATTRIBUTES)
"""
@recipe(Heatmap, x, y, values) do scene
    return Attributes(; default_theme(scene)..., colormap=:viridis, linewidth=0.0, interpolate=false,
                      levels=1, fxaa=true, lowclip=nothing, highclip=nothing)
end

"""
    volume(volume_data)

Plots a volume. Available algorithms are:
* `:iso` => IsoValue
* `:absorption` => Absorption
* `:mip` => MaximumIntensityProjection
* `:absorptionrgba` => AbsorptionRGBA
* `:additive` => AdditiveRGBA
* `:indexedabsorption` => IndexedAbsorptionRGBA

## Attributes
$(ATTRIBUTES)
"""
@recipe(Volume, x, y, z, volume) do scene
    return Attributes(; default_theme(scene)..., algorithm=:mip, isovalue=0.5, isorange=0.05, color=nothing,
                      colormap=:viridis, colorrange=(0, 1), fxaa=true)
end

"""
    surface(x, y, z)

Plots a surface, where `(x, y)`  define a grid whose heights are the entries in `z`.
`x` and `y` may be `Vectors` which define a regular grid, **or** `Matrices` which define an irregular grid.

## Attributes
$(ATTRIBUTES)
"""
@recipe(Surface, x, y, z) do scene
    return Attributes(; default_theme(scene)..., color=nothing, colormap=:viridis, shading=true, fxaa=true,
                      lowclip=nothing, highclip=nothing)
end

"""
    lines(positions)
    lines(x, y)
    lines(x, y, z)

Creates a connected line plot for each element in `(x, y, z)`, `(x, y)` or `positions`.

!!! tip
    You can separate segments by inserting `NaN`s.

## Attributes
$(ATTRIBUTES)
"""
@recipe(Lines, positions) do scene
    return Attributes(; default_theme(scene)..., linewidth=1.0, color=:black, colormap=:viridis,
                      linestyle=nothing, fxaa=false)
end

"""
    linesegments(positions)
    linesegments(x, y)
    linesegments(x, y, z)

Plots a line for each pair of points in `(x, y, z)`, `(x, y)`, or `positions`.

## Attributes
$(ATTRIBUTES)
"""
@recipe(LineSegments, positions) do scene
    return default_theme(scene, Lines)
end

# alternatively, mesh3d? Or having only mesh instead of poly + mesh and figure out 2d/3d via dispatch
"""
    mesh(x, y, z)
    mesh(mesh_object)
    mesh(x, y, z, faces)
    mesh(xyz, faces)

Plots a 3D or 2D mesh.

## Attributes
$(ATTRIBUTES)
"""
@recipe(Mesh, mesh) do scene
    return Attributes(; default_theme(scene)..., color=:black, colormap=:viridis, interpolate=false,
                      shading=true, fxaa=true)
end

"""
    scatter(positions)
    scatter(x, y)
    scatter(x, y, z)

Plots a marker for each element in `(x, y, z)`, `(x, y)`, or `positions`.

## Attributes
$(ATTRIBUTES)
"""
@recipe(Scatter, positions) do scene
    return Attributes(; default_theme(scene)..., color=:gray65, colormap=:viridis, marker=Circle,
                      markersize=10, strokecolor=:black, strokewidth=1.0, glowcolor=RGBA(0, 0, 0, 0),
                      glowwidth=0.0, rotations=Billboard(), marker_offset=automatic, transform_marker=false, # Applies the plots transformation to marker
                      uv_offset_width=Vec4f0(0), distancefield=nothing, markerspace=Pixel, fxaa=false)
end

"""
    meshscatter(positions)
    meshscatter(x, y)
    meshscatter(x, y, z)

Plots a mesh for each element in `(x, y, z)`, `(x, y)`, or `positions` (similar to `scatter`).
`markersize` is a scaling applied to the primitive passed as `marker`.

## Attributes
$(ATTRIBUTES)
"""
@recipe(MeshScatter, positions) do scene
    return Attributes(; default_theme(scene)..., color=:black, colormap=:viridis, colorrange=automatic,
                      marker=Sphere(Point3f0(0), 1.0f0), markersize=0.1, rotations=Quaternionf0(0, 0, 0, 1),
                      # markerspace = relative,
                      shading=true, fxaa=true)
end

"""
    text(string)

Plots a text.

## Attributes
$(ATTRIBUTES)
"""
@recipe(Text, text) do scene
    return Attributes(; default_theme(scene)..., font=theme(scene, :font), strokecolor=(:black, 0.0),
                      strokewidth=0, align=(:left, :bottom), rotation=0.0, textsize=20, position=Point2f0(0),
                      justification=0.5, lineheight=1.0)
end

function color_and_colormap!(plot, intensity=plot[:color])
    if isa(intensity[], AbstractArray{<:Number})
        haskey(plot, :colormap) ||
            error("Plot $(typeof(plot)) needs to have a colormap to allow the attribute color to be an array of numbers")

        replace_automatic!(plot, :colorrange) do
            return lift(extrema_nan, intensity)
        end
        return true
    else
        delete!(plot, :colorrange)
        return false
    end
end

function calculated_attributes!(::Type{<:Mesh}, plot)
    need_cmap = color_and_colormap!(plot)
    need_cmap || delete!(plot, :colormap)
    return
end

function calculated_attributes!(::Type{<:Union{Heatmap,Image}}, plot)
    plot[:color] = plot[3]
    return color_and_colormap!(plot)
end

function calculated_attributes!(::Type{<:Surface}, plot)
    colors = plot[3]
    if haskey(plot, :color)
        color = plot[:color][]
        if isa(color, AbstractMatrix{<:Number}) && !(color === to_value(colors))
            colors = plot[:color]
        end
    end
    return color_and_colormap!(plot, colors)
end

function calculated_attributes!(::Type{<:MeshScatter}, plot)
    return color_and_colormap!(plot)
end

function calculated_attributes!(::Type{<:Scatter}, plot)
    # calculate base case
    color_and_colormap!(plot)

    replace_automatic!(plot, :marker_offset) do
        # default to middle
        return lift(x -> to_2d_scale(x .* (-0.5f0)), plot[:markersize])
    end

    replace_automatic!(plot, :markerspace) do
        lift(plot.markersize) do ms
            if ms isa Pixel || (ms isa AbstractVector && all(x -> ms isa Pixel, ms))
                return Pixel
            else
                return SceneSpace
            end
        end
    end
end

function calculated_attributes!(::Type{<:Union{Lines,LineSegments}}, plot)
    color_and_colormap!(plot)
    pos = plot[1][]
    # extend one color per linesegment to be one (the same) color per vertex
    # taken from @edljk  in PR #77
    if haskey(plot, :color) &&
       isa(plot[:color][], AbstractVector) &&
       iseven(length(pos)) &&
       (length(pos) ÷ 2) == length(plot[:color][])
        plot[:color] = lift(plot[:color]) do cols
            return map(i -> cols[(i + 1) ÷ 2], 1:(length(cols) * 2))
        end
    end
end

const atomic_function_symbols = (:text, :meshscatter, :scatter, :mesh, :linesegments, :lines, :surface,
                                 :volume, :heatmap, :image)

const atomic_functions = getfield.(Ref(AbstractPlotting), atomic_function_symbols)
const Atomic{Arg} = Union{map(x -> Combined{x,Arg}, atomic_functions)...}

function (PT::Type{<:Combined})(parent, transformation, attributes, input_args, converted)
    return PT(parent, transformation, attributes, input_args, converted, AbstractPlot[])
end

plotsym(::Type{<:AbstractPlot{F}}) where {F} = Symbol(typeof(F).name.mt.name)

"""
    used_attributes(args...) = ()

function used to indicate what keyword args one wants to get passed in `convert_arguments`.
Usage:
```example
    struct MyType end
    used_attributes(::MyType) = (:attribute,)
    function convert_arguments(x::MyType; attribute = 1)
        ...
    end
    # attribute will get passed to convert_arguments
    # without keyword_verload, this wouldn't happen
    plot(MyType, attribute = 2)
    #You can also use the convenience macro, to overload convert_arguments in one step:
    @keywords convert_arguments(x::MyType; attribute = 1)
        ...
    end
```
"""
used_attributes(PlotType, args...) = ()

"""
apply for return type
    (args...,)
"""
function apply_convert!(P, attributes::Attributes, x::Tuple)
    return (plottype(P, x...), x)
end

"""
apply for return type PlotSpec
"""
function apply_convert!(P, attributes::Attributes, x::PlotSpec{S}) where {S}
    args, kwargs = x.args, x.kwargs
    # Note that kw_args in the plot spec that are not part of the target plot type
    # will end in the "global plot" kw_args (rest)
    for (k, v) in pairs(kwargs)
        attributes[k] = v
    end
    return (plottype(P, S), args)
end

function seperate_tuple(args::Node{<:NTuple{N,Any}}) where {N}
    ntuple(N) do i
        lift(args) do x
            if i <= length(x)
                x[i]
            else
                error("You changed the number of arguments. This isn't allowed!")
            end
        end
    end
end

function (PlotType::Type{<:AbstractPlot{Typ}})(scene::SceneLike, attributes::Attributes, args) where {Typ}
    input = convert.(Node, args)
    argnodes = lift(input...) do args...
        return convert_arguments(PlotType, args...)
    end
    return PlotType(scene, attributes, input, argnodes)
end

function plot(scene::Scene, plot::AbstractPlot)
    # plot object contains local theme (default values), and user given values (from constructor)
    # fill_theme now goes through all values that are missing from the user, and looks if the scene
    # contains any theming values for them (via e.g. css rules). If nothing founds, the values will
    # be taken from local theme! This will connect any values in the scene's theme
    # with the plot values and track those connection, so that we can separate them
    # when doing delete!(scene, plot)!
    complete_theme!(scene, plot)
    # we just return the plot... whoever calls plot (our pipeline usually)
    # will need to push!(scene, plot) etc!
    return plot
end

function (PlotType::Type{<:AbstractPlot{Typ}})(scene::SceneLike, attributes::Attributes, input,
                                               args) where {Typ}
    # The argument type of the final plot object is the assumened to stay constant after
    # argument conversion. This might not always hold, but it simplifies
    # things quite a bit
    ArgTyp = typeof(to_value(args))
    # construct the fully qualified plot type, from the possible incomplete (abstract)
    # PlotType
    FinalType = Combined{Typ,ArgTyp}
    plot_attributes = merged_get!(() -> default_theme(scene, FinalType), plotsym(FinalType), scene,
                                  attributes)

    # Transformation is a field of the plot type, but can be given as an attribute
    trans = get(plot_attributes, :transformation, automatic)
    transformation = if to_value(trans) == automatic
        Transformation(scene)
    elseif isa(to_value(trans), Transformation)
        to_value(trans)
    else
        t = Transformation(scene)
        transform!(t, to_value(trans))
        t
    end
    replace_automatic!(plot_attributes, :model) do
        return transformation.model
    end
    # create the plot, with the full attributes, the input signals, and the final signal nodes.
    plot_obj = FinalType(scene, transformation, plot_attributes, input, seperate_tuple(args))
    transformation.parent[] = plot_obj
    calculated_attributes!(plot_obj)
    return plot_obj
end

"""
    `plottype(plot_args...)`

Any custom argument combination that has a preferred way to be plotted should overload this.
e.g.:
```example
    # make plot(rand(5, 5, 5)) plot as a volume
    plottype(x::Array{<: AbstractFloat, 3}) = Volume
```
"""
plottype(plot_args...) = Combined{Any,Tuple{typeof.(to_value.(plot_args))...}} # default to dispatch to type recipes!

## generic definitions
# If the Combined has no plot func, calculate them
plottype(::Type{<:Combined{Any}}, argvalues...) = plottype(argvalues...)
plottype(::Type{Any}, argvalues...) = plottype(argvalues...)
# If it has something more concrete than Any, use it directly
plottype(P::Type{<:Combined{T}}, argvalues...) where {T} = P

## specialized definitions for types
plottype(::AbstractVector, ::AbstractVector) = Scatter
plottype(::AbstractVector) = Scatter
plottype(::AbstractMatrix{<:Real}) = Heatmap
plottype(::Array{<:AbstractFloat,3}) = Volume
plottype(::AbstractString) = Text

plottype(::LineString) = Lines
plottype(::AbstractVector{<:LineString}) = Lines
plottype(::MultiLineString) = Lines

plottype(::Polygon) = Poly
plottype(::GeometryBasics.AbstractPolygon) = Poly
plottype(::AbstractVector{<:GeometryBasics.AbstractPolygon}) = Poly
plottype(::MultiPolygon) = Lines

"""
    plottype(P1::Type{<: Combined{T1}}, P2::Type{<: Combined{T2}})

Chooses the more concrete plot type
```example
function convert_arguments(P::PlotFunc, args...)
    ptype = plottype(P, Lines)
    ...
end
"""
plottype(P1::Type{<:Combined{Any}}, P2::Type{<:Combined{T}}) where {T} = P2
plottype(P1::Type{<:Combined{T}}, P2::Type{<:Combined}) where {T} = P1

"""
Returns the Combined type that represents the signature of `args`.
"""
function Plot(args::Vararg{Any,N}) where {N}
    return Combined{Any,<:Tuple{args...}}
end
Base.@pure function Plot(::Type{T}) where {T}
    return Combined{Any,<:Tuple{T}}
end
Base.@pure function Plot(::Type{T1}, ::Type{T2}) where {T1,T2}
    return Combined{Any,<:Tuple{T1,T2}}
end

# all the plotting functions that get a plot type
const PlotFunc = Union{Type{Any},Type{<:AbstractPlot}}

######################################################################
# In this section, the plotting functions have P as the first argument
# These are called from type recipes

# non-mutating, without positional attributes

function plot(P::PlotFunc, args...; kw_attributes...)
    attributes = Attributes(kw_attributes)
    return plot(P, attributes, args...)
end

# with positional attributes

function plot(P::PlotFunc, attrs::Attributes, args...; kw_attributes...)
    attributes = merge!(Attributes(kw_attributes), attrs)
    scene_attributes = extract_scene_attributes!(attributes)
    scene = Scene(; scene_attributes...)
    return plot!(scene, P, attributes, args...)
end

# mutating, without positional attributes

function plot!(P::PlotFunc, scene::SceneLike, args...; kw_attributes...)
    attributes = Attributes(kw_attributes)
    return plot!(scene, P, attributes, args...)
end

# without scenelike, use current scene

function plot!(P::PlotFunc, args...; kw_attributes...)
    return plot!(P, current_scene(), args...; kw_attributes...)
end

# with positional attributes

function plot!(P::PlotFunc, scene::SceneLike, attrs::Attributes, args...; kw_attributes...)
    attributes = merge!(Attributes(kw_attributes), attrs)
    return plot!(scene, P, attributes, args...)
end
######################################################################

# Register plot / plot! using the Any type as PlotType.
# This is done so that plot(args...) / plot!(args...) can by default go
# through a pipeline where the appropriate PlotType is determined
# from the input arguments themselves.
eval(default_plot_signatures(:plot, :plot!, :Any))

# plots to scene

plotfunc(::Combined{F}) where {F} = F

"""
Main plotting signatures that plot/plot! route to if no Plot Type is given
"""
function plot!(scene::SceneLike, P::PlotFunc, attributes::Attributes, args...; kw_attributes...)
    attributes = merge!(Attributes(kw_attributes), attributes)
    argvalues = to_value.(args)
    PreType = plottype(P, argvalues...)
    # plottype will lose the argument types, so we just extract the plot func
    # type and recreate the type with the argument type
    PreType = Combined{plotfunc(PreType),typeof(argvalues)}
    convert_keys = intersect(used_attributes(PreType, argvalues...), keys(attributes))
    kw_signal = if isempty(convert_keys) # lift(f) isn't supported so we need to catch the empty case
        Node(())
    else
        lift((args...) -> Pair.(convert_keys, args), getindex.(attributes, convert_keys)...) # make them one tuple to easier pass through
    end
    # call convert_arguments for a first time to get things started
    converted = convert_arguments(PreType, argvalues...; kw_signal[]...)
    # convert_arguments can return different things depending on the recipe type
    # apply_conversion deals with that!

    FinalType, argsconverted = apply_convert!(PreType, attributes, converted)
    converted_node = Node(argsconverted)
    input_nodes = convert.(Node, args)
    onany(kw_signal, lift(tuple, input_nodes...)) do kwargs, args
        # do the argument conversion inside a lift
        result = convert_arguments(FinalType, args...; kwargs...)
        finaltype, argsconverted = apply_convert!(FinalType, attributes, result)
        if finaltype != FinalType
            error("Plot type changed from $FinalType to $finaltype after conversion.
                Changing the plot type based on values in convert_arguments is not allowed")
        end
        return converted_node[] = argsconverted
    end
    return plot!(scene, FinalType, attributes, input_nodes, converted_node)
end

plot!(p::Combined) = _plot!(p)

_plot!(p::Atomic{T}) where {T} = p

function _plot!(p::Combined{Any,T}) where {T}
    args = (T.parameters...,)
    typed_args = join(string.("::", args), ", ")
    return error("Plotting for the arguments ($typed_args) not defined. If you want to support those arguments, overload plot!(plot::Plot$((args...,)))")
end
function _plot!(p::Combined{X,T}) where {X,T}
    args = (T.parameters...,)
    typed_args = join(string.("::", args), ", ")
    return error("Plotting for the arguments ($typed_args) not defined for $X. If you want to support those arguments, overload plot!(plot::$X{ <: $T})")
end

function show_attributes(attributes)
    for (k, v) in attributes
        println("    ", k, ": ", v[] == nothing ? "nothing" : v[])
    end
end

"""
    extract_scene_attributes!(attributes)

removes all scene attributes from `attributes` and returns them in a new
Attribute dict.
"""
function extract_scene_attributes!(attributes)
    scene_attributes = (:backgroundcolor, :resolution, :show_axis, :show_legend, :scale_plot, :center, :axis,
                        :axis2d, :axis3d, :legend, :camera, :limits, :padding, :raw, :SSAO)
    result = Attributes()
    for k in scene_attributes
        haskey(attributes, k) && (result[k] = pop!(attributes, k))
    end
    return result
end

function plot!(scene::SceneLike, P::PlotFunc, attributes::Attributes, input::NTuple{N,Node},
               args::Node) where {N}
    # create "empty" plot type - empty meaning containing no plots, just attributes + arguments
    scene_attributes = extract_scene_attributes!(attributes)
    plot_object = P(scene, copy(attributes), input, args)
    # transfer the merged attributes from theme and user defined to the scene
    for (k, v) in scene_attributes
        scene.attributes[k] = v
    end
    # We allow certain scene attributes to be part of the plot theme
    for k in (:camera, :raw)
        if haskey(plot_object, k)
            scene.attributes[k] = plot_object[k]
        end
    end

    # call user defined recipe overload to fill the plot type
    plot!(plot_object)

    push!(scene, plot_object)

    if !scene.raw[] || scene[:camera][] !== automatic
        # if no camera controls yet, setup camera
        setup_camera!(scene)
    end
    if !scene.raw[]
        add_axis!(scene, scene.attributes)
    end
    # ! ∘ isaxis --> (x)-> !isaxis(x)
    # move axis to front, so that scene[end] gives back the last plot and not the axis!
    if !isempty(scene.plots) && isaxis(last(scene.plots))
        axis = pop!(scene.plots)
        pushfirst!(scene.plots, axis)
    end
    return scene
end

function plot!(scene::Combined, P::PlotFunc, attributes::Attributes, args...)
    # create "empty" plot type - empty meaning containing no plots, just attributes + arguments
    plot_object = P(scene, attributes, args)
    # call user defined recipe overload to fill the plot type
    plot!(plot_object)
    push!(scene.plots, plot_object)
    return scene
end
function plot!(scene::Combined, P::PlotFunc, attributes::Attributes, input::NTuple{N,Node},
               args::Node) where {N}
    # create "empty" plot type - empty meaning containing no plots, just attributes + arguments
    plot_object = P(scene, attributes, input, args)
    # call user defined recipe overload to fill the plot type
    plot!(plot_object)
    push!(scene.plots, plot_object)
    return scene
end

function apply_camera!(scene::Scene, cam_func)
    if cam_func in (cam2d!, cam3d!, campixel!, cam3d_cad!)
        cam_func(scene)
    else
        error("Unrecognized `camera` attribute type: $(typeof(cam_func)). Use automatic, cam2d! or cam3d!, campixel!, cam3d_cad!")
    end
end

function setup_camera!(scene::Scene)
    theme_cam = scene[:camera][]
    if theme_cam == automatic
        cam = cameracontrols(scene)
        # only automatically add camera when cameracontrols are empty (not set)
        if cam == EmptyCamera()
            if is2d(scene)
                cam2d!(scene)
            else
                cam3d!(scene)
            end
        end
    else
        apply_camera!(scene, theme_cam)
    end
    return scene
end

function find_in_plots(scene::Scene, key::Symbol)
    # TODO findfirst is a bit flaky... maybe merge multiple ranges + tick labels?!
    idx = findfirst(scene.plots) do plot
        return !isaxis(plot) && haskey(plot, key) && plot[key][] !== automatic
    end
    if idx !== nothing
        scene.plots[idx][key]
    else
        automatic
    end
end

function add_axis!(scene::Scene, attributes=Attributes())
    show_axis = scene.show_axis[]
    show_axis isa Bool || error("show_axis needs to be a bool")
    axistype = if scene.axis_type[] == automatic
        is2d(scene) ? axis2d! : axis3d!
    elseif scene.axis_type[] in (axis2d!, axis3d!)
        scene.axis_type[]
    else
        error("Unrecogniced `axis_type` attribute type: $(typeof(scene[:axis_type][])). Use automatic, axis2d! or axis3d!")
    end

    if show_axis && scene[Axis] === nothing
        axis_attributes = Attributes()
        for key in (:axis, :axis2d, :axis3d)
            if haskey(scene, key) && !isempty(scene[key])
                axis_attributes = scene[key]
                break
            end
        end
        ranges = get(attributes, :tickranges) do
            return find_in_plots(scene, :tickranges)
        end
        labels = get(attributes, :ticklabels) do
            return find_in_plots(scene, :ticklabels)
        end
        lims = lift(scene.limits, scene.data_limits) do sl, dl
            sl === automatic && return dl
            return sl
        end
        axistype(scene, axis_attributes, lims; ticks=(ranges=ranges, labels=labels))
    end
    return scene
end

function add_labels!(scene::Scene)
    if plot_attributes.show_legend[] && haskey(p.attributes, :colormap)
        legend_attributes = plot_attributes[:legend][]
        colorlegend(scene, p.attributes[:colormap], p.attributes[:colorrange], legend_attributes)
    end
    return scene
end
