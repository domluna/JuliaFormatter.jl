#####
##### constructors
#####

ChainRules.@non_differentiable (::Type{T} where {T<:Array})(::UndefInitializer, args...)

function rrule(::Type{T}, x::AbstractArray) where {T<:Array}
    project_x = ProjectTo(x)
    Array_pullback(ȳ) = (NoTangent(), project_x(ȳ))
    return T(x), Array_pullback
end

#####
##### `vect`
#####

@non_differentiable Base.vect()

# Case of uniform type `T`: the data passes straight through,
# so no projection should be required.
function rrule(::typeof(Base.vect), X::Vararg{T,N}) where {T,N}
    vect_pullback(ȳ) = (NoTangent(), NTuple{N}(ȳ)...)
    return Base.vect(X...), vect_pullback
end

# Numbers and arrays are often promoted, to make a uniform vector.
# ProjectTo here reverses this
function rrule(
    ::typeof(Base.vect), X::Vararg{Union{Number,AbstractArray{<:Number}},N}
) where {N}
    projects = map(ProjectTo, X)
    function vect_pullback(ȳ)
        X̄ = ntuple(n -> projects[n](ȳ[n]), N)
        return (NoTangent(), X̄...)
    end
    return Base.vect(X...), vect_pullback
end

# Data is unmodified, so no need to project.
function rrule(::typeof(Base.vect), X::Vararg{Any,N}) where {N}
    vect_pullback(ȳ) = (NoTangent(), ntuple(n -> ȳ[n], N)...)
    return Base.vect(X...), vect_pullback
end

#####
##### `reshape`
#####

function rrule(::typeof(reshape), A::AbstractArray, dims::Tuple{Vararg{Union{Colon,Int}}})
    A_dims = size(A)
    function reshape_pullback(Ȳ)
        return (NoTangent(), reshape(Ȳ, A_dims), NoTangent())
    end
    return reshape(A, dims), reshape_pullback
end

function rrule(::typeof(reshape), A::AbstractArray, dims::Union{Colon,Int}...)
    A_dims = size(A)
    function reshape_pullback(Ȳ)
        ∂A = reshape(Ȳ, A_dims)
        ∂dims = broadcast(Returns(NoTangent()), dims)
        return (NoTangent(), ∂A, ∂dims...)
    end
    return reshape(A, dims...), reshape_pullback
end

#####
##### `repeat`
#####
function rrule(
    ::typeof(repeat),
    xs::AbstractArray;
    inner=ntuple(Returns(1), ndims(xs)),
    outer=ntuple(Returns(1), ndims(xs)),
)
    project_Xs = ProjectTo(xs)
    S = size(xs)
    function repeat_pullback(ȳ)
        dY = unthunk(ȳ)
        Δ′ = zero(xs)
        # Loop through each element of Δ, calculate source dimensions, accumulate into Δ′
        for (dest_idx, val) in pairs(IndexCartesian(), dY)
            # First, round dest_idx[dim] to nearest gridpoint defined by inner[dim], then
            # wrap around based on original size S.
            src_idx = [
                mod1(div(dest_idx[dim] - 1, inner[dim]) + 1, S[dim]) for dim in 1:length(S)
            ]
            Δ′[src_idx...] += val
        end
        x̄ = project_Xs(Δ′)
        return (NoTangent(), x̄)
    end

    return repeat(xs; inner=inner, outer=outer), repeat_pullback
end

function rrule(::typeof(repeat), xs::AbstractArray, counts::Integer...)
    project_Xs = ProjectTo(xs)
    S = size(xs)
    function repeat_pullback(ȳ)
        dY = unthunk(ȳ)
        size2ndims = ntuple(
            d -> isodd(d) ? get(S, 1 + d ÷ 2, 1) : get(counts, d ÷ 2, 1), 2 * ndims(dY)
        )
        reduced = sum(reshape(dY, size2ndims); dims=ntuple(d -> 2d, ndims(dY)))
        x̄ = project_Xs(reshape(reduced, S))
        return (NoTangent(), x̄, map(Returns(NoTangent()), counts)...)
    end
    return repeat(xs, counts...), repeat_pullback
end

#####
##### `hcat`
#####

function rrule(::typeof(hcat), Xs::Union{AbstractArray,Number}...)
    Y = hcat(Xs...)  # note that Y always has 1-based indexing, even if X isa OffsetArray
    ndimsY = Val(ndims(Y))  # this avoids closing over Y, Val() is essential for type-stability
    sizes = map(size, Xs)   # this avoids closing over Xs
    project_Xs = map(ProjectTo, Xs)
    function hcat_pullback(ȳ)
        dY = unthunk(ȳ)
        hi = Ref(0)  # Ref avoids hi::Core.Box
        dXs = map(project_Xs, sizes) do project, sizeX
            ndimsX = length(sizeX)
            lo = hi[] + 1
            hi[] += get(sizeX, 2, 1)
            ind = ntuple(ndimsY) do d
                if d == 2
                    d > ndimsX ? lo : lo:hi[]
                else
                    d > ndimsX ? 1 : (:)
                end
            end
            dX = if ndimsX > 0
                # Here InplaceableThunk breaks @inferred, removed for now
                # InplaceableThunk(dX -> dX .+= view(dY, ind...), @thunk(dY[ind...]))
                dY[ind...]
            else
                # This is a hack to perhaps avoid GPU scalar indexing
                sum(view(dY, ind...))
            end
            return project(dX)
        end
        return (NoTangent(), dXs...)
    end
    return Y, hcat_pullback
end

function rrule(::typeof(reduce), ::typeof(hcat), As::AbstractVector{<:AbstractVecOrMat})
    widths = map(A -> size(A, 2), As)
    function reduce_hcat_pullback_2(dY)
        hi = Ref(0)
        dAs = map(widths) do w
            lo = hi[] + 1
            hi[] += w
            dY[:, lo:hi[]]
        end
        return (NoTangent(), NoTangent(), dAs)
    end
    return reduce(hcat, As), reduce_hcat_pullback_2
end

function rrule(::typeof(reduce), ::typeof(hcat), As::AbstractVector{<:AbstractVector})
    axe = axes(As, 1)
    function reduce_hcat_pullback_1(dY)
        hi = Ref(0)
        dAs = map(_ -> dY[:, hi[] += 1], axe)
        return (NoTangent(), NoTangent(), dAs)
    end
    return reduce(hcat, As), reduce_hcat_pullback_1
end

#####
##### `vcat`
#####

function rrule(::typeof(vcat), Xs::Union{AbstractArray,Number}...)
    Y = vcat(Xs...)
    ndimsY = Val(ndims(Y))
    sizes = map(size, Xs)
    project_Xs = map(ProjectTo, Xs)
    function vcat_pullback(ȳ)
        dY = unthunk(ȳ)
        hi = Ref(0)
        dXs = map(project_Xs, sizes) do project, sizeX
            ndimsX = length(sizeX)
            lo = hi[] + 1
            hi[] += get(sizeX, 1, 1)
            ind = ntuple(ndimsY) do d
                if d == 1
                    d > ndimsX ? lo : lo:hi[]
                else
                    d > ndimsX ? 1 : (:)
                end
            end
            dX = if ndimsX > 0
                # InplaceableThunk(@thunk(dY[ind...]), dX -> dX .+= view(dY, ind...))
                dY[ind...]
            else
                sum(view(dY, ind...))
            end
            return project(dX)
        end
        return (NoTangent(), dXs...)
    end
    return Y, vcat_pullback
end

function rrule(::typeof(reduce), ::typeof(vcat), As::AbstractVector{<:AbstractVecOrMat})
    Y = reduce(vcat, As)
    ndimsY = Val(ndims(Y))
    heights = map(A -> size(A, 1), As)
    function reduce_vcat_pullback(dY)
        hi = Ref(0)
        dAs = map(heights) do z
            lo = hi[] + 1
            hi[] += z
            ind = ntuple(d -> d == 1 ? (lo:hi[]) : (:), ndimsY)
            dY[ind...]
        end
        return (NoTangent(), NoTangent(), dAs)
    end
    return Y, reduce_vcat_pullback
end

#####
##### `cat`
#####

_val(::Val{x}) where {x} = x

function rrule(::typeof(cat), Xs::Union{AbstractArray,Number}...; dims)
    Y = cat(Xs...; dims=dims)
    cdims = if dims isa Val
        Int(_val(dims))
    elseif dims isa Integer
        Int(dims)
    else
        Tuple(dims)
    end
    ndimsY = Val(ndims(Y))
    sizes = map(size, Xs)
    project_Xs = map(ProjectTo, Xs)
    function cat_pullback(ȳ)
        dY = unthunk(ȳ)
        prev = fill(0, _val(ndimsY))  # note that Y always has 1-based indexing, even if X isa OffsetArray
        dXs = map(project_Xs, sizes) do project, sizeX
            ndimsX = length(sizeX)
            index = ntuple(ndimsY) do d
                if d in cdims
                    if d > ndimsX
                        (prev[d] + 1)
                    else
                        ((prev[d] + 1):(prev[d] + sizeX[d]))
                    end
                else
                    d > ndimsX ? 1 : (:)
                end
            end
            for d in cdims
                prev[d] += get(sizeX, d, 1)
            end
            dX = if ndimsX > 0
                # InplaceableThunk(@thunk(dY[index...]), dX -> dX .+= view(dY, index...))
                dY[index...]
            else
                sum(view(dY, index...))
            end
            return project(dX)
        end
        return (NoTangent(), dXs...)
    end
    return Y, cat_pullback
end

#####
##### `hvcat`
#####

function rrule(::typeof(hvcat), rows, values::Union{AbstractArray,Number}...)
    Y = hvcat(rows, values...)
    cols = size(Y, 2)
    ndimsY = Val(ndims(Y))
    sizes = map(size, values)
    project_Vs = map(ProjectTo, values)
    function hvcat_pullback(dY)
        prev = fill(0, 2)
        dXs = map(project_Vs, sizes) do project, sizeX
            ndimsX = length(sizeX)
            index = ntuple(ndimsY) do d
                if d in (1, 2)
                    if d > ndimsX
                        (prev[d] + 1)
                    else
                        ((prev[d] + 1):(prev[d] + sizeX[d]))
                    end
                else
                    d > ndimsX ? 1 : (:)
                end
            end
            prev[2] += get(sizeX, 2, 1)
            if prev[2] == cols
                prev[2] = 0
                prev[1] += get(sizeX, 1, 1)
            end
            project(dY[index...])
        end
        return (NoTangent(), NoTangent(), dXs...)
    end
    return Y, hvcat_pullback
end

#####
##### `reverse`
#####

# 1-dim case allows start/stop, N-dim case takes dims keyword
# whose defaults changed in Julia 1.6... just pass them all through:

function frule((_, xdot), ::typeof(reverse), x::AbstractArray, args...; kw...)
    return reverse(x, args...; kw...), reverse(xdot, args...; kw...)
end

function rrule(::typeof(reverse), x::AbstractArray, args...; kw...)
    nots = map(Returns(NoTangent()), args)
    function reverse_pullback(dy)
        dx = @thunk reverse(unthunk(dy), args...; kw...)
        return (NoTangent(), dx, nots...)
    end
    return reverse(x, args...; kw...), reverse_pullback
end

#####
##### `circshift`
#####

function frule((_, xdot), ::typeof(circshift), x::AbstractArray, shifts)
    return circshift(x, shifts), circshift(xdot, shifts)
end

function rrule(::typeof(circshift), x::AbstractArray, shifts)
    function circshift_pullback(dy)
        dx = @thunk circshift(unthunk(dy), map(-, shifts))
        # Note that circshift! is useless for InplaceableThunk, as it overwrites completely
        return (NoTangent(), dx, NoTangent())
    end
    return circshift(x, shifts), circshift_pullback
end

#####
##### `fill`
#####

function frule((_, xdot), ::typeof(fill), x::Any, dims...)
    return fill(x, dims...), fill(xdot, dims...)
end

function rrule(::typeof(fill), x::Any, dims...)
    project = x isa Union{Number,AbstractArray{<:Number}} ? ProjectTo(x) : identity
    nots = map(Returns(NoTangent()), dims)
    fill_pullback(Ȳ) = (NoTangent(), project(sum(Ȳ)), nots...)
    return fill(x, dims...), fill_pullback
end
