
if VERSION ≥ v"1.4"
    function frule((_, ẋ, ṗ), ::typeof(evalpoly), x, p)
        Δx, Δp = ẋ, unthunk(ṗ)
        N = length(p)
        @inbounds y = p[N]
        Δy = Δp[N]
        @inbounds for i in (N - 1):-1:1
            Δy = muladd(Δx, y, muladd(x, Δy, Δp[i]))
            y = muladd(x, y, p[i])
        end
        return y, Δy
    end

    function rrule(::typeof(evalpoly), x, p)
        y, ys = _evalpoly_intermediates(x, p)
        project_x = ProjectTo(x)
        project_p = p isa Tuple ? identity : ProjectTo(p)
        function evalpoly_pullback(Δy)
            ∂x, ∂p = _evalpoly_back(x, p, ys, Δy)
            return NoTangent(), project_x(∂x), project_p(∂p)
        end
        return y, evalpoly_pullback
    end

    function rrule(::typeof(evalpoly), x, p::Vector{<:Matrix}) # does not type infer with ProjectTo
        y, ys = _evalpoly_intermediates(x, p)
        function evalpoly_pullback(Δy)
            ∂x, ∂p = _evalpoly_back(x, p, ys, Δy)
            return NoTangent(), ∂x, ∂p
        end
        return y, evalpoly_pullback
    end

    # evalpoly but storing intermediates
    function _evalpoly_intermediates(x, p::Tuple)
        return if @generated
            N = length(p.parameters)
            exs = []
            vars = []
            ex = :(p[$N])
            for i in 1:(N - 1)
                yi = Symbol("y", i)
                push!(vars, yi)
                push!(exs, :($yi = $ex))
                ex = :(muladd(x, $yi, p[$(N - i)]))
            end
            push!(exs, :(y = $ex))
            Expr(:block, exs..., :(y, ($(vars...),)))
        else
            _evalpoly_intermediates_fallback(x, p)
        end
    end
    function _evalpoly_intermediates_fallback(x, p::Tuple)
        N = length(p)
        y = p[N]
        ys = (y, ntuple(N - 2) do i
            return y = muladd(x, y, p[N - i])
        end...)
        y = muladd(x, y, p[1])
        return y, ys
    end
    function _evalpoly_intermediates(x, p)
        N = length(p)
        @inbounds yn = one(x) * p[N]
        ys = similar(p, typeof(yn), N - 1)
        @inbounds ys[1] = yn
        @inbounds for i in 2:(N - 1)
            ys[i] = muladd(x, ys[i - 1], p[N - i + 1])
        end
        @inbounds y = muladd(x, ys[N - 1], p[1])
        return y, ys
    end

    # TODO: Handle following cases
    #     1) x is a UniformScaling, pᵢ is a matrix
    #     2) x is a matrix, pᵢ is a UniformScaling
    @inline _evalpoly_backx(x, yi, ∂yi) = ∂yi * yi'
    @inline _evalpoly_backx(x, yi, ∂x, ∂yi) = muladd(∂yi, yi', ∂x)
    @inline _evalpoly_backx(x::Number, yi, ∂yi) = conj(dot(∂yi, yi))
    @inline _evalpoly_backx(x::Number, yi, ∂x, ∂yi) = _evalpoly_backx(x, yi, ∂yi) + ∂x

    @inline _evalpoly_backp(pi, ∂yi) = ∂yi

    function _evalpoly_back(x, p::Tuple, ys, Δy)
        return if @generated
            exs = []
            vars = []
            N = length(p.parameters)
            for i in 2:(N - 1)
                ∂pi = Symbol("∂p", i)
                push!(vars, ∂pi)
                push!(exs, :(∂x = _evalpoly_backx(x, ys[$(N - i)], ∂x, ∂yi)))
                push!(exs, :($∂pi = _evalpoly_backp(p[$i], ∂yi)))
                push!(exs, :(∂yi = x′ * ∂yi))
            end
            push!(vars, :(_evalpoly_backp(p[$N], ∂yi))) # ∂pN
            Expr(
                :block,
                :(x′ = x'),
                :(∂yi = Δy),
                :(∂p1 = _evalpoly_backp(p[1], ∂yi)),
                :(∂x = _evalpoly_backx(x, ys[$(N - 1)], ∂yi)),
                :(∂yi = x′ * ∂yi),
                exs...,
                :(∂p = (∂p1, $(vars...))),
                :(∂x, Tangent{typeof(p),typeof(∂p)}(∂p)),
            )
        else
            _evalpoly_back_fallback(x, p, ys, Δy)
        end
    end
    function _evalpoly_back_fallback(x, p::Tuple, ys, Δy)
        x′ = x'
        ∂yi = unthunk(Δy)
        N = length(p)
        ∂p1 = _evalpoly_backp(p[1], ∂yi)
        ∂x = _evalpoly_backx(x, ys[N - 1], ∂yi)
        ∂yi = x′ * ∂yi
        ∂p = (
            ∂p1,
            ntuple(N - 2) do i
                ∂x = _evalpoly_backx(x, ys[N - i - 1], ∂x, ∂yi)
                ∂pi = _evalpoly_backp(p[i + 1], ∂yi)
                ∂yi = x′ * ∂yi
                return ∂pi
            end...,
            _evalpoly_backp(p[N], ∂yi), # ∂pN
        )
        return ∂x, Tangent{typeof(p),typeof(∂p)}(∂p)
    end
    function _evalpoly_back(x, p, ys, Δy)
        x′ = x'
        ∂yi = one(x′) * Δy
        N = length(p)
        @inbounds ∂p1 = _evalpoly_backp(p[1], ∂yi)
        ∂p = similar(p, typeof(∂p1))
        @inbounds begin
            ∂x = _evalpoly_backx(x, ys[N - 1], ∂yi)
            ∂yi = x′ * ∂yi
            ∂p[1] = ∂p1
            for i in 2:(N - 1)
                ∂x = _evalpoly_backx(x, ys[N - i], ∂x, ∂yi)
                ∂p[i] = _evalpoly_backp(p[i], ∂yi)
                ∂yi = x′ * ∂yi
            end
            ∂p[N] = _evalpoly_backp(p[N], ∂yi)
        end
        return ∂x, ∂p
    end
end
