test1 = "discrete_modified, saved_in_cb = DiffEqBase.apply_discrete_callback!(integrator, integrator.opts.callback.discrete_callbacks...)"
test2 = "new_rate = (u, p, t) -> rate(u.u, p, t) - min(rate(u.u, p, t), rate_control(u.u_control, p, t))"
test3 = "@inline function executerx!(speciesvec::AbstractVector{T}, rxidx::S, majump::M) where {T, S, M <: AbstractMassActionJump}"

println("Test 1 length: ", length(test1), " (margin = 92)")
println("Test 2 length: ", length(test2), " (margin = 92)")
println("Test 3 length: ", length(test3), " (margin = 92)")