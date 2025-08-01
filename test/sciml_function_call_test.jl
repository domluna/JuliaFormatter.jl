using JuliaFormatter
using Test

# Test cases based on the problematic formatting from JumpProcesses.jl PR #504

test_code1 = """
discrete_modified, saved_in_cb = DiffEqBase.apply_discrete_callback!(integrator, integrator.opts.callback.discrete_callbacks...)
"""

test_code2 = """
new_rate = (u, p, t) -> rate(u.u, p, t) - min(rate(u.u, p, t), rate_control(u.u_control, p, t))
"""

test_code3 = """
@inline function executerx!(speciesvec::AbstractVector{T}, rxidx::S, majump::M) where {T, S, M <: AbstractMassActionJump}
    # function body
end
"""

# Format with SciMLStyle
formatted1 = format_text(test_code1, SciMLStyle())
formatted2 = format_text(test_code2, SciMLStyle())
formatted3 = format_text(test_code3, SciMLStyle())

println("Test 1 - Function call with splatted arguments:")
println("Original:")
println(test_code1)
println("Formatted:")
println(formatted1)
println()

println("Test 2 - Lambda with long expression:")
println("Original:")
println(test_code2)
println("Formatted:")
println(formatted2)
println()

println("Test 3 - Long function signature:")
println("Original:")
println(test_code3)
println("Formatted:")
println(formatted3)

# The issue is that these should not be split into multiple lines unnecessarily
# when they fit within the margin (92 characters for SciMLStyle)