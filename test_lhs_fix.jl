using JuliaFormatter

# Test the long case
str = "p_a, p_b = @inbounds particle_neighbor_pressure(v_particle_system, v_neighbor_system, particle_system, neighbor_system, particle, neighbor)"

println("Testing LHS tuple fix...")
println("Original ($(length(str)) chars):")
println(str)

formatted = format_text(str, SciMLStyle())
println("\nFormatted:")
println(formatted)

if contains(formatted, "p_a,\np_b")
    println("\n❌ LHS broken")
else
    println("\n✓ LHS preserved")
end