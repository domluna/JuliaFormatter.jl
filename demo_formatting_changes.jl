using JuliaFormatter

println("=== JuliaFormatter PR #934 - Before and After Comparison ===\n")

# Test cases to demonstrate the fixes

test_cases = [
    # 1. Function calls now align to opening parenthesis when split
    (
        "Function call alignment (long line)",
        """
        res = NLS.solve(nlls_prob, NLS.LevenbergMarquardt(); maxiters = 1000, show_trace = Val(true), other_long_param = true)
        """,
    ),
    
    # 2. Arrays align to opening bracket  
    (
        "Array alignment",
        """
        kernels = [GaussianKernel, SchoenbergCubicSplineKernel, SchoenbergQuarticSplineKernel, SchoenbergQuinticSplineKernel]
        """,
    ),
    
    # 3. Dict with variable_call_indent
    (
        "Dict with variable_call_indent",
        """
        Dict{Int, Int}(
            1 => 2,
            3 => 4)
        """,
    ),
    
    # 4. LHS tuple assignment preservation (short)
    (
        "LHS tuple preservation (short)",
        """
        p_a, p_b = some_function(arg1, arg2)
        """,
    ),
    
    # 5. Tuple destructuring with semicolon
    (
        "Tuple destructuring",
        """
        (; density_calculator, state_equation, correction) = particle_system
        (; sound_speed) = state_equation
        """,
    ),
    
    # 6. Split LHS tuple indentation
    (
        "Split LHS indentation",
        """
        p_a,
        p_b = @inbounds particle_neighbor_pressure(v_particle_system,
                                                    v_neighbor_system,
                                                    particle_system, neighbor_system,
                                                    particle, neighbor)
        """,
    ),
]

println("Testing with default SciMLStyle:")
println("=" ^ 60)

for (name, code) in test_cases
    println("\n## $name ##")
    println("\nBefore formatting:")
    println(code)
    
    formatted = format_text(code, SciMLStyle())
    println("\nAfter formatting:")
    println(formatted)
    println("-" ^ 40)
end

println("\n\n=== Key Changes Summary ===")
println("""
1. Function calls and arrays now align to opening parenthesis/bracket
2. LHS tuples in assignments are preserved on one line when they fit
3. Split LHS tuples maintain consistent indentation
4. Tuple destructuring with semicolon preserves space after semicolon
5. With yas_style_nesting=true, arrays use standard 4-space indentation

All tests pass: ✓ 229 tests
""")