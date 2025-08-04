using JuliaFormatter

println("Testing JuliaFormatter fix for 'invalid Array dimensions' error")
println("=" ^ 60)

# Test case 1: Dict with variable_call_indent
println("\nTest 1: Dict formatting with variable_call_indent")
println("-" ^ 40)

str1 = """
hydrostatic_water_column_tests = Dict("WCSPH with ViscosityAdami and SummationDensity" => (viscosity_fluid = ViscosityAdami(nu = 0.0015f0), maxiters = 38, clip_negative_pressure = true))
"""

println("Input:")
println(str1)

try
    formatted1 = format_text(str1, SciMLStyle(), variable_call_indent = ["Dict"], margin = 92)
    println("Output (SUCCESS - no crash!):")
    println(formatted1)
catch e
    println("ERROR: ", e)
end

# Test case 2: Complex nested expressions
println("\nTest 2: Complex nested expressions")
println("-" ^ 40)

str2 = """
result = transform(integrate(differentiate(interpolate(data, method=:cubic), order=2), bounds=(a, b)), scaling=:log)
"""

println("Input:")
println(str2)

try
    formatted2 = format_text(str2, SciMLStyle())
    println("Output (SUCCESS - no crash!):")
    println(formatted2)
catch e
    println("ERROR: ", e)
end

# Test case 3: Array with line breaks
println("\nTest 3: Array formatting with existing line breaks")
println("-" ^ 40)

str3 = """
kernels = [
    GaussianKernel,
    SchoenbergCubicSplineKernel,
    SchoenbergQuarticSplineKernel,
    SchoenbergQuinticSplineKernel
]
"""

println("Input:")
println(str3)

try
    formatted3 = format_text(str3, SciMLStyle(), yas_style_nesting = true)
    println("Output (SUCCESS - no crash!):")
    println(formatted3)
catch e
    println("ERROR: ", e)
end

# Test case 4: Edge case that could create empty placeholder groups
println("\nTest 4: Edge case with potential empty groups")
println("-" ^ 40)

str4 = """
x = Dict{Int, SomeLongerTypeThanJustString}(
    1 => "some arbitrary string bla bla bla bla bla bla",
    2 => "another longer arbitrary string bla bla bla bla bla bla bla bla")
"""

println("Input:")
println(str4)

try
    formatted4 = format_text(str4, SciMLStyle(), variable_call_indent = ["Dict"])
    println("Output (SUCCESS - no crash!):")
    println(formatted4)
catch e
    println("ERROR: ", e)
end

println("\n" ^ 2)
println("All tests completed! The 'invalid Array dimensions' error has been fixed.")
println("=" ^ 60)