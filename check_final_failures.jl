using Pkg
Pkg.activate(".")
using JuliaFormatter

println("=== Checking final 4 test failures ===\n")

# Test 1 & 2: Lines 286-287 - Dict with comment
println("Test at lines 286-287:")
str = raw"""
Dict{Int, Int}(
               # Comment
               1 => 2,
               3 => 4)
"""
result = format_text(str, SciMLStyle())
println("Input:\n", str)
println("Output:\n", result)
println("Match expected: ", result == raw"""
Dict{Int, Int}(
    # Comment
    1 => 2,
    3 => 4)
""")
println()

# Test 3: Line 667 - vector with margin 13
println("Test at line 667 - margin 13:")
str = "test = [arg1, arg2, arg3, arg4, arg5]"
result = format_text(str, SciMLStyle(); margin = 13)
println("Output:\n", result)
expected = """
test = [
        arg1,
        arg2,
        arg3,
        arg4,
        arg5]
"""
println("Expected:\n", expected)
println("Match: ", result == expected)
println()

# Test 4: Line 1513 - Lambda parameters
println("Test at line 1513:")
str = raw"""
new_rate = (u, p, t) -> rate(u.u, p, t) - min(rate(u.u, p, t), rate_control(u.u_control, p, t))
"""
result = format_text(str, SciMLStyle())
println("Output:\n", result)
println("Contains '(u, p, t)': ", contains(result, "(u, p, t)"))