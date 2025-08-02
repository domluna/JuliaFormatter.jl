using Pkg
Pkg.activate(".")
using JuliaFormatter

println("=== Checking failing tests and their actual outputs ===\n")

# Test line 228 - yas_style_nesting
println("Test at line 228:")
str = raw"""
Dict{Int, Int}(
    1 => 2,
    3 => 4,
)
"""
result = format_text(str, SciMLStyle(); yas_style_nesting = true)
println("Input:\n", str)
println("Output:\n", result)
println()

# Test lines 254-255 - Long Dict
println("Test at lines 254-255:")
str = raw"""
SomeLongerTypeThanJustString = String
y = Dict{Int, SomeLongerTypeThanJustString}(1 => "some arbitrary string bla bla bla bla bla bla",
    2 => "another longer arbitrary string bla bla bla bla bla bla bla bla")
"""
result = format_text(str, SciMLStyle())
println("Output:\n", result)
println()

# Test lines 272-273 - Dict with comment
println("Test at lines 272-273:")
str = raw"""
Dict{Int, Int}(
               # Comment
               1 => 2,
               3 => 4)
"""
result = format_text(str, SciMLStyle())
println("Output:\n", result)
println()

# Test function definitions with margins
println("Function definition tests:")
str = """
function foo(arg1, arg2, arg3, arg4, arg5)
    body
end
"""

for (line, margin) in [(532, 29), (533, 25), (543, 24), (555, 18)]
    result = format_text(str, SciMLStyle(); margin = margin)
    println("Line $line, margin $margin:")
    println(result)
end

# Test vector definitions with margins
println("\nVector definition tests:")
str = """
test = [arg1, arg2, arg3, arg4, arg5]
"""

for (line, margin) in [(571, 36), (572, 33), (578, 32), (580, 26), 
                       (587, 25), (588, 21), (595, 24), (605, 19), 
                       (607, 18), (616, 16)]
    result = format_text(str, SciMLStyle(); margin = margin)
    println("Line $line, margin $margin:")
    println(result)
end