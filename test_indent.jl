using JuliaFormatter

# Test from the actual test file
str = "test = [arg1, arg2, arg3, arg4, arg5]"

# Expected from test
expected = """test = [arg1, arg2, arg3,
        arg4, arg5]"""

result = format_text(str, SciMLStyle(); margin = 26)

println("Input: ", repr(str))
println("\nExpected:")
for (i, line) in enumerate(split(expected, "\n"))
    spaces = length(match(r"^(\s*)", line).captures[1])
    println("  Line $i ($spaces spaces): ", repr(line))
end

println("\nActual result:")
for (i, line) in enumerate(split(result, "\n"))
    spaces = length(match(r"^(\s*)", line).captures[1])
    println("  Line $i ($spaces spaces): ", repr(line))
end

println("\nMatch: ", result == expected)