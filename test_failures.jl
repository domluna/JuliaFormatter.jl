using JuliaFormatter
using Test

# Test 1: Vector with margin 30
println("\n=== Test 1: Vector with margin 30 ===")
str = "[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]"
expected = """[a, b, c, d, e,
    f, g, h, i, j, k,
    l, m, n, o, p,
    q, r, s, t, u,
    v, w, x, y, z]"""
result = format_text(str, SciMLStyle(), margin=30)
println("Expected:\n$expected")
println("Got:\n$result")
println("Match: ", result == expected)

# Test 2: Lambda expression
println("\n=== Test 2: Lambda expression ===")
str = """lambda(x) -> veryverylongfunctionnameanother(argument1,
argument2, argument3, argument4)"""
expected = """lambda(x) -> veryverylongfunctionnameanother(argument1,
    argument2, argument3, argument4)"""
result = format_text(str, SciMLStyle(), margin=60)
println("Expected:\n$expected")
println("Got:\n$result")
println("Match: ", result == expected)

# Test 3: Issue #753
println("\n=== Test 3: Issue #753 ===")
str = """Dict("A" => "a", "B" => "bbbbbbbbbbbbbbbbbbbbbbbbbbbb", "C" => "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")"""
expected = """Dict("A" => "a", "B" => "bbbbbbbbbbbbbbbbbbbbbbbbbbbb",
    "C" => "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")"""
result = format_text(str, SciMLStyle(), margin=92)
println("Expected:\n$expected")
println("Got:\n$result")
println("Match: ", result == expected)

# Test 4: Issue #817
println("\n=== Test 4: Issue #817 ===")
str = """Dict{A, B}(data["filename"] => data["num_lines"] for data = datasets)"""
expected = """Dict{A, B}(data["filename"] => data["num_lines"]
    for data = datasets)"""
result = format_text(str, SciMLStyle(), margin=60)
println("Expected:\n$expected")
println("Got:\n$result")
println("Match: ", result == expected)