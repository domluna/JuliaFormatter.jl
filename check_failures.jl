using Test, JuliaFormatter

# Run only SciML style tests
@testset "SciML Style Failures" begin
    # Test vector with margin 30
    str = "[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]"
    expected = """[a, b, c, d, e,
    f, g, h, i, j, k,
    l, m, n, o, p,
    q, r, s, t, u,
    v, w, x, y, z]"""
    result = format_text(str, SciMLStyle(), margin=30)
    if result != expected
        println("Vector margin 30 FAILED:")
        println("Expected:\n", expected)
        println("Got:\n", result)
    else
        println("Vector margin 30 PASSED")
    end
    
    # Test from actual test file
    str2 = "test = [arg1, arg2, arg3, arg4, arg5]"
    expected2 = """test = [arg1, arg2, arg3,
            arg4, arg5]"""
    result2 = format_text(str2, SciMLStyle(), margin=26)
    if result2 != expected2
        println("\nVector margin 26 FAILED:")
        println("Expected:\n", expected2)
        println("Got:\n", result2)
    else
        println("Vector margin 26 PASSED")
    end
    
    # Lambda test
    str3 = """lambda(x) -> veryverylongfunctionnameanother(argument1,
    argument2, argument3, argument4)"""
    expected3 = """lambda(x) -> veryverylongfunctionnameanother(argument1,
        argument2, argument3, argument4)"""
    result3 = format_text(str3, SciMLStyle(), margin=60)
    if result3 != expected3
        println("\nLambda FAILED:")
        println("Expected:\n", expected3)
        println("Got:\n", result3)
    else
        println("Lambda PASSED")
    end
    
    # Issue #753
    str4 = """Dict("A" => "a", "B" => "bbbbbbbbbbbbbbbbbbbbbbbbbbbb", "C" => "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")"""
    expected4 = """Dict("A" => "a", "B" => "bbbbbbbbbbbbbbbbbbbbbbbbbbbb",
        "C" => "ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")"""
    result4 = format_text(str4, SciMLStyle(), margin=92)
    if result4 != expected4
        println("\nIssue 753 FAILED:")
        println("Expected:\n", expected4)
        println("Got:\n", result4)
    else
        println("Issue 753 PASSED")
    end
    
    # Issue #817
    str5 = """Dict{A, B}(data["filename"] => data["num_lines"] for data = datasets)"""
    expected5 = """Dict{A, B}(data["filename"] => data["num_lines"]
        for data = datasets)"""
    result5 = format_text(str5, SciMLStyle(), margin=60)
    if result5 != expected5
        println("\nIssue 817 FAILED:")
        println("Expected:\n", expected5)
        println("Got:\n", result5)
    else
        println("Issue 817 PASSED")
    end
    
    # JumpProcesses Issue
    str6 = """ConstantRateJump(rate1,
                      affect1!)"""
    expected6 = """ConstantRateJump(rate1,
    affect1!)"""
    result6 = format_text(str6, SciMLStyle())
    if result6 != expected6
        println("\nJumpProcesses FAILED:")
        println("Expected:\n", expected6)
        println("Got:\n", result6)
    else
        println("JumpProcesses PASSED")
    end
end