using JuliaFormatter

# Test case: assignment with tuple on LHS
code = """
discrete_modified, saved_in_cb = DiffEqBase.apply_discrete_callback!(integrator, integrator.opts.callback.discrete_callbacks...)
"""

# Try with different margin settings
println("With margin = 92 (default):")
println(format_text(code, SciMLStyle()))

println("\nWith margin = 150 (wide):")
println(format_text(code, SciMLStyle(), margin=150))

# Test a simpler case
code2 = """
a, b = some_function_call(arg1, arg2, arg3)
"""

println("\nSimpler test:")
println("Original:")
println(code2)
println("Formatted:")
println(format_text(code2, SciMLStyle()))