using Test
using JuliaFormatter

# Run the issue tests as part of the test suite
@testset "Issue #934 - Examples from comment #3152210407" begin
    # Example 1: Array with newline after opening bracket
    # With yas_style_nesting=true, this should move the first element to the bracket line
    # and use 4-space indentation for the rest
    str1 = """
    kernels = [
        GaussianKernel,
        SchoenbergCubicSplineKernel,
        SchoenbergQuarticSplineKernel,
        SchoenbergQuinticSplineKernel
    ]
    """
    expected1 = """
    kernels = [GaussianKernel,
        SchoenbergCubicSplineKernel,
        SchoenbergQuarticSplineKernel,
        SchoenbergQuinticSplineKernel]
    """
    @test format_text(str1, SciMLStyle(), yas_style_nesting=true) == expected1

    # Example 2: Array without newline after opening bracket
    str2 = """
    kernels = [GaussianKernel,
        SchoenbergCubicSplineKernel,
        SchoenbergQuarticSplineKernel,
        SchoenbergQuinticSplineKernel
    ]
    """
    expected2 = """
    kernels = [GaussianKernel,
               SchoenbergCubicSplineKernel,
               SchoenbergQuarticSplineKernel,
               SchoenbergQuinticSplineKernel]
    """
    @test format_text(str2, SciMLStyle(), yas_style_nesting=true) == expected2

    # Example 3: Tuple spacing
    str3 = """
    x = (0.0, 1.0)
    """
    expected3 = """
    x = (0.0, 1.0)
    """
    @test format_text(str3, SciMLStyle(), yas_style_nesting=true) == expected3

    # Example 4: Multi-line tuple destructuring
    str4 = """
    (viscosity_correction, pressure_correction,
     surface_tension_correction) = free_surface_correction(correction, rho_mean)
    """
    # This should not throw an error and produce valid syntax
    result4 = format_text(str4, SciMLStyle(), yas_style_nesting=true)
    @test !isnothing(result4)
    # Verify it's parseable
    @test_nowarn Meta.parse(result4)

    # Example 5: Named tuple destructuring
    str5 = """
    (; face_vertices, face_vertices_ids, edge_normals,
     face_edges_ids, face_normals, vertex_normals) = boundary
    """
    # This should not throw an error and produce valid syntax
    result5 = format_text(str5, SciMLStyle(), yas_style_nesting=true)
    @test !isnothing(result5)
    # Verify it's parseable
    @test_nowarn Meta.parse(result5)
end