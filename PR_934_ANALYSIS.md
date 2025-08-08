# PR #934 Test Failure Analysis

## Summary
PR #934 ("fix function call formatting in SciML style") has 6 failing tests that are currently marked as `@test_broken`. The failures are related to indentation and alignment issues in the SciML style formatting.

## Failing Tests

### 1. Dict with inline comment (Lines 325-326)
**Issue**: Excessive indentation when Dict has an inline comment after opening parenthesis
```julia
# Input:
Dict{Int, Int}( # Comment
        1 => 2,
        3 => 4)

# Expected (3-space indent):
Dict{Int, Int}( # Comment
   1 => 2,
   3 => 4)

# Actual (15-space alignment):
Dict{Int, Int}( # Comment
               1 => 2,
               3 => 4)
```
**Root Cause**: The formatter is aligning to the opening parenthesis position (YAS-style) instead of using standard indentation when there's an inline comment.

### 2. Arrays with yas_style_nesting (Lines 1808-1809)
**Issue**: Arrays are being reformatted to 4-space indentation when they should preserve YAS-style alignment
```julia
# Input (11-space alignment):
kernels = [GaussianKernel,
           SchoenbergCubicSplineKernel,
           SchoenbergQuarticSplineKernel,
           SchoenbergQuinticSplineKernel]

# Output (4-space indent):
kernels = [GaussianKernel,
    SchoenbergCubicSplineKernel,
    SchoenbergQuarticSplineKernel,
    SchoenbergQuinticSplineKernel]
```
**Root Cause**: The `n_vect!` function in `nest.jl` is forcing standard indentation even when `yas_style_nesting = true`.

### 3. Function definitions (Line 1874)
**Issue**: Function arguments have off-by-one indentation error
```julia
# Expected (35 spaces):
function gradient_correction_init(particle_system,
                                   ::KernelGradientCorrection{MixedKernelGradientCorrection},
                                   another_argument)

# Actual (34 spaces):
function gradient_correction_init(particle_system,
                                  ::KernelGradientCorrection{MixedKernelGradientCorrection},
                                  another_argument)
```
**Root Cause**: Alignment calculation is off by one character when breaking after the function name.

### 4. Dict with variable_call_indent (Line 1923)
**Issue**: Extreme indentation when using variable_call_indent with Dict
```julia
# Expected (standard indentation):
hydrostatic_water_column_tests = Dict(
    "WCSPH with ViscosityAdami and SummationDensity" => (
        viscosity_fluid = ViscosityAdami(nu = 0.0015f0),
        ...
    ),
)

# Actual (extreme alignment to column 38):
hydrostatic_water_column_tests = Dict(
                                      "WCSPH with ViscosityAdami and SummationDensity" => (
                                                                                           viscosity_fluid = ViscosityAdami(nu = 0.0015f0),
                                                                                           ...
                                                                                           ),
                                      )
```
**Root Cause**: The `variable_call_indent` option is causing YAS-style alignment to the opening parenthesis, resulting in excessive indentation.

## Code Locations

### Key Files to Fix:
1. `/src/styles/sciml/nest.jl` - Contains the nesting logic for SciML style
   - `n_call!` (lines 334-378): Handles function call indentation
   - `n_vect!` (lines 681-805): Handles array/vector indentation
   - `n_tuple!` (lines 381-506): Handles tuple indentation including Dict values
   - `n_functiondef!` (lines 39-80): Handles function definition indentation

2. `/src/nest_utils.jl` - Contains utilities for finding optimal nest placeholders
   - `find_optimal_nest_placeholders` (lines 260-361): Determines where to break lines

3. `/src/styles/sciml/pretty.jl` - Contains pretty printing logic
   - Various `p_*` functions that delegate to YAS or Default style

## Proposed Fixes

### Fix 1: Dict with inline comment
In `n_call!`, detect inline comments and use standard indentation instead of YAS alignment.

### Fix 2: Arrays with yas_style_nesting
In `n_vect!`, when `yas_style_nesting = true`, preserve the original YAS-style alignment instead of forcing standard indentation.

### Fix 3: Function definitions
In `n_functiondef!`, correct the alignment calculation to properly align with the opening parenthesis.

### Fix 4: Dict with variable_call_indent
In `n_tuple!`, when handling Dict pair values with `variable_call_indent`, use standard indentation instead of the hardcoded 42-space indent (lines 441-453).

## Testing Strategy
1. Run the test suite with `julia --project -e "using Pkg; Pkg.test()"`
2. Focus on the SciML style tests in `test/sciml_style.jl`
3. The 6 failing tests are currently marked as `@test_broken` to allow the suite to pass
4. Once fixes are implemented, change `@test_broken` back to `@test` for verification