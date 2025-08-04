# JuliaFormatter Fix Demonstration - PR #934

This document demonstrates the fix for the "invalid Array dimensions" error reported in PR #934.

## The Issue

When formatting certain code structures, JuliaFormatter would crash with:
```
ArgumentError: invalid Array dimensions
```

This occurred when `find_optimal_nest_placeholders` received empty placeholder groups, resulting in attempts to create arrays with negative dimensions.

## The Fix

Added safety checks to prevent empty groups from being processed:
1. Skip empty groups when collecting placeholder groups  
2. Add explicit check for n=0 in `find_optimal_nest_placeholders`
3. Filter out empty groups before processing

## Examples That Previously Crashed

### Example 1: Dict with variable_call_indent

**Code that caused crash:**
```julia
# With variable_call_indent = ["Dict"] and yas_style_nesting = true
hydrostatic_water_column_tests = Dict("WCSPH with ViscosityAdami and SummationDensity" => (viscosity_fluid = ViscosityAdami(nu = 0.0015f0), maxiters = 38, clip_negative_pressure = true))
```

**Now formats correctly to:**
```julia
hydrostatic_water_column_tests = Dict(
    "WCSPH with ViscosityAdami and SummationDensity" => (viscosity_fluid=ViscosityAdami(;
                                                                                         nu=0.0015f0),
                                                          maxiters=38,
                                                          clip_negative_pressure=true),
)
```

### Example 2: Complex nested expressions

**Code that could trigger the error:**
```julia
# Complex expressions with specific line break patterns
result = transform(integrate(differentiate(interpolate(data, method=:cubic), order=2), bounds=(a, b)), scaling=:log)
```

**Now formats correctly to:**
```julia
result = transform(integrate(differentiate(interpolate(data, method=:cubic), order=2),
                             bounds=(a, b)),
                   scaling=:log)
```

### Example 3: Array operations with specific structures

**Code that could cause issues:**
```julia
# Certain array structures that create empty placeholder groups
kernels = [
    GaussianKernel,
    SchoenbergCubicSplineKernel,
    SchoenbergQuarticSplineKernel,
    SchoenbergQuinticSplineKernel
]
```

**Now formats correctly to:**
```julia
kernels = [GaussianKernel,
           SchoenbergCubicSplineKernel,
           SchoenbergQuarticSplineKernel,
           SchoenbergQuinticSplineKernel]
```

## Technical Details

The error occurred in `src/nest_utils.jl` at line:
```julia
dp = fill(0, n - 1, n - 1)
```

When `n = 0` (from an empty placeholder group), this would try to create an array with dimensions (-1, -1), causing the error.

## Test Results

All tests now pass. The fix ensures:
- No crashes on edge cases
- Formatting behavior remains consistent with intended style
- Empty placeholder groups are handled gracefully

## Related Issues

- PR #934: Fix function call formatting
- Issue raised by @efaulhaber in PR comments