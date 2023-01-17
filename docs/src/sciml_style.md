# SciML Style

```@docs
SciMLStyle
```

## Configuration File Example

The `.JuliaFormatter.toml` which represents these settings is

```toml
style = "sciml"
```

Or to use `SciMLStyle` except change one of the settings:

```toml
style = "sciml"
remove_extra_newlines = false
```

## Direct Usage

```julia
format("file.jl", SciMLStyle())
```

Or to use `SciMLStyle` except change one of the settings:

```julia
format("file.jl", SciMLStyle(), remove_extra_newlines=false)
```

## Additional Options

The `SciMLStyle` supports the additional option `variable_dict_indent`,
which is set to `false` by default.
It allows `Dict` definitions without aligning to the opening parenthesis:

```julia
# Allowed with and without `variable_dict_indent`
Dict{Int, Int}(1 => 2,
               3 => 4)

# Allowed when `variable_dict_indent = true`, but
# will be changed to the first example when `variable_dict_indent = false`.
Dict{Int, Int}(
    1 => 2,
    3 => 4)

# Allowed when `variable_dict_indent = true`, but
# will be changed to the first example when `variable_dict_indent = false`.
Dict{Int, Int}(
    1 => 2,
    3 => 4,
)
```
