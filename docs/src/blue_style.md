# Blue Style

```@docs
BlueStyle
```

## Configuration File Example

The `.JuliaFormatter.toml` which represents these settings is

```toml
style = "blue"
```

Or to use `BlueStyle` except change one of the settings:

```toml
style = "yas"
remove_extra_newlines = false
```

## Direct Usage

```julia
format("file.jl", style=BlueStyle())
```

Or to use `BlueStyle` except change one of the settings:

```julia
format("file.jl", style=BlueStyle(), remove_extra_newlines=false)
```
