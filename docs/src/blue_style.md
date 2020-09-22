# Blue Style

Formatting style based on https://github.com/invenia/BlueStyle
and https://github.com/domluna/JuliaFormatter.jl/issues/283

Recommended options are:

- `always_use_return` = true
- `short_to_long_function_def` = true
- `whitespace_ops_in_indices` = true
- `remove_extra_newlines` = true
- `always_for_in` = true
- `import_to_using` = true
- `pipe_to_function_call` = true
- `whitespace_in_kwargs` = false

## Configuration File Example

The `.JuliaFormatter.toml` which represents these settings is

```toml
style = "blue"
always_use_return = true
short_to_long_function_def = true
whitespace_ops_in_indices = true
remove_extra_newlines = true
always_for_in = true
import_to_using = true
pipe_to_function_call = true
whitespace_in_kwargs = false
```


## Direct Usage

```julia
format("file.jl", style=BlueStyle(), ...)
```

