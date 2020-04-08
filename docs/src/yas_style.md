# YAS Style

Formatting style based on [YASGuide](https://github.com/jrevels/YASGuide) and https://github.com/domluna/JuliaFormatter.jl/issues/198.

Recommended options for confirming with the above guide are:

- `always_for_in` = true
- `whitespace_ops_in_indices` = true
- `whitespace_typedefs` = false
- `remove_extra_newlines` = true
- `import_to_using` = true
- `pipe_to_function_call` = true
- `short_to_long_function_def` = true
- `always_use_return` = true

## Usage

```julia
format("file.jl", style=YASStyle(), ...)
```

## Differences from `DefaultStyle`

There are three main differences between `YASStyle` and `DefaultStyle`. They are based
on alignment and line break behaviors.

1. Arguments are aligned to just after the start of the *opener* `[, {, (, etc`.

```julia
function_call(arg1,
              arg2)
```

2. As you can see from the above the *closer* sticks to the final argument.

3. Nesting (line breaks) only occur when the margin of the next argument exceeds
the maximim limit.

```julia
function_call(arg1, arg2,
              arg3)
```

`arg3` exceeded the margin limit and so it was placed on the following line.


### Nesting `=`

Unlike `DefaultStyle`, assignment operations `=` are not nested. That
is, the following

```julia
my_function(arg1, arg2) = arg1 * arg2
```

Is not nested to


```julia
my_function(arg1, arg2) =
    arg1 * arg2
```

It is highly recommended setting `short_to_long_function_def` to `true`. This option
transforms the above to a long function definition if it exceeds the maximum margin.

```julia
function my_function(arg1, arg2)
    arg1 * arg2
end
```
