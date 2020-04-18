# Configuration File

From v0.4.2, JuliaFormatter offers [`.prettierrc` style](https://prettier.io/docs/en/configuration.html) configuration file support.
This means you can specify [Formatting Options](@ref) in `.JuliaFormatter.toml` file and share that with others.

When [`format`](@ref) called, it will automatically searches for `.JuliaFormatter.toml` in the directory of the file being formatted,
and overwrite the current options with those defined in the file if exists.

!!! note
    [Juno](https://junolab.org/), a Julia IDE that offers formatting feature using this package, also respects configuration file.
    When you use `Julia-Client: Format-Code` command, Juno automatically searches for the configuration file in the directory of the current editor.

## Basic Configuration

In `.JuliaFormatter.toml`, you can specify any of [Formatting Options](@ref) in TOML, e.g. if you have
> hogedir/.JuliaFormatter.toml
```toml
indent = 2
margin = 100
```
then files under `hogedir` will be formatted with 2 spaces indentation and the maximum line length 100.

!!! note
    Currently the configuration file doesn't support [Custom Styles](@ref).
    For the time being, we only provide [YAS Style](@ref) support for the configuration file.
    In order to use YAS style, you can just specify:
    > .JuliaFormatter.toml
    ```toml
    ...
    style = "yas"
    ...
    ```
    Any other value will fallback to the default style.

## Search Rule

Configurations in .JuliaFormatter.toml will be applied to files in or below the directory where it appears.
So if you have:
```
dir
├─ code.jl
└─ subdir
   ├─ .JuliaFormatter.toml
   └─ sub_code.jl
```
and call `format("dir")`, then the configurations defined in `dir/subdir/.JuliaFormatter.toml` will only be applied to `dir/subdir/sub_code.jl`, but not to `dir/code.jl`.

If there are multiple `.JuliaFormatter.toml` files, the _deepest_ configuration has the precedence. For example, if you have
```
dir
├─ .JuliaFormatter.toml
├─ code.jl
├─ subdir1
│  ├─ .JuliaFormatter.toml
│  └─ sub_code1.jl
└─ subdir2
   └─ sub_code2.jl
```
and call `format("dir")`, `code.jl` and `sub_code2.jl` will be formatted according to rules defined in `dir/.JuliaFormatter.toml`,
while formatting `sub_code1.jl` will be configured by `dir/subdir1/.JuliaFormatter.toml`.
