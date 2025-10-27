# Command Line Interface

JuliaFormatter provides a command-line executable `jlfmt` for formatting Julia source code.

## Installation

Install using Julia's app manager:

```julia
pkg> app add JuliaFormatter
```

This makes the `jlfmt` command available in your `PATH`.

Alternatively, invoke directly without installation:

```bash
julia -m JuliaFormatter [<options>] <path>...
```

!!! note "Runic Compatibility"
    The CLI interface is designed to be compatible with [Runic.jl](https://github.com/fredrikekre/Runic.jl)'s CLI where possible, making it easier to switch between formatters.

    !!! warning "Missing features"
        Note that the `--lines` option is not yet implemented.

## Quick Start

```bash
# Preview formatted output
jlfmt src/file.jl

# Check if files are already formatted with verbose mode
jlfmt --check -v src/

# Format files in-place with multiple threads
jlfmt --threads=6 -- --inplace -v src/

# Show diff without modifying
jlfmt --diff src/file.jl
```

## Options

Run `jlfmt --help` for a complete list:

```@repl
using JuliaFormatter # hide
JuliaFormatter.main(["--help"]); # hide
```

## Configuration Files

`jlfmt` searches for [`.JuliaFormatter.toml` configuration](@ref config) files starting from each input file's directory and walking up the directory tree.

By default, command-line options override configuration file settings:

```bash
# Use indent=2 even if config file specifies indent=4
jlfmt --indent=2 src/file.jl
```

Use `--prioritize-config-file` to make configuration file settings take precedence (might be useful for language server integration):

```bash
jlfmt --prioritize-config-file --indent=2 src/file.jl
```

### Configuration with stdin

When formatting from stdin, no configuration file is used by default.
Use `--config-dir` to specify a directory for configuration file lookup:

```bash
# Format stdin using config from ./src directory
echo 'f(x,y)=x+y' | jlfmt --config-dir=./src

# Useful in editor integrations to respect project config
cat file.jl | jlfmt --config-dir=$(dirname file.jl)
```

The formatter will search for `.JuliaFormatter.toml` in the specified directory and its parent directories, just like it does for regular file inputs.

## Conventions

`jlfmt` follows standard CLI conventions:
- Exit code 0 on success
- Exit code 1 on formatting errors or when `--check` detects unformatted files
- Formatted output to stdout (default) or in-place with `--inplace`
- Error messages to stderr
