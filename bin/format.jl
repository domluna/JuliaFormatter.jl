#!/usr/bin/env julia

import Pkg

if isfile("Project.toml")
    # Okay, we're in $GITHUB_WORKSPACE, and we have a Julia package.
    project = read("Project.toml", String)
    if occursin("uuid = \"98e50ef6-434e-11e9-1051-2b60c6c9e899\"", project)
        # This package is JuliaFormatter itself. Let's use the copy stored here
        # instead of the last taged version
        Pkg.add(Pkg.PackageSpec(path = pwd()))
    else
        # This is a package that's not JuliaFormatter. Just use the last tagged
        # version of JuliaFormatter.jl.
        Pkg.add("JuliaFormatter")
    end
else
    # This doesn't look like a standard Julia package, but it might be a script.
    # In any case, it isn't JuliaFormatter.jl.
    Pkg.add("JuliaFormatter")
end

using JuliaFormatter

help = """
JuliaFormatter formats Julia (.jl) programs. The formatter is width-sensitive.

Without an explicit file or path, this help message is written to stdout.
Given a file, it operates on that file; given a directory, it operates on
all .jl files in that directory, recursively.  By default, JuliaFormatter overwrites
files with the reformatted source.

Usage:

    format.jl [flags] [path ...]

Flags:

    -i, --indent
        The number of spaces used for an indentation.

    -m, --margin
        The maximum number of characters of code on a single line.  Lines over the
        limit will be wrapped if possible. There are cases where lines cannot be wrapped
        and they will still end up wider than the requested margin.

    -v, --verbose
        Print the name of the files being formatted with relevant details.

    -h, --help
        Print this message.

    -o, --overwrite
        Writes the formatted source to a new file where the original
        filename is suffixed with _fmt, i.e. `filename_fmt.jl`.

    --always_for_in
        Always replaces `=` with `in` for `for` loops.
        Example: `for i = 1:10` will be transformed to `for i in 1:10`.

    --whitespace_typedefs
        Add whitespace in type definitions.
        Example: `Union{A <: B, C}` to `Union{A<:B,C}`.

    --whitespace_ops_in_indices`
        Add whitespace to binary ops in indices.
        Example: `arr[a + b]` to `arr[a+b]`.
        Additionally, if there's a colon `:` involved, parenthesis will be added to the LHS and RHS.
        Example: `arr[(i1 + i2):(i3 + i4)]` instead of `arr[i1+i2:i3+i4]`.
"""

function parse_opts!(args::Vector{String})
    i = 1
    opts = Dict{Symbol,Union{Int,Bool}}()
    while i ≤ length(args)
        arg = args[i]
        if arg[1] != '-'
            i += 1
            continue
        end
        if arg == "-i" || arg == "--indent"
            opt = :indent
        elseif arg == "-m" || arg == "--margin"
            opt = :margin
        elseif arg == "-v" || arg == "--verbose"
            opt = :verbose
        elseif arg == "-h" || arg == "--help"
            opt = :help
        elseif arg == "-o" || arg == "--overwrite"
            opt = :overwrite
        elseif arg == "--always_for_in"
            opt = :always_for_in
        elseif arg == "--whitespace_typedefs"
            opt = :whitespace_typedefs
        elseif arg == "--whitespace_ops_in_indices"
            opt = :whitespace_ops_in_indices
        else
            error("invalid option $arg")
        end
        if opt in (
            :verbose,
            :help,
            :always_for_in,
            :whitespace_typedefs,
            :whitespace_ops_in_indices,
        )
            opts[opt] = true
            deleteat!(args, i)
        elseif opt == :overwrite
            opts[opt] = false
            deleteat!(args, i)
        else
            i < length(args) || error("option $arg requires and argument")
            val = tryparse(Int, args[i+1])
            val != nothing || error("invalid value for option $arg: $(args[i+1])")
            opts[opt] = val
            deleteat!(args, i:i+1)
        end
    end
    return opts
end

opts = parse_opts!(ARGS)
if isempty(ARGS) || haskey(opts, :help)
    write(stdout, help)
    exit(0)
end
format(ARGS; opts...)
