#!/usr/bin/env julia
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

    -c, --check
        Check if the files are correctly formatted.  Return code 0 means nothing
        would change.  Return code 1 means some files would be reformatted.

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
        elseif arg == "-c" || arg == "--check"
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
exit(format(ARGS; opts...) ? 0 : 1)
