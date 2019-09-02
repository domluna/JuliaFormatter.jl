#!/usr/bin/env julia

using JuliaFormatter

help = """
JuliaFormatter formats Julia (.jl) programs. The formatter is width-sensitive.

Without an explicit file or path, this help message is written to stdout.
Given a file, it operates on that file; given a directory, it operates on
all .jl files in that directory, recursively (Files starting with a nofmt comment are ignored).
By default, JuliaFormatter overwrites files with the reformatted source.

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
        Print the name of the files being formatted.
    -h, --help
        Print this message.
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
        else
            error("invalid option $arg")
        end
        if opt == :verbose || opt == :help
            opts[opt] = true
            deleteat!(args, i)
            continue
        end
        i < length(args) || error("option $arg requires and argument")
        val = tryparse(Int, args[i+1])
        val != nothing || error("invalid value for option $arg: $(args[i+1])")
        opts[opt] = val
        deleteat!(args, i:i+1)
    end
    return opts
end

@info "" ARGS
opts = parse_opts!(ARGS)
@info "" opts

if isempty(ARGS) || haskey(opts, :help)
    write(stdout, help)
    exit(0)
end
format(ARGS; opts...)
