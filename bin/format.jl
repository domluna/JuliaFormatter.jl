#!/usr/bin/env julia

using JLFmt

function parse_opts!(args::Vector{String})
    i = 1
    opts = Dict{Symbol,Int}()
    while i ≤ length(args)
        arg = args[i]
        if arg[1] != '-'
            i += 1; continue
        end
        if arg == "-i" || arg == "--indent"
            opt = :indent
        elseif arg == "-m" || arg == "--margin"
            opt = :margin
        else
            error("invalid option $arg")
        end
        i < length(args) ||
            error("option $arg requires and argument")
        val = tryparse(Int, args[i + 1])
        val != nothing ||
            error("invalid value for option $arg: $(args[i+1])")
        opts[opt] = val
        deleteat!(args, i:i+1)
    end
    return opts
end

opts = parse_opts!(ARGS)
isempty(ARGS) && push!(ARGS, ".")
format(ARGS; opts...)
