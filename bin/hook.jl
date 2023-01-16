#!/usr/bin/env julia
using JuliaFormatter

@debug ARGS
filename = ARGS[1]
exit(format_file(filename) ? 0 : 1)
