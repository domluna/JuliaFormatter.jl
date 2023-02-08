#!/usr/bin/env -S julia --startup-file=no -O1
using JuliaFormatter

@debug ARGS
filename = ARGS[1]
exit(format_file(filename) ? 0 : 1)
