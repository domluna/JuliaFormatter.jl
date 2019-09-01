#!/usr/bin/env julia

using JuliaFormatter: parse_commandline, format

opts = parse_commandline()
file = opts[:path]
delete!(opts, :path)
format(file; opts...)

