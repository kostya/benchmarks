#!/usr/bin/env julia

using Pkg
Pkg.add("JuliaFormatter")

using JuliaFormatter: format_file
for file in ARGS
    format_file(file, verbose = true)
end
