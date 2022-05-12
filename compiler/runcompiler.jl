using PackageCompiler

function compile_app()
    create_app(
        dirname(@__DIR__),
        joinpath(@__DIR__, "build");
        precompile_execution_file = joinpath(dirname(@__DIR__), "test", "runtests.jl"),
        cpu_target = "generic;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1)",
        incremental = false,
        force = true,
    )
end
