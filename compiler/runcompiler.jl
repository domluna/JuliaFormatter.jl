using PackageCompiler, Test, TOML

"Compiles JuliaFormatter into a relocatable binary inside `./JuliaFormatter-x.x.x`. Removes any existing build."
function compile_app()
    create_app(
        dirname(@__DIR__),
        joinpath(@__DIR__, "JuliaFormatter-$(get_version_number())");
        precompile_execution_file = joinpath(dirname(@__DIR__), "test", "runtests.jl"),
        cpu_target = "generic;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1)",
        incremental = false,
        force = true,
    )
end

"Tests the binary inside `./JuliaFormatter-x.x.x` against a simple regression suite."
function test_app()
    sandbox = mkpath(mktempdir())
    try
        sandbox_build_path = joinpath(sandbox, "build")
        testfile = abspath(joinpath(sandbox, "test_app.jl"))
        cp(joinpath(@__DIR__, "test_app.jl"), testfile)
        cp(joinpath(@__DIR__, "JuliaFormatter-$(get_version_number())"), sandbox_build_path)
        run(`$(joinpath(sandbox_build_path, "bin", "JuliaFormatter")) $testfile`)

        expected = """
        a = 10


        # test



        f(a = 1; b = 2)

        for iter in I
            arg
        end
        """
        @test read(testfile, String) == expected
    finally
        rm(sandbox; recursive = true)
    end
end

"Returns JuliaFormatter's version number."
function get_version_number()
    projecttoml_filename = joinpath(dirname(dirname(@__FILE__)), "Project.toml")
    projecttoml_parsed = TOML.parse(read(projecttoml_filename, String))
    return VersionNumber(projecttoml_parsed["version"])
end
