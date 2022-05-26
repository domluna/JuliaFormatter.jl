using PackageCompiler, Test, TOML

"Compiles JuliaFormatter into a relocatable binary inside `./JuliaFormatter-x.x.x`. Removes any existing build."
function compile_app()
    # Pkg.dev(dirname(@__DIR__))
    create_app(
        dirname(@__DIR__),
        joinpath(@__DIR__, get_build_name());
        precompile_execution_file = joinpath(dirname(@__DIR__), "test", "runtests.jl"),
        # set this cpu target to compile for a generic arch
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
        cp(joinpath(@__DIR__, get_build_name()), sandbox_build_path)
        
        # The exit code should be 1 as the code is not formatted
        @test_throws ProcessFailedException run(`$(joinpath(sandbox_build_path, "bin", "JuliaFormatter")) $testfile`)

        expected = """
        a = 10


        # test



        f(a = 1; b = 2)

        for iter in I
            arg
        end
        """
        # The code should have been formatted
        @test read(testfile, String) == expected

        # With the code formatted, the exit code should be 0 so this should not throw
        run(`$(joinpath(sandbox_build_path, "bin", "JuliaFormatter")) $testfile`)
    finally
        rm(sandbox; recursive = true)
    end
end

function get_build_name()
    "JuliaFormatter-$(get_version_number())-$(Sys.ARCH)-$(Sys.KERNEL)"
end

"Returns JuliaFormatter's version number."
function get_version_number()
    projecttoml_filename = joinpath(dirname(dirname(@__FILE__)), "Project.toml")
    projecttoml_parsed = TOML.parse(read(projecttoml_filename, String))
    return VersionNumber(projecttoml_parsed["version"])
end
