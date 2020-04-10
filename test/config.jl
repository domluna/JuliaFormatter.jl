@testset "configuration" begin
    before = "begin rand() end\n"
    config = "indent = 2"
    after4 = "begin\n    rand()\nend\n"
    after2 = "begin\n  rand()\nend\n"

    # test a case when separate file paths are given
    # ├─ sandbox1
    # │  └─ code1.jl (before -> after4)
    # └─ sandbox2
    #    ├─ .JuliaFormatter.toml (config2)
    #    └─ code2.jl (before -> after2)
    sandbox1_dir = mktempdir(@__DIR__)
    sandbox2_dir = mktempdir(@__DIR__)
    try
        code1_path = joinpath(sandbox1_dir, "code1.jl")
        config2_path = joinpath(sandbox2_dir, CONFIG_FILE_NAME)
        code2_path = joinpath(sandbox2_dir, "code2.jl")
        open(io -> write(io, before), code1_path, "w")
        open(io -> write(io, config), config2_path, "w")
        open(io -> write(io, before), code2_path, "w")

        format((code1_path, code2_path))
        @test read(code1_path, String) == after4
        @test read(code2_path, String) == after2
    finally
        rm(sandbox1_dir; recursive = true)
        rm(sandbox2_dir; recursive = true)
    end
end

@testset "configuration directory walk" begin
    before = "begin rand() end\n"
    after4 = "begin\n    rand()\nend\n"
    after2 = "begin\n  rand()\nend\n"
    config4 = "indent = 4"
    config2 = "indent = 2"

    # test basic
    # basic_test
    # ├─ .JuliaFormatter.toml (config2)
    # └─ code.jl (before -> after2)
    sandbox_dir = joinpath(@__DIR__, "basic_test")
    try
        mkdir(sandbox_dir)
        config_path = joinpath(sandbox_dir, CONFIG_FILE_NAME)
        code_path = joinpath(sandbox_dir, "code.jl")
        open(io -> write(io, config2), config_path, "w")
        open(io -> write(io, before), code_path, "w")

        format(sandbox_dir)
        @test read(code_path, String) == after2
    finally
        rm(sandbox_dir; recursive = true)
    end

    # test rules only applies to sub directory
    # subdir_test
    # ├─ code.jl (before -> after4)
    # └─ sub
    #    ├─ .JuliaFormatter.toml (config2)
    #    └─ sub_code.jl (before -> after2)
    sandbox_dir = joinpath(@__DIR__, "subdir_test")
    try
        mkdir(sandbox_dir)
        sub_dir = joinpath(sandbox_dir, "sub")
        mkdir(sub_dir)
        code_path = joinpath(sandbox_dir, "code.jl")
        sub_config_path = joinpath(sub_dir, CONFIG_FILE_NAME)
        sub_code_path = joinpath(sub_dir, "sub_code.jl")
        open(io -> write(io, before), code_path, "w")
        open(io -> write(io, config2), sub_config_path, "w")
        open(io -> write(io, before), sub_code_path, "w")

        format(sandbox_dir)
        @test read(code_path, String) == after4
        @test read(sub_code_path, String) == after2
    finally
        rm(sandbox_dir; recursive = true)
    end

    # test configs defined in nested way
    # sandbox_dir
    # ├─ .JuliaFormatter.toml (config2)
    # ├─ code.jl (before -> after2)
    # ├─ sub1
    # │  ├─ .JuliaFormatter.toml (config4)
    # │  └─ sub_code1.jl (before -> after4)
    # └─ sub2
    #    └─ sub_code2.jl (before -> after2)
    sandbox_dir = joinpath(@__DIR__, "test_nested")
    try
        mkdir(sandbox_dir)
        sub1_dir = joinpath(sandbox_dir, "sub1")
        sub2_dir = joinpath(sandbox_dir, "sub2")
        mkdir(sub1_dir)
        mkdir(sub2_dir)
        config_path = joinpath(sandbox_dir, CONFIG_FILE_NAME)
        code_path = joinpath(sandbox_dir, "code.jl")
        sub_config1_path = joinpath(sub1_dir, CONFIG_FILE_NAME)
        sub_code1_path = joinpath(sub1_dir, "sub_code1.jl")
        sub_code2_path = joinpath(sub2_dir, "sub_code2.jl")
        open(io -> write(io, config2), config_path, "w")
        open(io -> write(io, before), code_path, "w")
        open(io -> write(io, config4), sub_config1_path, "w")
        open(io -> write(io, before), sub_code1_path, "w")
        open(io -> write(io, before), sub_code2_path, "w")

        format(sandbox_dir)
        @test read(code_path, String) == after2
        @test read(sub_code1_path, String) == after4
        @test read(sub_code2_path, String) == after2
    finally
        rm(sandbox_dir; recursive = true)
    end
end
