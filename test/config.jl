@testset "configuration" begin
    config2 = "indent = 2"
    config4 = "indent = 4"
    before = "begin rand() end\n"
    after2 = "begin\n  rand()\nend\n"
    after4 = "begin\n    rand()\nend\n"

    # test basic configuration case
    # test_basic_config
    # ├─ .JuliaFormatter.toml (config2)
    # └─ code.jl (before -> after2)
    sandbox_dir = joinpath(@__DIR__, "test_basic_config")
    mkdir(sandbox_dir)
    try
        config_path = joinpath(sandbox_dir, CONFIG_FILE_NAME)
        code_path = joinpath(sandbox_dir, "code.jl")
        open(io -> write(io, config2), config_path, "w")
        open(io -> write(io, before), code_path, "w")

        format(code_path)
        @test read(code_path, String) == after2
    finally
        rm(sandbox_dir; recursive = true)
    end

    # test upward config search
    # test_search
    # ├─ .JuliaFormatter.toml (config2)
    # └─ sub
    #    ├─ sub_code.jl (before -> after2)
    #    └─ subsub
    #       └─ subsub_code.jl (before -> after2)
    sandbox_dir = joinpath(@__DIR__, "test_search")
    mkdir(sandbox_dir)
    try
        config_path = joinpath(sandbox_dir, CONFIG_FILE_NAME)
        sub_dir = joinpath(sandbox_dir, "sub")
        mkdir(sub_dir)
        sub_code_path = joinpath(sub_dir, "sub_code.jl")
        subsub_dir = joinpath(sub_dir, "sub")
        mkdir(subsub_dir)
        subsub_code_path = joinpath(subsub_dir, "sub_code.jl")
        open(io -> write(io, config2), config_path, "w")
        open(io -> write(io, before), sub_code_path, "w")
        open(io -> write(io, before), subsub_code_path, "w")

        format(sub_code_path)
        @test read(sub_code_path, String) == after2
        format(subsub_code_path)
        @test read(subsub_code_path, String) == after2
    finally
        rm(sandbox_dir; recursive = true)
    end

    # test basic directory walk
    # test_basic_walk
    # ├─ .JuliaFormatter.toml (config2)
    # ├─ code.jl (before -> after2)
    # └─ sub
    #    └─ sub_code.jl (before -> after2)
    sandbox_dir = joinpath(@__DIR__, "test_basic_walk")
    mkdir(sandbox_dir)
    try
        config_path = joinpath(sandbox_dir, CONFIG_FILE_NAME)
        code_path = joinpath(sandbox_dir, "code.jl")
        sub_dir = joinpath(sandbox_dir, "sub")
        mkdir(sub_dir)
        sub_code_path = joinpath(sub_dir, "sub_code.jl")
        open(io -> write(io, config2), config_path, "w")
        open(io -> write(io, before), code_path, "w")
        open(io -> write(io, before), sub_code_path, "w")

        format(sandbox_dir)
        @test read(code_path, String) == after2
        @test read(sub_code_path, String) == after2
    finally
        rm(sandbox_dir; recursive = true)
    end

    # test directory walk with nested configs
    # test_nested_config
    # ├─ .JuliaFormatter.toml (config2)
    # ├─ code.jl (before -> after2)
    # ├─ sub1
    # │  ├─ .JuliaFormatter.toml (config4)
    # │  └─ sub_code1.jl (before -> after4)
    # └─ sub2
    #    └─ sub_code2.jl (before -> after2)
    sandbox_dir = joinpath(@__DIR__, "test_nested_config")
    mkdir(sandbox_dir)
    try
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

    # test directory walk with nested configs
    # same as above except format from within the
    # top level directory, i.e. `format(".")`
    #
    # test_nested_config
    # ├─ .JuliaFormatter.toml (config2)
    # ├─ code.jl (before -> after2)
    # ├─ sub1
    # │  ├─ .JuliaFormatter.toml (config4)
    # │  └─ sub_code1.jl (before -> after4)
    # └─ sub2
    #    └─ sub_code2.jl (before -> after2)
    sandbox_dir = joinpath(@__DIR__, "test_nested_config")
    mkdir(sandbox_dir)
    try
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

        cd(sandbox_dir)
        format(".")
        @test read(code_path, String) == after2
        @test read(sub_code1_path, String) == after4
        @test read(sub_code2_path, String) == after2
    finally
        rm(sandbox_dir; recursive = true)
    end
end
