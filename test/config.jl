@testset ".JuliaFormatter.toml config" begin
    config2 = "indent = 2"
    config4 = "indent = 4"
    before = "begin rand() end\n"
    after2 = "begin\n  rand()\nend\n"
    after4 = "begin\n    rand()\nend\n"

    # test basic configuration case
    # test_basic_config
    # ├─ .JuliaFormatter.toml (config2)
    # └─ code.jl (before -> after2)
    sandbox_dir = joinpath(tempdir(), "test_basic_config")
    mkdir(sandbox_dir)
    try
        config_path = joinpath(sandbox_dir, CONFIG_FILE_NAME)
        code_path = joinpath(sandbox_dir, "code.jl")
        open(io -> write(io, config2), config_path, "w")
        open(io -> write(io, before), code_path, "w")

        @test format(code_path) == false
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
    sandbox_dir = joinpath(tempdir(), "test_search")
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

        @test format(sub_code_path) == false
        @test read(sub_code_path, String) == after2
        @test format(subsub_code_path) == false
        @test read(subsub_code_path, String) == after2
        @test format(sub_code_path) == true
        @test format(subsub_code_path) == true
    finally
        rm(sandbox_dir; recursive = true)
    end

    # test basic directory walk
    # test_basic_walk
    # ├─ .JuliaFormatter.toml (config2)
    # ├─ code.jl (before -> after2)
    # └─ sub
    #    └─ sub_code.jl (before -> after2)
    sandbox_dir = joinpath(tempdir(), "test_basic_walk")
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

        @test format(sandbox_dir) == false
        @test read(code_path, String) == after2
        @test read(sub_code_path, String) == after2
        @test format(sandbox_dir) == true
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
    sandbox_dir = joinpath(tempdir(), "test_nested_config")
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

        @test format(sandbox_dir) == false
        @test read(code_path, String) == after2
        @test read(sub_code1_path, String) == after4
        @test read(sub_code2_path, String) == after2
        @test format(sandbox_dir) == true
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
    sandbox_dir = joinpath(tempdir(), "test_nested_config")
    mkdir(sandbox_dir)
    original_dir = pwd()
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
        @test format(".") == false
        @test read(code_path, String) == after2
        @test read(sub_code1_path, String) == after4
        @test read(sub_code2_path, String) == after2
        @test format(".") == true
    finally
        cd(original_dir)
        rm(sandbox_dir; recursive = true)
    end

    config2 = """
    indent = 2
    format_markdown = true
    """

    before = """
    # hello world

    ```julia
    begin body end
    ```
    - a
    -             b
    """
    after2 = """
    # hello world

    ```julia
    begin
      body
    end
    ```

      - a
      -             b
    """
    # test formatting a markdown file
    # test_basic_markdown_format
    # ├─ .JuliaFormatter.toml (config2)
    # └─ file.md (before -> after2)
    sandbox_dir = joinpath(tempdir(), "test_basic_config")
    mkdir(sandbox_dir)
    try
        config_path = joinpath(sandbox_dir, CONFIG_FILE_NAME)
        md_path = joinpath(sandbox_dir, "file.md")
        open(io -> write(io, config2), config_path, "w")
        open(io -> write(io, before), md_path, "w")

        @test format(md_path) == false
        @test read(md_path, String) == after2
    finally
        rm(sandbox_dir; recursive = true)
    end

    config_trailing_comma_nothing = """
    trailing_comma = "nothing"
    """

    code_trailing_comma = """
    const A_SET_OF_SYMBOLS_WITH_TRAILING_COMMA = Set([
        :accesses, :allowedtypes, :connector, :digits, :equals, :expand,
        :ignores, :sigdigits, :sort, :val_to_string,
    ])
    const A_SET_OF_SYMBOLS_WITHOUT_TRAILING_COMMA = Set([
        :accesses, :allowedtypes, :connector, :digits, :equals, :expand,
        :ignores, :sigdigits, :sort, :val_to_string
    ])
    """
    code_trailing_comma_after = """
    const A_SET_OF_SYMBOLS_WITH_TRAILING_COMMA = Set([
        :accesses,
        :allowedtypes,
        :connector,
        :digits,
        :equals,
        :expand,
        :ignores,
        :sigdigits,
        :sort,
        :val_to_string,
    ])
    const A_SET_OF_SYMBOLS_WITHOUT_TRAILING_COMMA = Set([
        :accesses,
        :allowedtypes,
        :connector,
        :digits,
        :equals,
        :expand,
        :ignores,
        :sigdigits,
        :sort,
        :val_to_string
    ])
    """
    # test `trailing_comma = "nothing"` in config (#539)
    # test_trailing_comma_nothing_config
    # ├─ .JuliaFormatter.toml (config_trailing_comma_nothing)
    # └─ code.jl (code_trailing_comma -> code_trailing_comma_after)
    sandbox_dir = joinpath(tempdir(), "test_trailing_comma_nothing_config")
    mkdir(sandbox_dir)
    try
        config_path = joinpath(sandbox_dir, CONFIG_FILE_NAME)
        code_path = joinpath(sandbox_dir, "code.jl")
        open(io -> write(io, config_trailing_comma_nothing), config_path, "w")
        open(io -> write(io, code_trailing_comma), code_path, "w")

        @test format(code_path) == false
        @test read(code_path, String) == code_trailing_comma_after
    finally
        rm(sandbox_dir; recursive = true)
    end
end
