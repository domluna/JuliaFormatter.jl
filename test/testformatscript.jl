module TestFormatScript
using Test
using JuliaFormatter
using JuliaFormatter: format_text

@testset "Test format.jl script" begin
    referencedir = mktempdir()
    testdir = mktempdir()

    # copy test files to reference and test directory
    testfiles = ["prettify_demo.jl", "issue_38.jl"]
    for file in testfiles
        cp(joinpath("files", file),
            joinpath(referencedir, file))
        cp(joinpath("files", file),
            joinpath(testdir, file))
    end

    # make sure that reference files where copied
    for file in testfiles
        @test isfile(joinpath(referencedir, file))
        @test isfile(joinpath(testdir, file))
    end

    # create reference formated files
    refformatstrings = []
    for file in testfiles
        format_file(joinpath(referencedir, file), overwrite=false)
        path, ext = splitext(file)
        formatedfile = joinpath(referencedir, "$(path)_fmt$(ext)")
        @test isfile(formatedfile)
        push!(refformatstrings, read(formatedfile, String))
    end

    # make sure the testscript path is valid
    script = joinpath(@__DIR__, "../bin/format.jl")
    @test isfile(script)



    olddir = pwd()
    try
        cd(testdir)
        @test pwd() == testdir

        # make sure that test files exist in test directory
        for file in testfiles
            @test isfile(file)
        end
        # and nothing more exists in directory
        @test length(readdir()) == length(testfiles)

        @testset "Execute with no arguments (display help)" begin
            # test script with no arguments
            @test_throws Exception run(`$script`)

            # make sure no files where changed or created
            @test length(readdir()) == length(testfiles)
            for (i, file) in enumerate(testfiles)
                reffile = joinpath(referencedir, file)
                testfile = joinpath(testdir, file)
                @test (read(reffile, String) == read(testfile, String))
            end
        end
        @testset "Format all files in directory" begin
            # test script with no arguments
            @test read(`$script .`, String) == ""

            for (i, file) in enumerate(testfiles)
                testfile = joinpath(testdir, file)
                @test (refformatstrings[i] == read(testfile, String))
            end
        end

        @testset "Test indent parameter" begin
            refformat = format_text(refformatstrings[1], indent=8)
            # make sure indent of 8 differs from normal formating
            @test refformat != read(testfiles[1], String)

            @test read(`$script $(testfiles[1]) --indent 8`, String) == ""
            @test refformat == read(testfiles[1], String)
        end

        @testset "Test margin parameter" begin
            refformat = format_text(refformatstrings[1], margin=60)
            # make sure margin of 60 differs from normal formating
            @test refformat != read(testfiles[1], String)

            @test read(`$script $(testfiles[1]) --margin 60`, String) == ""
            @test refformat == read(testfiles[1], String)
        end

        @testset "Test margin and indent parameter" begin
            refformat = format_text(refformatstrings[1], margin=60, indent=8)
            # make sure margin of 60 and indent of 8 differs from normal
            # formating
            @test refformat != read(testfiles[1], String)

            @test read(`$script $(testfiles[1]) -m 60 -i 8`, String) == ""
            @test refformat == read(testfiles[1], String)
        end
    finally
        cd(olddir)
    end
end
end

