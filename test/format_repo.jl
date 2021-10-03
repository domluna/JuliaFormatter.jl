@testset "Format repo" begin
    sandbox_dir = joinpath(tempdir(), "format_repo_test")
    mkdir(sandbox_dir)
    try
        cp(@__DIR__, sandbox_dir; force = true)

        # initial format
        format(sandbox_dir)

        # follow up formats should be the same
        @test format(sandbox_dir, join_lines_based_on_source = true) == true
        @test format(sandbox_dir, join_lines_based_on_source = true, margin = 10_000) ==
              true
    finally
        rm(sandbox_dir; recursive = true)
    end
end
