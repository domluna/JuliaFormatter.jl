@testset "Format repo" begin
    try
        sandbox_dir = joinpath(tempdir(), "format_repo_test", join(rand('a':'z', 20)))
        mkdir(sandbox_dir)
        cp(@__DIR__, sandbox_dir; force = true)

        # initial format
        format(sandbox_dir)

        # follow up formats should be the same
        @test format(sandbox_dir, join_lines_based_on_source = true) == true
        @test format(sandbox_dir, join_lines_based_on_source = true, margin = 10_000) ==
              true
    finally
        try
            rm(sandbox_dir; recursive = true)
        catch
        end
    end
end
