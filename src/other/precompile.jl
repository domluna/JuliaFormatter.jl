# # ! format: off
# using PrecompileTools
# @setup_workload begin
#     dir = joinpath(@__DIR__, "..", "..")
#     sandbox_dir = joinpath(tempdir(), join(rand('a':'z', 40)))
#     mkdir(sandbox_dir)
#     cp(dir, sandbox_dir; force=true)
#     @compile_workload begin
#         for style in [DefaultStyle(), BlueStyle(), SciMLStyle(), YASStyle(), MinimalStyle()]
#             format(sandbox_dir, style)
#         end
#     end
# end
