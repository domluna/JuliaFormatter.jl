#! format: off
@setup_workload begin
    dir = joinpath(@__DIR__,"..", "..")
    str = raw"""
       @noinline require_complete(m::Matching) =
           m.inv_match === nothing && throw(ArgumentError("Backwards matching not defined. `complete` the matching first."))
    """
    sandbox_dir = joinpath(tempdir(), join(rand('a':'z', 40)))
    mkdir(sandbox_dir)
    cp(dir, sandbox_dir; force = true)

    @compile_workload begin
        format(dir)
        for style = [DefaultStyle(), BlueStyle(), SciMLStyle(), YASStyle(), MinimalStyle()]
          format_text(str, style)
        end
    end
end
