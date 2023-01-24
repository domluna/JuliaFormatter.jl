#! format: off
using SnoopPrecompile
@precompile_setup begin
    dir = joinpath(@__DIR__,"..", "..")
    str = raw"""
       @noinline require_complete(m::Matching) =
           m.inv_match === nothing && throw(ArgumentError("Backwards matching not defined. `complete` the matching first."))
    """
    @precompile_all_calls begin
        format(dir)
        for style = [DefaultStyle(), BlueStyle(), SciMLStyle(), YASStyle(), MinimalStyle()]
          format_text(str, style)
        end
    end
end
