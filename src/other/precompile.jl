#! format: off
function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    # pretty
    Base.precompile(Tuple{typeof(pretty),DefaultStyle,CSTParser.EXPR,State})
    Base.precompile(Tuple{typeof(pretty),YASStyle,CSTParser.EXPR,State})
    Base.precompile(Tuple{typeof(pretty),BlueStyle,CSTParser.EXPR,State})
    Base.precompile(Tuple{typeof(pretty),MinimalStyle,CSTParser.EXPR,State})
end
using SnoopPrecompile
@precompile_setup begin
    dir = joinpath(@__DIR__,"../..")
    @precompile_all_calls begin
        format(dir)
    end
end
