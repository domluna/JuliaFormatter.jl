#! format: off
function _precompile_()
    # pretty
    Base.precompile(Tuple{typeof(pretty),DefaultStyle,CSTParser.EXPR,State})
    Base.precompile(Tuple{typeof(pretty),YASStyle,CSTParser.EXPR,State})
    Base.precompile(Tuple{typeof(pretty),BlueStyle,CSTParser.EXPR,State})
end
