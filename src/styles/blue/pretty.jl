
struct BlueStyle <: AbstractStyle end
@inline getstyle(s::BlueStyle) = s

function nestable(::BlueStyle, cst::CSTParser.EXPR)
    is_assignment(cst) && is_iterable(cst[end]) && return false
    return nestable(DefaultStyle(), cst)
end
