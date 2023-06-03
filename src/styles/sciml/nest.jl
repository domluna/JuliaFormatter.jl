function n_binaryopcall!(ss::SciMLStyle, fst::FST, s::State; indent::Int = -1)
    style = getstyle(ss)
    line_margin = s.line_offset + length(fst) + fst.extra_margin
    if line_margin > s.opts.margin &&
       fst.ref !== nothing &&
       CSTParser.defines_function(fst.ref[])
        transformed = short_to_long_function_def!(fst, s)
        transformed && nest!(style, fst, s)
    end

    if findfirst(n -> n.typ === PLACEHOLDER, fst.nodes) !== nothing
        n_binaryopcall!(DefaultStyle(style), fst, s; indent = indent)
        return
    end

    start_line_offset = s.line_offset
    walk(increment_line_offset!, (fst.nodes::Vector)[1:end-1], s, fst.indent)
    nest!(style, fst[end], s)
end
