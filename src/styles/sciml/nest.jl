for f in [
    :n_curly!,
    :n_ref!,
    :n_macrocall!,
    :n_typedcomprehension!,
    :n_braces!,
    :n_parameters!,
    :n_invisbrackets!,
    :n_comprehension!,
    :n_vcat!,
    :n_typedvcat!,
    :n_bracescat!,
    :n_generator!,
    :n_filter!,
    :n_flatten!,
    :n_using!,
    :n_export!,
    :n_import!,
    :n_chainopcall!,
    :n_comparison!,
    :n_for!,
    # :n_tuple!,
    # :n_call!,
    #:n_vect!
]
    @eval function $f(ss::SciMLStyle, fst::FST, s::State)
        style = getstyle(ss)
        if s.opts.yas_style_nesting
            $f(YASStyle(style), fst, s)
        else
            $f(DefaultStyle(style), fst, s)
        end
    end
end

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

    walk(increment_line_offset!, (fst.nodes::Vector)[1:end-1], s, fst.indent)
    nest!(style, fst[end], s)
end

function n_call!(ds::SciMLStyle, fst::FST, s::State)
    style = getstyle(ds)
    if s.opts.yas_style_nesting
        n_call!(YASStyle(style), fst, s)
        return
    end
    n_tuple!(ds, fst, s)
end

function n_tuple!(ds::SciMLStyle, fst::FST, s::State)
    style = getstyle(ds)
    if s.opts.yas_style_nesting
        n_tuple!(YASStyle(style), fst, s)
        return
    end

    n_tuple!(DefaultStyle(style), fst, s)
    if fst.ref !== nothing &&
       parent_is(fst.ref[], n -> is_function_or_macro_def(n) || n.head == :macrocall)
        add_indent!(fst, s, s.opts.indent)
        if is_closer(fst[end])
            fst[end].indent -= s.opts.indent
        end
    end
end
