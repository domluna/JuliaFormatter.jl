function n_call!(df::DocumentFormatStyle, fst::FST, s::State)
    n_call!(typeof(df.indentstyle)(df), fst, s)
end

@inline n_curly!(df::DocumentFormatStyle, fst::FST, s::State) = n_call!(df, fst, s)
@inline n_ref!(df::DocumentFormatStyle, fst::FST, s::State) = n_call!(df, fst, s)
@inline n_macrocall!(df::DocumentFormatStyle, fst::FST, s::State) = n_call!(df, fst, s)
@inline n_typedcomprehension!(df::DocumentFormatStyle, fst::FST, s::State) =
    n_call!(df, fst, s)

function n_tupleh!(df::DocumentFormatStyle, fst::FST, s::State)
    n_call!(typeof(df.indentstyle)(df), fst, s)
end
@inline n_braces!(df::DocumentFormatStyle, fst::FST, s::State) = n_tupleh!(df, fst, s)
@inline n_vect!(df::DocumentFormatStyle, fst::FST, s::State) = n_tupleh!(df, fst, s)
@inline n_parameters!(df::DocumentFormatStyle, fst::FST, s::State) = n_tupleh!(df, fst, s)
@inline n_invisbrackets!(df::DocumentFormatStyle, fst::FST, s::State) =
    n_tupleh!(df, fst, s)
@inline n_comprehension!(df::DocumentFormatStyle, fst::FST, s::State) =
    n_tupleh!(df, fst, s)

function n_generator!(df::DocumentFormatStyle, fst::FST, s::State)
    n_call!(typeof(df.indentstyle)(df), fst, s)
end
@inline n_filter!(df::DocumentFormatStyle, fst::FST, s::State) = n_generator!(df, fst, s)
@inline n_flatten!(df::DocumentFormatStyle, fst::FST, s::State) = n_generator!(df, fst, s)

function n_whereopcall!(df::DocumentFormatStyle, fst::FST, s::State)
    n_call!(typeof(df.indentstyle)(df), fst, s)
end

function n_using!(df::DocumentFormatStyle, fst::FST, s::State)
    n_call!(typeof(df.indentstyle)(df), fst, s)
end
@inline n_export!(df::DocumentFormatStyle, fst::FST, s::State) = n_using!(df, fst, s)
@inline n_import!(df::DocumentFormatStyle, fst::FST, s::State) = n_using!(df, fst, s)

n_chainopcall!(df::DocumentFormatStyle, fst::FST, s::State) =
    n_block!(DefaultStyle(df), fst, s, indent = s.line_offset)
n_comparison!(df::DocumentFormatStyle, fst::FST, s::State) =
    n_block!(DefaultStyle(df), fst, s, indent = s.line_offset)

function n_binaryopcall!(df::DocumentFormatStyle, fst::FST, s::State)
    idx = findfirst(n -> n.typ === PLACEHOLDER, fst.nodes)

    line_offset = s.line_offset

    nest!(df, fst.nodes[1:end-1], s, fst.indent)

    if idx !== nothing

        cst = fst.ref[]

        indent_nest =
            CSTParser.defines_function(cst) ||
            nest_assignment(cst) ||
            cst[2].kind === Tokens.PAIR_ARROW ||
            cst[2].kind === Tokens.ANON_FUNC ||
            is_standalone_shortcircuit(cst)

        if indent_nest
            s.line_offset = fst.indent + s.indent_size
            fst[idx] = Whitespace(s.indent_size)
            add_indent!(fst[end], s, s.indent_size)
        else
            fst.indent = line_offset
        end

    end

    nest!(df, fst.nodes[end], s)
end
