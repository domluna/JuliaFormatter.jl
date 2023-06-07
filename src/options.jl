const VALID_FOR_IN_OPERATORS = ("in", "=", "∈")

Base.@kwdef struct Options
    indent::Int = 4
    margin::Int = 92
    always_for_in::Union{Bool,Nothing} = false
    for_in_replacement::String = "in"
    whitespace_typedefs::Bool = false
    whitespace_ops_in_indices::Bool = false
    remove_extra_newlines::Bool = false
    import_to_using::Bool = false
    pipe_to_function_call::Bool = false
    short_to_long_function_def::Bool = false
    long_to_short_function_def::Bool = false
    always_use_return::Bool = false
    whitespace_in_kwargs::Bool = true
    annotate_untyped_fields_with_any::Bool = true
    format_docstrings::Bool = false
    align_struct_field::Bool = false
    align_assignment::Bool = false
    align_conditional::Bool = false
    align_pair_arrow::Bool = false
    conditional_to_if::Bool = false
    normalize_line_endings::String = "auto"
    align_matrix::Bool = false
    join_lines_based_on_source::Bool = false
    trailing_comma::Union{Bool,Nothing} = true
    trailing_zero::Bool = true
    indent_submodule::Bool = false
    separate_kwargs_with_semicolon::Bool = false
    surround_whereop_typeparameters::Bool = true
    config_applied::Bool = false
    ignore::Vector{String} = String[]
    variable_call_indent::Vector{String} = []
    yas_style_nesting::Bool = false
end

function needs_alignment(opts::Options)
    opts.align_struct_field ||
        opts.align_conditional ||
        opts.align_assignment ||
        opts.align_pair_arrow ||
        opts.align_matrix
end

valid_for_in_op(s::String) = s in VALID_FOR_IN_OPERATORS
valid_for_in_op(_) = false
