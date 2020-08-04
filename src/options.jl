Base.@kwdef struct Options
    indent_size::Int = 4
    max_margin::Int = 92
    always_for_in::Bool = false
    whitespace_typedefs::Bool = false
    whitespace_ops_in_indices::Bool = false
    remove_extra_newlines::Bool = false
    import_to_using::Bool = false
    pipe_to_function_call::Bool = false
    short_to_long_function_def::Bool = false
    always_use_return::Bool = false
    whitespace_in_kwargs::Bool = true
    annotate_untyped_fields_with_any::Bool = true
    format_docstrings::Bool = false
    align_struct_fields::Bool = false
end
