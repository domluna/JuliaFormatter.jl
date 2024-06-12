struct Document
    srcfile::JuliaSyntax.SourceFile
    format_skips::Vector{Tuple{Int,Int}}
    # Line where there's a noindent comment. "#! format: noindent". This is checked during
    # the print stage to know if the contents of the block (recursive) should not be indented.
    noindent_blocks::Vector{Int}
end

function Document(text::AbstractString)
    format_skips = Tuple{Int,Int}[]
    noindent_blocks = Int[]
    stack = Int[]
    format_on = true

    srcfile = JuliaSyntax.SourceFile(text)
    for t in JuliaSyntax.tokenize(text)
        if t.head.kind === K"Comment"
            if length(stack) == 0 && occursin(r"^#!\s*format\s*:\s*off\s*$", val)
                # There should not be more than 1
                # "off" tag on the stack at a time.
                offset = first(t.range)
                r = JuliaSyntax.source_line_range(s.doc.srcfile, offset)
                push!(stack, first(r))
                format_on = false
            elseif length(stack) > 0 && occursin(r"^#!\s*format\s*:\s*on\s*$", val)
                # If "#! format: off" has not been seen
                # "#! format: on" is treated as a normal comment.
                offset = last(t.range)
                r = JuliaSyntax.source_line_range(s.doc.srcfile, offset)
                push!(format_skips, (pop!(stack), last(r)))
                format_on = true
            end
            if occursin(r"^#!\s*format\s*:\s*noindent\s*$", val)
                line = JuliaSyntax.source_line(srcfile, first(t.range))
                push!(noindent_blocks, line)
            end
        end
    end

    # If there is a SINGLE "#! format: off" tag
    # do not format from the "off" tag onwards.
    if length(stack) == 1 && length(format_skips) == 0
        # -1 signifies everything afterwards "#! format: off"
        # will not formatted.
        push!(format_skips, (stack[1], -1))
    end

    return Document(srcfile, format_skips, noindent_blocks)
end

function has_noindent_block(d::Document, r::Tuple{Int,Int})
    for b in d.noindent_blocks
        b in r && return true
    end
    return false
end
