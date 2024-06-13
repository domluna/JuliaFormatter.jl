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
    inline_comments::Dict{Int,Tuple{Int,String}}
    comments::Dict{Int,Tuple{Int,String}}
    semicolons = Dict{Int,Vector{Int}}()

    srcfile = JuliaSyntax.SourceFile(text)
    tokens = JuliaSyntax.tokenize(text)
    for (i, t) in enumerate(tokens)
        kind = t.head.kind
        if kind === K"Comment"

            ws = 0
            if i > 1 && tokens[i-1].head.kind === "Whitespace"
                r = tokens[i-1].head.range
                ws = last(r) - first(r)
            end

            startline = JuliaSyntax.source_line(srcfile, first(t.range))
            endline = JuliaSyntax.source_line(srcfile, last(t.range))
            val = text[first(t.range):last(t.range)]
            if startline == endline
                if ws > 0
                inline_comments[line] = (ws, val)
            else
                comments[line] = (ws, val)
                end
            else
                # multiline comment
                idx = findfirst(x -> x == '\n', val) + 1
                fc2 = findfirst(c -> !isspace(c), val[idx:end])

                lr = JuliaSyntax.source_line_range(srcfile, first(t.range))
                lineoffset = (first(t.range) - lr[1]) + 1
                ws2 = fc2 === nothing || lineoffset < fc2 ? ws : max(ws, fc2 - 1)

                line = startline
                offset = goffset
                cs = ""
                for (i, c) in enumerate(val)
                    cs *= c
                    if c == '\n'
                        s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                        push!(ranges, s:offset+1)
                        fc = findfirst(c -> !isspace(c), cs)
                        if fc === nothing
                            # comment is all whitespace
                            comments[line] = (ws, cs[end:end])
                        else
                            idx = min(fc, ws + 1)
                            comments[line] = (ws, cs[idx:end])
                        end
                        line += 1
                        ws = ws2
                        cs = ""
                    end
                    offset += 1
                end
                # last comment
                idx = min(findfirst(c -> !isspace(c), cs)::Int, ws + 1)
                comments[line] = (ws, cs[idx:end])
            end

            if length(stack) == 0 && occursin(r"^#!\s*format\s*:\s*off\s*$", val)
                # There should not be more than 1
                # "off" tag on the stack at a time.
                offset = first(t.range)
                r = JuliaSyntax.source_line_range(srcfile, offset)
                push!(stack, first(r))
                format_on = false
            elseif length(stack) > 0 && occursin(r"^#!\s*format\s*:\s*on\s*$", val)
                # If "#! format: off" has not been seen
                # "#! format: on" is treated as a normal comment.
                offset = last(t.range)
                r = JuliaSyntax.source_line_range(srcfile, offset)
                push!(format_skips, (pop!(stack), last(r)))
                format_on = true
            end
            if occursin(r"^#!\s*format\s*:\s*noindent\s*$", val)
                line = JuliaSyntax.source_line(srcfile, first(t.range))
                push!(noindent_blocks, line)
            end
        elseif kind === K"ErrorEofMultiComment"
                l = JuliaSyntax.source_line(srcfile, first(t.range))
             throw(
                ErrorException(
                    """Unable to format. Multi-line comment on line $l is not closed.""",
                ),
            )
        elseif kind === K"CmdString" || kind == K"String"
        elseif kind === K";"
            line = JuliaSyntax.source_line(srcfile, first(t.range))
             if haskey(semicolons, line)
                if i > 1 && tokens[i-1].head.kind === K";"
                    semicolons[line][end] += 1
                else
                    push!(semicolons[line], 1)
                end
            else
                semicolons[line] = Int[1]
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
