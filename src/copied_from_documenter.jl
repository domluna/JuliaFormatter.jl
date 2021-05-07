# The content of this file was directly copied from Documenter.jl

# REPL doctest splitter.

const PROMPT_REGEX = r"^julia> (.*)$"
const SOURCE_REGEX = r"^       (.*)$"

function repl_splitter(code)
    lines = split(string(code, "\n"), '\n')
    input = String[]
    output = String[]
    buffer = IOBuffer() # temporary buffer for doctest inputs and outputs
    found_first_prompt = false
    while !isempty(lines)
        line = popfirst!(lines)
        prompt = match(PROMPT_REGEX, line)
        # We allow comments before the first julia> prompt
        !found_first_prompt && startswith(line, '#') && continue
        if prompt === nothing
            source = match(SOURCE_REGEX, line)
            if source === nothing
                savebuffer!(input, buffer)
                println(buffer, line)
                takeuntil!(PROMPT_REGEX, buffer, lines)
            else
                println(buffer, source[1])
            end
        else
            found_first_prompt = true
            savebuffer!(output, buffer)
            println(buffer, prompt[1])
        end
    end
    savebuffer!(output, buffer)
    zip(input, output)
end

function savebuffer!(out, buf)
    n = bytesavailable(seekstart(buf))
    n > 0 ? push!(out, rstrip(String(take!(buf)))) : out
end

function takeuntil!(r, buf, lines)
    while !isempty(lines)
        line = lines[1]
        if !occursin(r, line)
            println(buf, popfirst!(lines))
        else
            break
        end
    end
end
