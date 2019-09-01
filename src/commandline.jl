using ArgParse

function parse_commandline()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--margin", "-m"
            help = "the maximum number of characters of code on a single line"
            arg_type = Int
            default = 92
        "--indent", "-i"
            help = "the number of spaces used for an indentation"
            arg_type = Int
            default = 4
        "path"
            help = "the path of a file to be formatted or the path of the directory to recursively format every .jl file in"
            required = true
    end

    return parse_args(s, as_symbols=true)
end
