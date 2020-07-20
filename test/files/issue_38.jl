

"""
Free space in Gb at the given path.
"""
free_space(path) = shutil.disk_usage(path)[3] / 1000_000_000


settings = ArgParseSettings()

@add_arg_table settings begin
    """
    --file
    """
    help = """
    path to the file.
    ...
    """
end

@add_arg_table settings begin
    "--output", "-o"
    help = "output..."
    default = "."
    """
    --file
    """
    help = """
    path to the file.
    ...
    """
    default = ""
end
