# Custom Styles

The default style surrounds keyword arguments with whitespace. Suppose
we wanted to have no spaces, how could we do this? Using custom styles this turns
out to be easy.

First we'll define the style:

```julia
using JuliaFormatter, CSTParser
using JuliaFormatter: AbstractStyle, FST, State, add_node!
import JuliaFormatter: pretty, p_kw

struct CustomStyle <: AbstractStyle end

# this must be defined
getstyle(s::CustomStyle) = s
```

Next we'll create a function for the `p_kw` to dispatch on `CustomStyle`.

```julia
function p_kw(style::CustomStyle, cst::CSTParser.EXPR, s::State)
    t = FST(cst, 0)
    for a in cst
        add_node!(t, pretty(style, a, s), s, join_lines = true)
    end
    t
end
```

For comparison here's the default definition:

```julia
function p_kw(style::DefaultStyle, cst::CSTParser.EXPR, s::State)
    style = getstyle(style)
    t = FST(cst, nspaces(s))
    for a in cst
        if a.kind === Tokens.EQ
            add_node!(t, Whitespace(1), s)
            add_node!(t, pretty(style, a, s), s, join_lines = true)
            add_node!(t, Whitespace(1), s)
        else
            add_node!(t, pretty(style, a, s), s, join_lines = true)
        end
    end
    t
end
```

And that's it! All other functions will fallback to use `DefaultStyle`.

Finally, let's check the output:

```julia
julia> s = "foo(a,b, key1=val1, key3=val4)"
"foo(a,b, key1=val1, key3=val4)"

julia> format_text(s) |> print
foo(a, b, key1 = val1, key3 = val4)

julia> format_text(s, style=CustomStyle()) |> print
foo(a, b, key1=val1, key3=val4)
```

Nice! Looks like it's working.
