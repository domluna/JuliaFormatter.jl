# #! format: off
# using PrecompileTools
# @setup_workload begin
#     str = raw"""
# @noinline require_complete(m::Matching) =
#    m.inv_match === nothing && throw(ArgumentError("Backwards matching not defined. `complete` the matching first."))
#
# # Macro definition
# macro twice(ex)
#     quote
#         $ex
#         $ex
#     end
# end
#
# # Type hierarchy
# abstract type Animal end
# struct Dog <: Animal
#     name::String
#     age::Int
# end
#
# # Multiple dispatch
# function speak(a::Animal)
#     println("Some generic animal sound")
# end
#
# function speak(d::Dog)
#     println("Woof!")
# end
#
# # Generator expression
# squares = (x^2 for x in 1:10 if x % 2 == 0)
#
# # Do-block syntax
# map(1:5) do x
#     if x % 2 == 0
#         "even"
#     else
#         "odd"
#     end
# end
#
# # Comprehension
# matrix = [i + j for i in 1:5, j in 1:5]
#
# # Exception handling
# try
#     sqrt(-1)
# catch e
#     if isa(e, DomainError)
#         println("Cannot take square root of a negative number")
#     else
#         rethrow(e)
#     end
# finally
#     println("This always executes")
# end
#
# # Parametric type
# struct Point{T<:Real}
#     x::T
#     y::T
# end
#
# # Custom operator
# import Base: +
# +(p1::Point, p2::Point) = Point(p1.x + p2.x, p1.y + p2.y)
#
# # Metaprogramming
# expr = :(x + y)
# eval(expr)
#
# # Module definition
# module MyModule
#     export hello
#
#     function hello(name)
#         println("Hello, $name!")
#     end
# end
#
# # Using newly defined module
# using .MyModule
# hello("World")
#
# # Docstring
# \"\"\"
#     greet(name::String)
#
# Print a greeting for the given name.
#
# # Arguments
# - `name::String`: The name to greet.
#
# # Examples
# ```julia-repl
# julia> greet("Alice")
# Hello, Alice!
# ```
# \"\"\"
# function greet(name::String)
#     println("Hello, $name!")
# end
#
# # Named tuple
# person = (name="John Doe", age=30, occupation="Programmer")
#
# # Pipe operator
# 1:10 |> sum |> sqrt |> round
#
# # Multiple return values
# function min_max(x, y)
#     if x < y
#         return x, y
#     else
#         return y, x
#     end
# end
#
# # Destructuring assignment
# a, b = min_max(10, 5)
#
# # String macros and interpolation
# name = "World"
# println(@sprintf("Hello, %s!", name))
#
# # Array comprehension with multiple generators
# [(i, j) for i in 1:3 for j in 1:3 if i != j]
#
# # Type annotations and where clauses
# function add(x::T, y::T) where T <: Number
#     return x + y
# end
#
# # Keyword arguments
# function greet(; name="World", greeting="Hello")
#     println("$greeting, $name!")
# end
#
# # Parametric composite types
# struct Pair{T, S}
#     first::T
#     second::S
# end
#
# # Abstract type hierarchies
# abstract type Shape end
# abstract type TwoDShape <: Shape end
# abstract type ThreeDShape <: Shape end
#
# # Mutable structs
# mutable struct Counter
#     count::Int
# end
#
# # Multiple inheritance simulation
# abstract type Drawable end
# abstract type Movable end
# struct Circle <: (TwoDShape, Drawable, Movable)
#     radius::Float64
# end
#
# # Conditional evaluation
# x = 10
# y = x > 0 ? "Positive" : "Non-positive"
#
# # Custom string literal
# macro s_str(str)
#     return uppercase(str)
# end
#
# println(s"hello")  # Prints "HELLO"
#
# # Type assertions
# function process_positive(x::Int)
#     @assert x > 0 "x must be positive"
#     return x * 2
# end
#
# # Broadcasting
# A = [1, 2, 3]
# B = [4, 5, 6]
# C = A .+ B
#
# # Anonymous functions
# double = x -> 2x
#
# # Do-while loop equivalent
# let x = 0
#     while true
#         println(x)
#         x += 1
#         x >= 5 && break
#     end
# end
#
# # Enum definition
# @enum Color red green blue
#
# # Varargs functions
# function sum_all(x...)
#     return sum(x)
# end
#
# # Matrix operations
# A = [1 2; 3 4]
# B = [5 6; 7 8]
# C = A * B
#
# # Multiple line string with triple quotes
# multiline = \"\"\"
# This is a
# multiline
# string
# \"\"\"
#
# # Command interpolation
# run(`echo $name`)
#
# # Global and local variables
# global_var = 10
# function modify_global()
#     global global_var
#     global_var += 1
# end
#
# # Big integers and rationals
# big_num = BigInt(2)^100
# ratio = 1//3
#
# # Complex numbers
# z = 1 + 2im
#
# # Custom exceptions
# struct MyCustomError <: Exception
#     msg::String
# end
#
# # Try-catch with custom exception
# try
#     throw(MyCustomError("Something went wrong"))
# catch e
#     if isa(e, MyCustomError)
#         println("Caught custom error: $(e.msg)")
#     else
#         rethrow(e)
#     end
# end
#
# # Unicode variable names
# π = 3.14159
# ∑ = sum
#
# # Multiple dispatch with type constraints
# f(x::Int) = "Integer"
# f(x::AbstractFloat) = "Float"
# f(x::Number) = "Number"
# f(x::Any) = "Any"
#
# # Closures
# function counter()
#     count = 0
#     return () -> (count += 1)
# end
#
# # Parametric methods
# same_type(x::T, y::T) where {T} = true
# same_type(x, y) = false
#
# # Tuple unpacking in function arguments
# function print_coordinates((x, y))
#     println("Coordinates: ($x, $y)")
# end
#
# # Type aliases
# const IntOrString = Union{Int, String}
#
# # Custom indexing
# struct MyContainer
#     data::Vector{Int}
# end
# Base.getindex(c::MyContainer, i::Int) = c.data[i]
# Base.setindex!(c::MyContainer, v::Int, i::Int) = (c.data[i] = v)
#
# # Metaprogramming: AST manipulation
# expr = :(x + y)
# new_expr = Expr(:call, :+, :x, :y, :z)
#
# # Custom sorting
# import Base: isless
# struct Person
#     name::String
#     age::Int
# end
# isless(a::Person, b::Person) = a.age < b.age
#
# # Parallel programming primitives
# using Distributed
# @everywhere function f(x)
#     return x^2
# end
# result = pmap(f, 1:10)
#
# # Coroutines via Channels
# function producer(c::Channel)
#     for i in 1:5
#         put!(c, i)
#     end
# end
#
# # Custom ranges
# struct EvenRange
#     start::Int
#     stop::Int
# end
# Base.iterate(r::EvenRange, state=r.start) = state > r.stop ? nothing : (state, state + 2)
#
# # Compiler directives
# @inbounds for i in eachindex(A)
#     # Code here
# end
#
# # Custom pretty-printing
# import Base: show
# struct MyType
#     x::Int
# end
# show(io::IO, m::MyType) = print(io, "MyType(", m.x, ")")
#
# # Multiple methods in a single function definition
# f(x::Int) = "Integer"
# f(x::String) = "String"
#
# # Static parameters
# struct MyArray{T, N}
#     data::Array{T, N}
# end
#
# # Generated functions
# @generated function dot(x, y)
#     if x <: Number && y <: Number
#         return :(x * y)
#     else
#         return :(sum(zip(x, y)) do (a, b)
#             a * b
#         end)
#     end
# end
# """
#     @compile_workload begin
#         for style = [DefaultStyle(), BlueStyle(), SciMLStyle(), YASStyle(), MinimalStyle()]
#           format_text(str, style)
#         end
#     end
# end
