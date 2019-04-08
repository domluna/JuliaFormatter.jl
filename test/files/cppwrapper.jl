

struct CPolygon
  vertexlist::Ptr{Cint}
  numberofvertices::Cint
end

struct CFacet{T}
  polygonlist::Ptr{CPolygon}
  numberofpolygons::Cint
  holelist::Ptr{T}
  numberofholes::Cint
end

# function Base.cconvert(::Type{Vector{CFacet{T}}}, triangles::Vector{NGonFace{N, Cint}}) where {N, T}
#     polygons = map(1:length(triangles)) do i
#         CPolygon(Ptr{Cint}(pointer(triangles, i)), Cint(N))
#     end
#     facets = map(1:length(polygons)) do i
#         CPolygon(Ptr{CPolygon}(pointer(polygons, i)), Cint(1), Ptr{T}(C_NULL), Cint(0))
#     end
#     return facets
# end

struct JLFacet{T, A <: AbstractVector{Cint}}
    polygons::Vector{A}
    holelist::Vector{T}
end
JLFacet(x::AbstractVector{Cint}) = JLFacet([x])
JLFacet(x::Vector{<: AbstractVector{Cint}}) = JLFacet{Float64, eltype(typeof(x))}(x, Float64[])


function JLPolygon(x::CPolygon)
    JLPolygon(unsafe_wrap(Array, x.vertexlist, x.numberofvertices, own = true))
end

function JLFacet(f::CFacet{T}) where T
    c_polys = unsafe_wrap(Array, f.polygonlist, f.numberofpolygons, own = true)
    holes = unsafe_wrap(Array, f.holelist, f.numberofholes, own = true)
    JLFacet{T}(JLPolygon.(c_polys), holes)
end

function convert_poly(x::Vector{T}) where T <: Union{NgonFace{N, Cint}, NTuple{N, Cint}} where N
    return map(1:length(x)) do i
        CPolygon(pointer(x, i), N)
    end
end

function convert_poly(x::Vector{Vector{Cint}})
    return map(1:length(x)) do i
        CPolygon(Base.unsafe_convert(Ptr{Cint}, x[i]), length(x[i]))
    end
end


struct Region{T}
    pos::Point{3, T}
    attribute::T
    maxvol::T
end

struct FacetConstraint{IT, T}
    facet_marker::IT
    max_area_bound::T
end
struct SegmentationConstraint{IT, T}
    index::IT # index into pointlist
    max_length_bound::T
end


struct CPPTetgenIO{T}

    firstnumber::Cint # 0 or 1, default 0.
    mesh_dim::Cint # must be 3.

    pointlist::Ptr{T}
    pointattributelist::Ptr{T}
    pointmtrlist::Ptr{T}

    pointmarkerlist::Ptr{Cint}
    numberofpoints::Cint
    numberofpointattributes::Cint
    numberofpointmtrs::Cint

    tetrahedronlist::Ptr{Cint}
    tetrahedronattributelist::Ptr{T}
    tetrahedronvolumelist::Ptr{T}
    neighborlist::Ptr{Cint}
    numberoftetrahedra::Cint
    numberofcorners::Cint
    numberoftetrahedronattributes::Cint

    facetlist::Ptr{CFacet{T}}
    facetmarkerlist::Ptr{Cint}
    numberoffacets::Cint

    holelist::Ptr{T}
    numberofholes::Cint

    regionlist::Ptr{T}
    numberofregions::Cint

    facetconstraintlist::Ptr{T}
    numberoffacetconstraints::Cint

    segmentconstraintlist::Ptr{T}
    numberofsegmentconstraints::Cint

    trifacelist::Ptr{Cint}
    trifacemarkerlist::Ptr{Cint}
    numberoftrifaces::Cint

    edgelist::Ptr{Cint}
    edgemarkerlist::Ptr{Cint}
    numberofedges::Cint
end



inttype(::Type{Float64}) = Int64
inttype(::Type{Float32}) = Int32

struct TetgenIO{T, NSimplex, NAttributes, NMTr, IT, FT}
    points::Vector{Point{3, T}}
    pointattributes::Vector{SVector{NAttributes, T}}
    pointmtrs::Vector{SVector{NMTr, T}}
    pointmarkers::Vector{Cint}

    tetrahedra::Vector{SimplexFace{NSimplex, Cint}}
    tetrahedronattributes::Vector{T}
    tetrahedronvolumes::Vector{T}
    neighbors::Vector{Cint}

    facets::FT
    facetmarkers::Vector{Cint}

    holes::Vector{Point{3, T}}

    regions::Vector{Region{T}}

    facetconstraints::Vector{FacetConstraint{IT, T}}

    segmentconstraints::Vector{SegmentationConstraint{IT, T}}

    trifaces::Vector{TriangleFace{Cint}}
    trifacemarkers::Vector{Cint}

    edges::Vector{LineFace{Cint}}
    edgemarkers::Vector{Cint}

    function TetgenIO(
            points::Vector{Point{3, T}},
            pointattributes::Vector{SVector{NAttributes, T}},
            pointmtrs::Vector{SVector{NMTr, T}},
            pointmarkers::Vector{Cint},
            tetrahedrons::Vector{SimplexFace{NSimplex, Cint}},
            tetrahedronattributes::Vector{T},
            tetrahedronvolumes::Vector{T},
            neighbors::Vector{Cint},
            facets::FT,
            facetmarkers::Vector{Cint},
            holes::Vector{Point{3, T}},
            regions::Vector{Region{T}},
            facetconstraints::Vector{FacetConstraint{IT, T}},
            segmentconstraints::Vector{SegmentationConstraint{IT, T}},
            trifaces::Vector{TriangleFace{Cint}},
            trifacemarkers::Vector{Cint},
            edges::Vector{LineFace{Cint}},
            edgemarkers::Vector{Cint},
        ) where {T, NSimplex, NAttributes, NMTr, IT, FT}


        new{T, NSimplex, NAttributes, NMTr, IT, FT}(
            points,
            pointattributes,
            pointmtrs,
            pointmarkers,
            tetrahedrons,
            tetrahedronattributes,
            tetrahedronvolumes,
            neighbors,
            facets,
            facetmarkers,
            holes,
            regions,
            facetconstraints,
            segmentconstraints,
            trifaces,
            trifacemarkers,
            edges,
            edgemarkers,
        )
    end

end

function TetgenIO(
        points::Vector{Point{3, T}};
        pointattributes = SVector{0, T}[],
        pointmtrs = SVector{0, T}[],
        pointmarkers = Cint[],
        tetrahedrons = SimplexFace{4, Cint}[],
        tetrahedronattributes = T[],
        tetrahedronvolumes = T[],
        neighbors = Cint[],
        facets = JLFacet{T, Vector{Cint}}[],
        facetmarkers = Cint[],
        holes = Point{3, T}[],
        regions = Region{T}[],
        facetconstraints = FacetConstraint{inttype(T), T}[],
        segmentconstraints = SegmentationConstraint{inttype(T), T}[],
        trifaces = TriangleFace{Cint}[],
        trifacemarkers = Cint[],
        edges = LineFace{Cint}[],
        edgemarkers = Cint[]

    ) where T
    TetgenIO(
        points,
        pointattributes,
        pointmtrs,
        pointmarkers,
        tetrahedrons,
        tetrahedronattributes,
        tetrahedronvolumes,
        neighbors,
        facets,
        facetmarkers,
        holes,
        regions,
        facetconstraints,
        segmentconstraints,
        trifaces,
        trifacemarkers,
        edges,
        edgemarkers,
    )

end


function number_of_elements_field(io, basename)
    nfield = if basename == "tetrahedra"
        Symbol("numberof" * basename)
    else
        Symbol("numberof" * basename * "s")
    end
    n = if nfield in fieldnames(typeof(io))
        return nfield
    elseif startswith(basename, "point")
        return :numberofpoints
    elseif startswith(basename, "facet")
        return :numberoffacets
    elseif startswith(basename, "triface")
        return :numberoftrifaces
    elseif startswith(basename, "edge")
        return :numberofedges
    else
        error("No numberof for $nfield")
    end
end

function get_array(io, field::Symbol, Typ)
    basename = if field == :tetrahedra
        string(field)
    else
        string(field)[1:end-1] # cut of s
    end
    ptr = if field == :tetrahedra
        io.tetrahedronlist
    else
        getfield(io, Symbol(basename * "list"))
    end
    ptr == C_NULL && return Typ[]
    n = getfield(io, number_of_elements_field(io, basename))
    # own true, since we disable freeing in the c-part
    return unsafe_wrap(Array, Base.unsafe_convert(Ptr{Typ}, ptr), n, own = true)
end

function Base.convert(
        IOT::Type{TetgenIO{T, NSimplex, NAttributes, NMTr, IT, A}}, io::CPPTetgenIO{T}
    ) where {T, NSimplex, NAttributes, NMTr, IT, A}
    TetgenIO(ntuple(fieldcount(IOT)) do i
        fname, ftype = fieldname(IOT, i), fieldtype(IOT, i)
        get_array(io, fname, eltype(ftype))
    end...)
end

function Base.convert(::Type{TetgenIO}, io::CPPTetgenIO{T}) where T
    NSimplex = Int(io.numberofcorners)
    NAttributes = Int(io.numberofpointattributes)
    NMTr = Int(io.numberofpointmtrs)
    IT = inttype(T) # TODO these are indices,
    convert(TetgenIO{T, NSimplex, NAttributes, NMTr, IT, Vector{CFacet{T}}}, io)
end


function Base.cconvert(::Type{Ptr{CFacet{T}}}, facets::Vector{JLFacet{T, A}}) where {T, A}
    c_polys = map(facets) do facet
        convert_poly(facet.polygons)
    end
    c_facets = map(c_polys) do cpoly
        ptr = Base.unsafe_convert(Ptr{CPolygon}, cpoly)
        CFacet{T}(ptr, length(cpoly), C_NULL, 0)
    end
    return (facets, c_polys, c_facets)
end

function Base.cconvert(::Type{Ptr{CFacet{T}}}, facets::Vector{NgonFace{N, Cint}}) where {N, T}
    nfacets = length(facets)
    c_polys = map(1:nfacets) do i
        CPolygon(Ptr{Cint}(pointer(facets, i)), N)
    end
    c_facets = map(1:nfacets) do i
        ptr = pointer(c_polys, i)
        CFacet{T}(ptr, 1, C_NULL, 0)
    end
    return (facets, c_polys, c_facets)
end


function unsafe_array_convert(
        ::Type{Ptr{CFacet{T}}},
        facets::Tuple{Vector{F}, Vector{Vector{CPolygon}}, Vector{CFacet{T}}}
    ) where {F, T}
    return Base.unsafe_convert(Ptr{CFacet{T}}, facets[3])
end

function unsafe_array_convert(
        ::Type{Ptr{CFacet{T}}},
        facets::Tuple{Vector{F}, Vector{CPolygon}, Vector{CFacet{T}}}
    ) where {F, T}
    return Base.unsafe_convert(Ptr{CFacet{T}}, facets[3])
end

function unsafe_array_convert(P::Type{Ptr{T}}, x::Vector{T}) where T
    Base.unsafe_convert(P, x)
end

function unsafe_array_convert(P::Type{Ptr{T1}}, x::Vector{T2}) where {T1, T2}
    Ptr{T1}(Base.unsafe_convert(Ptr{T2}, x))
end

function Base.cconvert(CIO::Type{CPPTetgenIO{T}}, obj::TetgenIO{T, NSimplex, NAttributes, NMTr, IT, A}) where {T, NSimplex, NAttributes, NMTr, IT, A}
    cfnames = fieldnames(CIO)
    dict = Dict{Symbol, Any}(
        :numberofpointattributes => NAttributes,
        :numberofpointmtrs => NMTr,
        :numberofcorners => NSimplex,
        :firstnumber => Cint(1),
        :mesh_dim => Cint(3),
    )
    gc_tack_cconvert = []
    for field in fieldnames(typeof(obj))
        basename = if field == :tetrahedra
            string(field)
        else
            string(field)[1:end-1] # cut of s
        end
        listname = Symbol(replace(basename * "list", "tetrahedra" => "tetrahedron"))
        FT = fieldtype(CIO, listname)
        array = getfield(obj, field)
        converted = Base.cconvert(FT, array)
        push!(gc_tack_cconvert, converted)
        if isempty(array)
            # make sure we forward NULL for empty
            dict[listname] = Base.unsafe_convert(FT, C_NULL)
        else
            dict[listname] = unsafe_array_convert(FT, converted)
        end
        nfield = if basename == "tetrahedra"
            Symbol("numberof" * basename)
        else
            Symbol("numberof" * basename * "s")
        end
        if nfield in cfnames
            dict[nfield] = length(array)
        end
    end
    fields_tuple = getindex.((dict,), cfnames)
    return (CPPTetgenIO{T}(fields_tuple...), gc_tack_cconvert)
end

function Base.unsafe_convert(::Type{CPPTetgenIO{T}}, x::Tuple{CPPTetgenIO{T}, Vector{Any}}) where T
    return x[1]
end


function tetrahedralize(io::TetgenIO{Float64}, command::String)
    cres = ccall((:tetrahedralizef64, libtet), CPPTetgenIO{Float64}, (CPPTetgenIO{Float64}, Cstring), io, command)
    return convert(TetgenIO, cres)
end

function tetrahedralize(io::TetgenIO{Float32}, command::String)
    cres = ccall((:tetrahedralizef32, libtet), CPPTetgenIO{Float32}, (CPPTetgenIO{Float32}, Cstring), io, command)
    return convert(TetgenIO, cres)
end
