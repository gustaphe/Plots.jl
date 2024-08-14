# previously https://github.com/jw3126/UnitfulRecipes.jl
# authors: Benoit Pasquier (@briochemc) - David Gustavsson (@gustaphe) - Jan Weidner (@jw3126)

module UnitfulExt

import Plots: Plots, @ext_imp_use, @recipe, PlotText, Subplot, AVec, AMat, Axis
import RecipesBase
@ext_imp_use :import Unitful Quantity unit ustrip Unitful dimension Units NoUnits LogScaled logunit MixedUnits Level Gain uconvert DimensionError
import LaTeXStrings: LaTeXString
import Latexify: latexify
import Base: showerror
using UnitfulLatexify

const MissingOrQuantity = Union{Missing,<:Quantity,<:LogScaled}

#==========
Main recipe
==========#

@recipe function f(::Type{T}, x::T) where {T<:AbstractArray{<:MissingOrQuantity}}  # COV_EXCL_LINE
    axisletter = plotattributes[:letter]   # x, y, or z
    clims_types = (:contour, :contourf, :heatmap, :surface)
    if axisletter === :z && get(plotattributes, :seriestype, :nothing) ∈ clims_types
        fixcolorbar!(plotattributes, x)
    end
    fixaxis!(plotattributes, x, axisletter)
end

function fixaxis!(attr, x, axisletter)
    # Attribute keys
    axislabel = Symbol(axisletter, :guide) # xguide, yguide, zguide
    axislims = Symbol(axisletter, :lims)   # xlims, ylims, zlims
    axisticks = Symbol(axisletter, :ticks) # xticks, yticks, zticks
    axiserror = Symbol(axisletter, :error)       # xerror, yerror, zerror
    axisunit = Symbol(axisletter, :unit)   # xunit, yunit, zunit
    axis = Symbol(axisletter, :axis)       # xaxis, yaxis, zaxis
    u = _unit(eltype(x))
    # if the subplot already exists with data, get its unit
    sp = get(attr, :subplot, 1)
    if sp ≤ length(attr[:plot_object]) && attr[:plot_object].n > 0
        subplot = attr[:plot_object][sp]
        u = subplot[axis][:unit]
        supplied_unit = get(attr, axisunit, u)
        if u != supplied_unit
            dimension(u) == dimension(supplied_unit) || throw(DimensionError(u, supplied_unit))
            # Reinterpret existing data in new unit
            for series in subplot.series_list
                series[axisletter] = _ustrip.(supplied_unit, series[axisletter]*u)
                if series[axiserror] isa Tuple
                    series[axiserror] = map(x -> _ustrip.(supplied_unit, x*u), series[axiserror])
                elseif series[axiserror] isa AbstractArray
                    series[axiserror] = _ustrip.(supplied_unit, series[axiserror]*u)
                end
            end
            for attribute in (:lims, :ticks)
                # If these arguments are symbols, for instance :auto, they are not
                # guaranteed to visually update immediately.
                if subplot[axis][attribute] isa AbstractArray{Number}
                    subplot[axis][attribute] = _ustrip.(attr[axisunit], subplot[axis][attribute]*u)
                end
            end
        end
        label = attr[:plot_object][sp][axis][:guide]
        get!(attr, axislabel, label)  # if label was not given as an argument, reuse
    end
    u = get!(attr, axisunit, u)
    # fix the attributes: labels, lims, ticks, marker/line stuff, etc.
    append_unit_if_needed!(attr, axislabel, u)
    ustripattribute!(attr, axiserror, u)
    if axisletter === :y
        ustripattribute!(attr, :ribbon, u)
        ustripattribute!(attr, :fillrange, u)
    end
    fixaspectratio!(attr, u, axisletter)
    fixmarkercolor!(attr)
    fixmarkersize!(attr)
    fixlinecolor!(attr)
    _ustrip.(u, x)  # strip the unit
end

function fixcolorbar!(attr, x)
    # Follows, code from fixaxis!, incrementally changed
    # Attribute keys
    axislabel = :colorbar_title
    axislims = :clims
    axisticks = :colorbar_ticks
    axisunit = :cunit
    axis = :colorbar
    u = _unit(eltype(x))
    # if the subplot already exists with data, get its unit
    sp = get(attr, :subplot, 1)
    if sp ≤ length(attr[:plot_object]) && attr[:plot_object].n > 0
        subplot = attr[:plot_object][sp]
        u = subplot[:cunit]
        supplied_unit = get(attr, axisunit, u)
        if u != supplied_unit
            dimension(u) == dimension(supplied_unit) || throw(DimensionError(u, supplied_unit))
            # Reinterpret existing data in new unit
            for series in subplot.series_list
                for attribute in (:marker_z, :line_z, :fill_z)
                    if series[attribute] isa AbstractArray{Number}
                        series[attribute] = _ustrip.(supplied_unit, series[attribute]*u)
                    end
                end
            end
            for attribute in (axislims, axisticks)
                if subplot[attribute] isa AbstractArray{Number}
                    subplot[attribute] = _ustrip.(attr[axisunit], subplot[attribute]*u)
                end
            end
        end
        label = attr[:plot_object][sp][axislabel]
        get!(attr, axislabel, label)  # if label was not given as an argument, reuse
    end
    u = get!(attr, axisunit, u)
    # fix the attributes: labels, lims, ticks, marker/line stuff, etc.
    append_unit_if_needed!(attr, axislabel, u)
    _ustrip.(u, x)  # strip the unit
end

# Recipe for (x::AVec, y::AVec, z::Surface) types
@recipe function f(x::AVec, y::AVec, z::AMat{T}) where {T<:Quantity}  # COV_EXCL_LINE
    u = get(plotattributes, :zunit, _unit(eltype(z)))
    ustripattribute!(plotattributes, :clims, u)
    z = fixaxis!(plotattributes, z, :z)
    append_unit_if_needed!(plotattributes, :colorbar_title, u)
    x, y, z
end

# Recipe for vectors of vectors
@recipe function f(::Type{T}, x::T) where {T<:AVec{<:AVec{<:MissingOrQuantity}}}  # COV_EXCL_LINE
    axisletter = plotattributes[:letter]   # x, y, or z
    unitsymbol = Symbol(axisletter, :unit)
    axisunit = pop!(plotattributes, unitsymbol, _unit(eltype(first(x))))
    map(
        x -> (
            plotattributes[unitsymbol] = axisunit; fixaxis!(plotattributes, x, axisletter)
        ),
        x,
    )
end

# Recipe for bare units
@recipe function f(::Type{T}, x::T) where {T<:Units}  # COV_EXCL_LINE
    primary := false
    Float64[] * x
end

# Recipes for functions
@recipe f(f::Function, x::T) where {T<:AVec{<:MissingOrQuantity}} = x, f.(x)
@recipe f(x::T, f::Function) where {T<:AVec{<:MissingOrQuantity}} = x, f.(x)
@recipe f(x::T, y::AVec, f::Function) where {T<:AVec{<:MissingOrQuantity}} = x, y, f.(x', y)
@recipe f(x::AVec, y::T, f::Function) where {T<:AVec{<:MissingOrQuantity}} = x, y, f.(x', y)
@recipe function f(  # COV_EXCL_LINE
    x::T1,
    y::T2,
    f::Function,
) where {T1<:AVec{<:MissingOrQuantity},T2<:AVec{<:MissingOrQuantity}}
    x, y, f.(x', y)
end
@recipe function f(f::Function, u::Units)  # COV_EXCL_LINE
    uf = UnitFunction(f, [u])
    recipedata = RecipesBase.apply_recipe(plotattributes, uf)
    _, xmin, xmax = recipedata[1].args
    f, xmin * u, xmax * u
end

"""
```julia
UnitFunction
```
A function, bundled with the assumed units of each of its inputs.

```julia
f(x, y) = x^2 + y
uf = UnitFunction(f, u"m", u"m^2")
uf(3, 2) == f(3u"m", 2u"m"^2) == 7u"m^2"
```
"""
struct UnitFunction <: Function
    f::Function
    u::Vector{Units}
end
(f::UnitFunction)(args...) = f.f((args .* f.u)...)

#===============
Attribute fixing
===============#
# Aspect ratio
function fixaspectratio!(attr, u, axisletter)
    aspect_ratio = get(attr, :aspect_ratio, :auto)
    if aspect_ratio in (:auto, :none)
        # Keep the default behavior (let Plots figure it out)
        return
    end
    if aspect_ratio === :equal
        aspect_ratio = 1
    end
    #=======================================================================================
    Implementation example:

    Consider an x axis in `u"m"` and a y axis in `u"s"`, and an `aspect_ratio` in `u"m/s"`.
    On the first pass, `axisletter` is `:x`, so `aspect_ratio` is converted to `u"m/s"/u"m"
    = u"s^-1"`. On the second pass, `axisletter` is `:y`, so `aspect_ratio` becomes
    `u"s^-1"*u"s" = 1`. If at this point `aspect_ratio` is *not* unitless, an error has been
    made, and the default aspect ratio fixing of Plots throws a `DimensionError` as it tries
    to compare `0 < 1u"m"`.
    =======================================================================================#
    if axisletter === :y
        attr[:aspect_ratio] = aspect_ratio * u
    elseif axisletter === :x
        attr[:aspect_ratio] = aspect_ratio / u
    end
    nothing
end

# Markers / lines
function fixmarkercolor!(attr)
    u = ustripattribute!(attr, :marker_z)
    ustripattribute!(attr, :clims, u)
    u == NoUnits || append_unit_if_needed!(attr, :colorbar_title, u)
end
fixmarkersize!(attr) = ustripattribute!(attr, :markersize)
fixlinecolor!(attr) = ustripattribute!(attr, :line_z)

# strip unit from attribute[key]
ustripattribute!(attr, key) =
    if haskey(attr, key)
        v = attr[key]
        u = _unit(eltype(v))
        attr[key] = _ustrip.(u, v)
        u
    else
        NoUnits
    end

# if supplied, use the unit (optional 3rd argument)
function ustripattribute!(attr, key, u)
    if haskey(attr, key)
        v = attr[key]
        if eltype(v) <: Quantity
            attr[key] = _ustrip.(u, v)
        end
    end
    u
end

#=======================================
Label string containing unit information
=======================================#

abstract type AbstractProtectedString <: AbstractString end
struct ProtectedString{S} <: AbstractProtectedString
    content::S
end
struct UnitfulString{S,U,F} <: AbstractProtectedString
    label::S
    unit::U
    format::F
end
# Minimum required AbstractString interface to work with Plots
const S = AbstractProtectedString
content(n::ProtectedString) = n.content
content(n::UnitfulString) = strip(format_unit_label(n.label, n.unit, n.format))
Base.iterate(n::S) = iterate(content(n))
Base.iterate(n::S, i::Integer) = iterate(content(n), i)
Base.codeunit(n::S) = codeunit(content(n))
Base.ncodeunits(n::S) = ncodeunits(content(n))
Base.isvalid(n::S, i::Integer) = isvalid(content(n), i)
Base.pointer(n::S) = pointer(content(n))
Base.pointer(n::S, i::Integer) = pointer(content(n), i)

Plots.protectedstring(s) = ProtectedString(s)

#=====================================
Append unit to labels when appropriate
=====================================#

append_unit_if_needed!(attr, key, u) =
    append_unit_if_needed!(attr, key, get(attr, key, nothing), u)
# dispatch on the type of `label`
append_unit_if_needed!(attr, key, label::ProtectedString, u) = nothing
function append_unit_if_needed!(attr, key, label::UnitfulString, u)
    if u != label.unit
        attr[key] = UnitfulString(label.label, u, label.format)
    end
    nothing
end
function append_unit_if_needed!(attr, key, label::Nothing, u)
    if attr[:plot_object].backend isa Plots.PGFPlotsXBackend
        return attr[key] = UnitfulString("", u, (l, u) -> latexify(u))
    end
    attr[key] = UnitfulString("", u, nothing)
end
function append_unit_if_needed!(attr, key, label::S, u) where {S<:AbstractString}
    isempty(label) && return attr[key] = UnitfulString(label, u, nothing)
    attr[key] = UnitfulString(label, u, get(attr, Symbol(get(attr, :letter, ""), :unitformat), :round))
end

#=============================================
Surround unit string with specified delimiters
=============================================#

const UNIT_FORMATS = Dict(
    :round => ('(', ')'),
    :square => ('[', ']'),
    :curly => ('{', '}'),
    :angle => ('<', '>'),
    :slash => '/',
    :slashround => (" / (", ")"),
    :slashsquare => (" / [", "]"),
    :slashcurly => (" / {", "}"),
    :slashangle => (" / <", ">"),
    :verbose => " in units of ",
    :none => nothing,
)

format_unit_label(l, u, f::Nothing)                    = string(l, ' ', u)
format_unit_label(l, u, f::Function)                   = f(l, u)
format_unit_label(l, u, f::AbstractString)             = string(l, f, u)
format_unit_label(l, u, f::NTuple{2,<:AbstractString}) = string(l, f[1], u, f[2])
format_unit_label(l, u, f::NTuple{3,<:AbstractString}) = string(f[1], l, f[2], u, f[3])
format_unit_label(l, u, f::Char)                       = string(l, ' ', f, ' ', u)
format_unit_label(l, u, f::NTuple{2,Char})             = string(l, ' ', f[1], u, f[2])
format_unit_label(l, u, f::NTuple{3,Char})             = string(f[1], l, ' ', f[2], u, f[3])
format_unit_label(l, u, f::Bool)                       = f ? format_unit_label(l, u, :round) : format_unit_label(l, u, nothing)
format_unit_label(l, u, f::Symbol)                     = format_unit_label(l, u, UNIT_FORMATS[f])

#==============
Fix annotations
===============#
function Plots.locate_annotation(
        sp::Subplot,
        x::MissingOrQuantity,
        y::MissingOrQuantity,
        label::PlotText,
)
    xunit = sp.attr[:xaxis][:unit]
    yunit = sp.attr[:yaxis][:unit]
    (_ustrip(xunit, x), _ustrip(yunit, y), label)
end
function Plots.locate_annotation(
        sp::Subplot,
        x::MissingOrQuantity,
        y::MissingOrQuantity,
        z::MissingOrQuantity,
        label::PlotText,
)
    xunit = sp.attr[:xaxis][:unit]
    yunit = sp.attr[:yaxis][:unit]
    zunit = sp.attr[:zaxis][:unit]
    (_ustrip(xunit, x), _ustrip(yunit, y), _ustrip(zunit, z), label)
end
function Plots.locate_annotation(
        sp::Subplot,
        rel::NTuple{N,<:MissingOrQuantity},
        label,
) where {N}
    units = (
             sp.attr[:xaxis][:unit],
             sp.attr[:yaxis][:unit],
             sp.attr[:zaxis][:unit]
            )
    Plots.locate_annotation(sp, _ustrip.(zip(units, rel)), label)
end

#==================#
# ticks and limits #
#==================#
Plots._transform_ticks(ticks::AbstractArray{T}, axis) where {T<:Quantity} = _ustrip.(axis[:unit], ticks)
Plots.process_limits(lims::AbstractArray{T}, axis) where {T<:Quantity} = _ustrip.(axis[:unit], lims)
Plots.process_limits(lims::Tuple{S,T}, axis) where {S<:Quantity,T<:Quantity} = _ustrip.(axis[:unit], lims)

_ustrip(u::MixedUnits, x) = ustrip(uconvert(u, x))
_ustrip(u, x) = ustrip(u, x)

function _unit(x)
    (T = eltype(x)) <: LogScaled && return logunit(T)
    unit(x)
end

function Plots.pgfx_sanitize_string(s::UnitfulString)
    UnitfulString(Plots.pgfx_sanitize_string(s.label), s.unit, s.format)
end

end  # module
