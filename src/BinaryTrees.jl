module BinaryTrees

export adjacency_matrix, BinaryTree, child!, left!, right!, sibling, isleftchild, isrightchild, isleaf, parent!, nchildren

import AbstractTrees
import AbstractTrees: children, childtype, descendleft, nextsibling, nodevalue, parent, ParentLinks, PreOrderDFS, prevsibling, StoredParents
import AbstractTrees: NodeType, HasNodeType, nodetype
import Base: iterate, SizeUnknown, IteratorSize, IteratorEltype, HasEltype, eltype, show
import SparseArrays: spzeros

"""
    BinaryTree{T}

Stores a value of type `T` and a reference to a left/right child and parent of the same type.

Implements the [`AbstractTrees.jl`](https://github.com/JuliaCollections/AbstractTrees.jl) interface
"""
mutable struct BinaryTree{T}
    val::T
    parent::Union{Nothing, BinaryTree{T}}
    left::Union{Nothing, BinaryTree{T}}
    right::Union{Nothing, BinaryTree{T}}
    function BinaryTree{T}(v=zero(T), parent=nothing, left=nothing, right=nothing) where T
        new{T}(v, parent, left, right)
    end
end
BinaryTree(v::T) where T = BinaryTree{T}(v, nothing, nothing, nothing)

## AbstractTrees.jl interface ##
function AbstractTrees.children(t::BinaryTree) 
    if isnothing(t.left) && isnothing(t.right)
        return ()
    elseif isnothing(t.right)
        return (t.left,)
    elseif isnothing(t.left)
        return (t.right,)
    else
        return (t.left, t.right)
    end
end
AbstractTrees.childtype(::Type{BinaryTree{T}}) where T = BinaryTree{T}
AbstractTrees.childtype(::BinaryTree{T}) where T = BinaryTree{T}
function AbstractTrees.childrentype(t::BinaryTree{T}) where T
    if isnothing(t.left) && isnothing(t.right)
        return Tuple{}
    elseif isnothing(t.right) || isnothing(t.left)
        return Tuple{BinaryTree{T}}
    else
        return Tuple{BinaryTree{T}, BinaryTree{T}}
    end
end
AbstractTrees.nextsibling(t::BinaryTree) = isleftchild(t) ? children(parent(t))[2] : nothing
AbstractTrees.prevsibling(t::BinaryTree) = isrightchild(t) ? children(parent(t))[1] : nothing
AbstractTrees.nodevalue(t::BinaryTree) = t.val
AbstractTrees.parent(t::BinaryTree) = t.parent
AbstractTrees.ParentLinks(::Type{<:BinaryTree}) = StoredParents()
AbstractTrees.NodeType(::Type{<:BinaryTree}) = HasNodeType()
AbstractTrees.nodetype(::Type{BinaryTree{T}}) where T = BinaryTree{T} 
function AbstractTrees.descendleft(t::BinaryTree)
    while true
        if !isnothing(t.left)
            t = t.left
        elseif !isnothing(t.right)
            t = t.right
        else
            break
        end
    end
    t
end

## End Interface ##

## Iteration iterface   ##
## Post-order iteration ##

function Base.iterate(p::BinaryTree{T}) where T
    cursor = descendleft(p)
    return (cursor, cursor)::Tuple{BinaryTree{T}, BinaryTree{T}}
end
function Base.iterate(p::BinaryTree{T}, cursor::BinaryTree{T}) where T
    if p === cursor 
        return nothing
    end

    pa::BinaryTree = parent(cursor)
    if isrightchild(cursor)
        return (pa, pa)::Tuple{BinaryTree{T}, BinaryTree{T}}
    end
    if !isnothing(pa.right)
        cursor::BinaryTree = descendleft(pa.right)
    else
        cursor = pa
    end

    return (cursor, cursor)::Tuple{BinaryTree{T}, BinaryTree{T}}
end

Base.IteratorSize(::BinaryTree) = SizeUnknown()
Base.IteratorEltype(::BinaryTree) = Base.HasEltype()
Base.eltype(::BinaryTree{T}) where T = BinaryTree{T}

## End Iteration

child!(t::BinaryTree{T}, v::T, location::Symbol) where T = child!(t, BinaryTree(v), location)
function child!(t::BinaryTree{T}, newnode::BinaryTree{T}, location::Symbol) where T
    newnode.parent = t
    if location===:left
        t.left = newnode
    elseif location===:right
        t.right = newnode
    else
        throw(ArgumentError("location must be one of :left, :right"))
    end
    return newnode
end

"""
    left!(t, v)

Create a new BinaryTree with nodevalue `v` and set it
as the left child of `t`.

See also: [`right!`](@ref)
"""
left!(t::BinaryTree{T}, v) where T = child!(t, v, :left)

"""
    right!(t, v)

Create a new BinaryTree with nodevalue `v` and set it
as the right child of `t`.

See also: [`left!`](@ref)
"""
right!(t::BinaryTree{T}, v) where T = child!(t, v, :right)

"""
    parent!(t, p, location={:left,:right})

Set `t` as the child on the `location` of `p`. Unset `p` as child of previous parent.

See also: [`left!`, `right!`](@ref)
"""
parent!(::Nothing, v, location) = nothing
parent!(t::BinaryTree{T}, v::T, location::Symbol) where T = parent!(t, BinaryTree(v), location)
function parent!(p::BinaryTree{T}, t::BinaryTree{T}, location) where T
    if isleftchild(t)
        parent(t).left = nothing
    elseif isrightchild(t)
        parent(t).right = nothing
    end

    if location == :left
        p.left = t
    elseif location == :right
        p.right = t
    else
        error("`location` must be one of `:right`, `left`")
    end

    t.parent = p

    nothing
end

"""
    sibling(t)

Return the sibling node of `t` in a binary tree.
If `t` has no parent or no sibling, `nothing` is returned.

See also: [`nextsibling`](@ref), [`prevsibling`](@ref)
"""
function sibling(t::BinaryTree)
    p = parent(t)
    isnothing(p) && return nothing
    c = children(p)
    length(c) == 2 || return nothing
    return c[1]===t ? c[2] : c[1]
end

"Check if argument is the left child of its parent node."
isleftchild(t::BinaryTree)  = !isnothing(parent(t)) && t===parent(t).left

"Check if argument is the right child of its parent node."
isrightchild(t::BinaryTree) = !isnothing(parent(t)) && t===parent(t).right

nchildren(t::BinaryTree) = !isnothing(t.left) + !isnothing(t.right)

isleaf(t::BinaryTree) = isnothing(t.left) && isnothing(t.right)

"""
    adjacency_matrix(t)

Return the adjacency matrix of the binary tree as a sparse matrix.

Nodes are in pre-order.

Useful for constructing a graph representation from `Graphs.jl`, and for visualization.
"""
function adjacency_matrix(t::BinaryTree)
    n = 0
    for _ in PreOrderDFS(t)
        n += 1
    end

    A = spzeros(Int, n,n)
    id = IdDict(t=>1)
    i = 2
    for v in PreOrderDFS(t)
        p = parent(v)
        isnothing(p) && continue
        if !(v in keys(id))
            id[v] = i
            A[id[p], i] = 1
            i += 1
        end
    end

    return A
end

function show(io::IO, t::BinaryTree{T}) where T
    print(io, "($(nodevalue(t))) $(string(objectid(t), base=16)) with $(length(children(t))) children")
    if isnothing(parent(t))
        print(io, " and no parent.")
    else
        print(io, ".")
    end
    print(io, "\n")
    nothing
end

end # MODULE