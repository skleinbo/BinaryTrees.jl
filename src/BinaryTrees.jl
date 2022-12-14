module BinaryTrees

export adjacency_matrix, BinaryTree, child!, left!, right!, sibling, isleftchild, isrightchild

import AbstractTrees
import AbstractTrees: children, childtype, descendleft, nextsibling, nodevalue, parent, ParentLinks, PreOrderDFS, prevsibling, StoredParents
import AbstractTrees: NodeType, HasNodeType, nodetype
import Base: show
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
AbstractTrees.nextsibling(t::BinaryTree) = isleftchild(t) ? children(parent(t))[2] : nothing
AbstractTrees.prevsibling(t::BinaryTree) = isrightchild(t) ? children(parent(t))[1] : nothing
AbstractTrees.nodevalue(t::BinaryTree) = t.val
AbstractTrees.parent(t::BinaryTree) = t.parent
AbstractTrees.ParentLinks(::Type{<:BinaryTree}) = StoredParents()
AbstractTrees.NodeType(::Type{<:BinaryTree}) = HasNodeType()
AbstractTrees.nodetype(::Type{BinaryTree{T}}) where T = BinaryTree{T} 
function AbstractTrees.descendleft(t::BinaryTree)
    while !isnothing(t.left)
        t = t.left
    end
    t
end

## End Interface ##

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
    sibling(t)

Return the sibing node of `t` in a binary tree.
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
    print(io, "BinaryTree{$T}($(nodevalue(t))) $(objectid(t)) with $(length(children(t))) children")
    if isnothing(parent(t))
        print(io, " and no parent.")
    else
        print(io, ".")
    end
    print(io, "\n")
    nothing
end

end # MODULE