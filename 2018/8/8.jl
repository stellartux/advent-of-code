if basename(pwd()) == "aoc"
    cd("2018/8")
end

abstract type Graph end

struct NodeTree <: Graph
    children::Vector{<:Graph}
    metadata::Vector{Int}
end

function NodeTree(xs::AbstractVector{<:Integer})
    childcount = popfirst!(xs)
    datacount = popfirst!(xs)
    children = [NodeTree(xs) for _ in 1:childcount]
    metadata = [popfirst!(xs) for _ in 1:datacount]
    NodeTree(children, metadata)
end

function Base.sum(tree::NodeTree; init::Real = 0)
    init + sum(tree.metadata, init = 0) + sum(sum(child) for child in tree.children; init = 0)
end

part1(tree::NodeTree) = sum(tree)

loadnumbers(filename::AbstractString) = parse.(Int, split(readline(filename)))
part1(filename::AbstractString) = part1(NodeTree(loadnumbers(filename)))

@assert part1("example.txt") == 138
part1("input.txt")

function value(tree::NodeTree)
    sum(
        if isempty(tree.children)
            tree.metadata
        else
            sum(
                if checkbounds(Bool, tree.children, index)
                    value(tree.children[index])
                else
                    0
                end
                for index in tree.metadata;
                init = 0
            )
        end,
        init = 0
    )
end

part2(tree::NodeTree) = value(tree)
part2(filename::AbstractString) = part2(NodeTree(loadnumbers(filename)))

@assert part2("example.txt") == 66
