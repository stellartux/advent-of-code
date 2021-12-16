function dot(g::SimpleWeightedGraph)
    result = "graph {"
    for edge in edges(g)
        result *= "\n    $(edge.src) -- $(edge.dst) [label=$(edge.weight)]"
    end
    result *= "\n}"
    result
end

function dot(g::SimpleWeightedDiGraph)
    result = "digraph {"
    for edge in edges(g)
        result *= "\n    $(edge.src) -> $(edge.dst) [label=$(edge.weight)]"
    end
    result *= "\n}"
    result
end

function tosvg(g)
    read(pipeline(IOBuffer(dot(g)), `dot -Tsvg`), String)
end
