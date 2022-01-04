if basename(pwd()) == "aoc"
    cd("2017/12")
end

function loadnet(filename::AbstractString)
    Dict{Int,Vector{Int}}(
        begin
            l, r = split(line, " <-> ")
            k = parse(Int, l)
            vs = parse.(Int, sort!(split(r, ", ")))
            k => vs
        end
        for line in eachline(filename)
    )
end

loadnet("example.txt")

part1(filename::AbstractString) = length(visit(loadnet(filename), 0))

function visit(net, i::Int, visited = Int[])
    sort!(push!(visited, i))
    for j in net[i]
        if !insorted(j, visited)
            visit(net, j, visited)
        end
    end
    visited
end
@assert part1("example.txt") == 6
part1("input.txt")

function dot(net)
    join((
        "graph {\n",
        ("  $k -- $v\n" for k in keys(net) for v in net[k])...,
        "}"
    ))
end

function part2(filename::AbstractString)
    net = loadnet(filename)
    result = IOBuffer()
    run(pipeline(IOBuffer(dot(net)), `dot -Tsvg`, result))
    open(replace(filename, r"\.[^.]*$" => ".svg"), "w") do f
        write(
            f,
            replace(
                String(take!(result)),
                "</svg>" => raw"""
                    <script>
                    window.addEventListener("load",  function() {
                        alert('The answer is ' + document.querySelectorAll(`ellipse[cy="${
                            Math.min(...[...document.querySelectorAll('ellipse')].map((el) => el.getAttribute('cy')))
                        }"]`).length)
                      })
                    </script>
                    </svg>
                    """
            )
        )
    end
end

part2("input.txt")
