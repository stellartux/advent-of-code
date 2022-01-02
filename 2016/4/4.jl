if basename(pwd()) == "aoc"
    cd("2016/4")
end
using StatsBase: countmap

function part1(filename::AbstractString)
    sum(
        begin
            name, id, checksum = match(r"(.*)-(\d+)\[(.*)\]", line)
            isreal(name, checksum) ? parse(Int, id) : 0
        end
        for line in eachline(filename)
    )
end

function isreal(name::AbstractString, checksum::AbstractString)
    cm = countmap(replace(name, '-' => ""))
    join(unique!(sort!(collect(keys(cm)), lt = (a, b) -> cm[a] == cm[b] && a < b || cm[a] > cm[b]))[1:5]) == checksum
end

@assert part1("example.txt") == 1514

function part2(filename::AbstractString)
    for (name, idstr) in filter(m -> isreal(m[1], m[3]), [match(r"(.*)-(\d+)\[(.*)\]", line) for line in eachline(filename)])
        id = parse(Int, idstr)
        if replace(name, '-' => " ", r"[a-z]" => (c) -> Char((first(c) - 'a' + id) % 26 + 'a')) == "northpole object storage"
            return id
        end
    end
end
