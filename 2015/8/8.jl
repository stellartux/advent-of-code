function part1(filename::AbstractString)
    sum(length(line) - length(unescape_string(line[begin+1:end-1])) for line in eachline(filename))
end

function part2(filename::AbstractString)
    sum(length(escape_string(line)) - length(line) + 2 for line in eachline(filename))
end
