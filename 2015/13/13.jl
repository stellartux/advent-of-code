function solver(filename::AbstractString; parttwo=false)
    pairings = Dict()
    names = Set()
    for (left, _, gainlose, points, _, _, _, _, _, _, right) in split.(eachline(filename))
        change = parse(Int, points)
        if gainlose == "lose"
            change = -change
        end
        right = right[1:end-1]
        if left > right
            left, right = right, left
        end
        push!(names, left, right)
        pairings[(left, right)] = get(pairings, (left, right), 0) + change
    end
    if parttwo
        push!(pairings, ((name, "Me") => 0 for name in names)...)
    end
    pairlist = sort!(collect(pairings), by=last, rev=true)
    with(name::AbstractString) = (pair) -> name in first(pair)
    maximum(begin
        options = copy(pairlist)
        total = 0
        n = name
        while !isempty(options)
            index = findfirst(with(n), options)
            (a, b), score = options[index]
            total += score
            filter!(!with(n), options)
            n = a == n ? b : a
        end
        total += pairings[(n, name) in keys(pairings) ? (n, name) : (name, n)]
        total
    end for name in names)
end
