if basename(pwd()) == "aoc"
    cd("2015/15")
end

function loadingredients(filename::AbstractString)
    map(eachline(filename)) do line
        parse.(Int, (m.match for m in eachmatch(r"-?\d+", line)))
    end
end

function part1(filename::AbstractString)
    ingredients = loadingredients(filename)
    foreach(pop!, ingredients)
    maximum(score(ingredients, p) for p in eachproportion(length(ingredients)))
end

function score(ingredients, proportions)
    prod(max.(0, sum(ingredients .* proportions)))
end

function eachproportion(len, max = 100)
    if len == 1
        max
    else
        ([i, p...] for i in 0:max for p in eachproportion(len - 1, max - i))
    end
end

function part2(filename::AbstractString)
    ingredients = loadingredients(filename)
    calories = pop!.(ingredients)
    maximum(
        score(ingredients, p)
        for p in eachproportion(length(ingredients))
        if score(calories, p) == 500
    )
end
