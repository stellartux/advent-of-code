#!/usr/bin/julia

struct Foodstuff
    ingredients::Set{<:AbstractString}
    allergens::Set{<:AbstractString}
end

function Foodstuff(s::AbstractString)
    ingredients, allergens = split(rstrip(s, ')'), " (contains ")
    Foodstuff(Set(eachsplit(ingredients, " ")), Set(eachsplit(allergens, ", ")))
end

function puzzle(filename::AbstractString)
    food = Foodstuff.(eachline(filename))
    ingredients = union((f.ingredients for f in food)...)
    allergens = union((f.allergens for f in food)...)
    possibilities = Dict(
        a => intersect((f.ingredients for f in food if a in f.allergens)...)
        for a in allergens)
    hypoallergenic = setdiff!(ingredients, values(possibilities)...)
    println(count(ingredient in hypoallergenic for f in food for ingredient in f.ingredients))
    dangerous = Dict()
    while any(isone ∘ length, values(possibilities))
        found = [only(v) => k for (k, v) in possibilities if isone(length(v))]
        for (ingredient, allergen) in found
            dangerous[ingredient] = allergen
            delete!(possibilities, allergen)
            for p in values(possibilities)
                delete!(p, ingredient)
            end
        end
    end
    println(join(first.(sort([dangerous...], by=last)), ','))
end

if !isempty(ARGS)
    puzzle(first(ARGS))
end
