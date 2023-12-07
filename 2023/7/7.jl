#!/usr/bin/env julia
# usage: julia 7.jl [FILENAME]

module AOC2023Day7

load(file) =
    [(left, parse(Int, right)) for (left, right) = split.(eachline(file))]

@enum CamelHand FiveOfAKind FourOfAKind FullHouse ThreeOfAKind TwoPair OnePair HighCard

function camelhand(hand; jokers=false)
    counts = Dict{Char,Int}()
    for card in hand
        counts[card] = get(counts, card, 0) + 1
    end
    jcount = jokers ? pop!(counts, 'J', 0) : 0
    countlist = sort!(collect(values(counts)); rev=true)
    if isempty(countlist)
        return FiveOfAKind
    end
    countlist[1] += jcount
    if countlist[1] == 5
        FiveOfAKind
    elseif countlist[1] == 4
        FourOfAKind
    elseif countlist == [3, 2]
        FullHouse
    elseif countlist[1] == 3
        ThreeOfAKind
    elseif countlist == [2, 2, 1]
        TwoPair
    elseif countlist[1] == 2
        OnePair
    else
        HighCard
    end
end

scorecard(card; jokers=false) =
    findfirst(card, jokers ? "AKQT98765432J" : "AKQJT98765432")

function strength(left, right; jokers=false)
    leftcamel = camelhand(left[1]; jokers)
    rightcamel = camelhand(right[1]; jokers)
    if leftcamel == rightcamel
        for (leftcard, rightcard) in zip(left[1], right[1])
            if leftcard != rightcard
                leftscore = scorecard(leftcard; jokers)
                rightscore = scorecard(rightcard; jokers)
                if leftscore != rightscore
                    return leftscore > rightscore
                end
            end
        end
    else
        leftcamel > rightcamel
    end
end

strength(; jokers=false) =
    (left, right) -> strength(left, right; jokers)

scorehands(hands; jokers=false) =
    sum(key * score for (key, (_, score)) in pairs(sort(hands, lt=strength(; jokers))))

if abspath(PROGRAM_FILE) == @__FILE__
    hands = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(scorehands(hands))
    println(scorehands(hands; jokers=true))
end

end # module
