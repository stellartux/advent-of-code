struct BingoCard
    numbers::Matrix{Int}
    called::Matrix{Bool}
end

function BingoCard(numbers::AbstractString)
    BingoCard(reshape(parse.(Int, split(numbers)), 5, 5), zeros(Bool, (5, 5)))
end

function loadgame(filename)
    nums, cards... = split(readchomp(filename), r"\r?\n\r?\n")
    nums = parse.(Int, split(nums, ","))
    cards = BingoCard.(cards)
    nums, cards
end

"""
Calls a number for the given `BingoCard`.
Returns true if the card is a winner after the call.
"""
function call(card::BingoCard, n::Integer)
    index = findfirst(==(n), card.numbers)
    if !isnothing(index)
        card.called[index] = true
        iswinner(card)
    else
        false
    end
end

function iswinner(card::BingoCard)
    any(all.(eachrow(card.called))) || any(all.(eachcol(card.called)))
end

function winningrow(card::BingoCard)
    for (i, col) in enumerate(eachcol(card.called))
        if all(col)
            return card.numbers[:, i]
        end
    end
    for (i, row) in enumerate(eachrow(card.called))
        if all(row)
            return card.numbers[i, :]
        end
    end
end

function part1(filename::AbstractString)
    nums, cards = loadgame(filename)
    for n in nums
        for card in cards
            if call(card, n)
                return n * sum(card.numbers .* .!card.called)
            end
        end
    end
end
