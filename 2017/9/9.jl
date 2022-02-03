if basename(pwd()) == "aoc"
    cd("2017/9")
end

function score(stream::AbstractString)
    iter = Iterators.Stateful(stream)
    level = 0
    total = 0
    garbagemode = false
    garbagetotal = 0
    for c in iter
        if garbagemode
            if c == '!'
                popfirst!(iter)
            elseif c == '>'
                garbagemode = false
            else
                garbagetotal += 1
            end
        else
            if c == '<'
                garbagemode = true
            elseif c == '{'
                level += 1
                total += level
            elseif c == '}'
                level -= 1
            end
        end
    end
    total, garbagetotal
end

@assert score("{}") == (1, 0)
@assert score("{{{}}}") == (6, 0)
@assert score("{{},{}}") == (5, 0)
@assert score("{{{},{},{{}}}}") == (16, 0)
@assert score("{<{},{},{{}}>}") == (1, 10)
@assert score("{<a>,<a>,<a>,<a>}") == (1, 4)
@assert score("{{<a>},{<a>},{<a>},{<a>}}") == (9, 4)
@assert score("{{<!!>},{<!!>},{<!!>},{<!!>}}") == (9, 0)
@assert score("{{<a!>},{<a!>},{<a!>},{<ab>}}") == (3, 17)

score(readline("input.txt"))
