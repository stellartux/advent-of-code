#!/usr/bin/env julia

println(sum(
    parse(Int, l[[findfirst(isdigit, l), findlast(isdigit, l)]])
    for l = eachline(get(ARGS, 1, "input.txt"))))

let d = Dict(
        "zero" => "0",
        "one" => "1",
        "two" => "2",
        "three" => "3",
        "four" => "4",
        "five" => "5",
        "six" => "6",
        "seven" => "7",
        "eight" => "8",
        "nine" => "9")

    println(sum(begin
        m = match(r"^.*?([0-9]|zero|one|two|three|four|five|six|seven|eight|nine)", l)
        n = match(r"^.*([0-9]|zero|one|two|three|four|five|six|seven|eight|nine)", l)
        parse(Int, get(d, m[1], m[1]) * get(d, n[1], n[1]))
    end for l = eachline(get(ARGS, 1, "input.txt"))))
end
