
function isnice(str::AbstractString)
    occursin(r"[aeiou].*[aeiou].*[aeiou]", str) &&
    occursin(r"(.)\1", str) &&
    !occursin(r"ab|cd|pq|xy", str)
end

@assert isnice("ugknbfddgicrmopn")
@assert isnice("aaa")
@assert !isnice("yzbqklnj")
@assert !isnice("haegwjzuvuyypxyu")
@assert !isnice("dvszwmarrgswjxmb")

println("Part 1: ", count(isnice, eachline(filename)))

isnice2(str::AbstractString) = occursin(r"(..).*\1", str) && occursin(r"(.).\1", str)

@assert isnice2("qjhvhtzxzqqjkmpb")
@assert isnice2("xxyxx")
@assert !isnice2("uurcxstgmygtbstg")
@assert !isnice2("ieodomkazucvgmuy")

println("Part 2: ", count(isnice2, eachline("input.txt")))
