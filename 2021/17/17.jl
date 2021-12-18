if basename(pwd()) == "aoc"
    cd("2021/17")
end

# target area: x=20..30, y=-10..-5
# target area: x=79..137, y=-176..-117

exampletarget = CartesianIndices((-10:-5, 20:30))
inputtarget = CartesianIndices((-176:-117, 79:137))

mutable struct Probe
    xpos::Int
    ypos::Int
    xvel::Int
    yvel::Int
end
Probe(xvel, yvel) = Probe(0, 0, xvel, yvel)

position(p::Probe) = CartesianIndex(p.ypos, p.xpos)

function step!(p::Probe)
    p.xpos += p.xvel
    p.ypos += p.yvel
    p.xvel -= sign(p.xvel)
    p.yvel -= 1
end

Base.in(p::Probe, cs::CartesianIndices) = position(p) in cs

triangle(n) = n * (n - 1) ÷ 2

# If the x distance is a triangular number, the x velocity can be that triangular
# number and any step >= x will have xpos == x, else the x distance is the
# difference between two triangular numbers, and the x will eventually overshoot.
# This bounds the number of steps that can be taken, and hence the value of y.

# y is also the difference between two triangular numbers
# yₙ = -(triangle(n - 2yᵤ) - triangle(n))
# or something like that

# FUCK IT BRUTE FORCE GOES BRRRRRRRRRRRRRRRRRR
function launchprobe(target, xvel, yvel)
    probe = Probe(xvel, yvel)
    highest = probe.ypos
    miny = minimum(target)[1]
    while probe.ypos >= miny
        step!(probe)
        highest = max(highest, probe.ypos)
        if probe in target
            return highest
        end
    end
    nothing
end

function part1(target)
    maximum(
        Iterators.filter(
            !isnothing,
            launchprobe(target, x, y) for x = 1:138 for y = 1:177
        )
    )
end

function part2(target)
    count(!isnothing, launchprobe(target, x, y) for x = 1:140 for y = -180:180)
end
