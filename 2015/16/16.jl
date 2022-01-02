if basename(pwd()) == "aoc"
    cd("2015/16")
end

ForgottenInt{T} = Union{Int,Missing}

struct Compound
    children::ForgottenInt
    cats::ForgottenInt
    samoyeds::ForgottenInt
    pomeranians::ForgottenInt
    akitas::ForgottenInt
    vizslas::ForgottenInt
    goldfish::ForgottenInt
    trees::ForgottenInt
    cars::ForgottenInt
    perfumes::ForgottenInt
end
Compound(;
    children::ForgottenInt = missing,
    cats::ForgottenInt = missing,
    samoyeds::ForgottenInt = missing,
    pomeranians::ForgottenInt = missing,
    akitas::ForgottenInt = missing,
    vizslas::ForgottenInt = missing,
    goldfish::ForgottenInt = missing,
    trees::ForgottenInt = missing,
    cars::ForgottenInt = missing,
    perfumes::ForgottenInt = missing
) = Compound(
    children,
    cats,
    samoyeds,
    pomeranians,
    akitas,
    vizslas,
    goldfish,
    trees,
    cars,
    perfumes
)

function Compound(str::AbstractString)
    Compound(; (
        Symbol(m[1]) => parse(Int, m[2]) for m in eachmatch(r"(\S+): (\d+)", str)
    )...)
end

loadsues() = [Compound(line[findfirst(':', line):end]) for line in eachline("input.txt")]

tickertape = Compound("""
    children: 3
    cats: 7
    samoyeds: 2
    pomeranians: 3
    akitas: 0
    vizslas: 0
    goldfish: 5
    trees: 3
    cars: 2
    perfumes: 1
""")

function comparefields(a, b, fields, fn = (==))
    all(
        begin
            af = getfield(a, f)
            bf = getfield(b, f)
            ismissing(af) || !ismissing(bf) && fn(af, bf)
        end
        for f in fields
    )
end

Base.issubset(a::Compound, b::Compound) = comparefields(a, b, fieldnames(Compound))
Base.issubset(c::Compound) = Base.Fix2(issubset, c)

# part 1
findfirst(issubset(tickertape), sues)

function isweirdsubset(a::Compound, b::Compound)
    comparefields(a, b, (:children, :samoyeds, :akitas, :vizslas, :cars, :perfumes)) &&
        comparefields(a, b, (:cats, :trees), (>)) &&
        comparefields(a, b, (:pomeranians, :goldfish), (<))
end
isweirdsubset(c::Compound) = Base.Fix2(isweirdsubset, c)

# part 2
findfirst(isweirdsubset(tickertape), sues)
