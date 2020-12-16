import Base.in

struct Constraint
    name::String
    ranges::Vector{UnitRange{Int64}}
end
function Constraint(filestring::String)
    name, ranges = split(filestring, ':')
    a, b, c, d = parse.(Int64, match(r"(\d+)-(\d+)\D+(\d+)-(\d+)", ranges).captures)
    Constraint(name, [a:b, c:d])
end
in(value::Int64, constraint::Constraint) = any(value in range for range in constraint.ranges)

struct Ticket
    numbers::Vector{Int64}
end
Ticket(filestring::String) = Ticket(parse.(Int64, [m.match for m in eachmatch(r"\d+", filestring)]))

function loaddata(path::String)::Tuple{Vector{Constraint},Ticket,Vector{Ticket}}
    fieldsstr, myticketstr, otherticketsstr = split(strip(read(path, String)), "\n\n")
    fields = Constraint.(split(fieldsstr, '\n'))
    myticket = Ticket(string(myticketstr))
    othertickets = Ticket.(string.(split(otherticketsstr, '\n')[2:end]))
    (fields, myticket, othertickets)
end

fields, myticket, othertickets = loaddata("input.txt")

# part 1
sum(filter(number -> !any(number in field for field in fields), collect(Iterators.flatten(ticket.numbers for ticket in othertickets))))

# part 2
validtickets = filter(ticket -> all(number -> any(number in field for field in fields), ticket.numbers), othertickets)

function findconstraints(fields, myticket, validtickets)
    numbers = hcat((ticket.numbers for ticket in [myticket; validtickets])...)
    possiblefieldindices = []
    for field in fields 
        push!(possiblefieldindices, (name = field.name, numbers = Set(findall(row -> all(number -> number in field, row), [eachrow(numbers)...]))))
    end
    sort!(possiblefieldindices, by=f -> length(f.numbers), lt=(>))

    fieldindices = Dict()
    while length(possiblefieldindices) > 0
        field = pop!(possiblefieldindices)
        n = first(field.numbers)
        fieldindices[field.name] = n
        for f in possiblefieldindices
            delete!(f.numbers, n)
        end
    end

    fieldindices
end

cs = findconstraints(fields, myticket, validtickets)
println(prod(myticket.numbers[i] for i in (c[2] for c in cs if startswith(c[1], "departure"))))
