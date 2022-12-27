if basename(pwd()) != "21"
    cd("2022/21")
end

abstract type Monkey end

struct MonkeyLeaf <: Monkey
    n::Int
end
Base.:+(m::MonkeyLeaf, n::MonkeyLeaf) = MonkeyLeaf(m.n + n.n)
Base.:-(m::MonkeyLeaf, n::MonkeyLeaf) = MonkeyLeaf(m.n - n.n)
Base.:*(m::MonkeyLeaf, n::MonkeyLeaf) = MonkeyLeaf(m.n * n.n)
Base.div(m::MonkeyLeaf, n::MonkeyLeaf) = MonkeyLeaf(m.n ÷ n.n)
Base.print(io::IO, m::MonkeyLeaf) = print(io, m.n)
Base.show(io::IO, m::MonkeyLeaf) = print(io, "MonkeyLeaf(", m.n, ")")
MonkeyLeaf(s::AbstractString) = MonkeyLeaf(parse(Int, s))
Base.convert(::Type{Int}, m::MonkeyLeaf) = m.n

struct MonkeyBranch{S<:AbstractString} <: Monkey
    op::S
    left::S
    right::S
end
Base.print(io::IO, m::MonkeyBranch) = print(io, '(', m.op, ' ', m.left, ' ', m.right, ')')

struct Monkeys
    monkeys::Dict{AbstractString,Monkey}
end

Base.getindex(ms::Monkeys, i...) = ms.monkeys[i...]

function getop(op)
    if op == "-"
        -
    elseif op == "+"
        +
    elseif op == "*"
        *
    elseif op == "/" || op == "div"
        ÷
    end
end

Monkeys(s::AbstractString) = Monkeys(split(s, '\n'))
function Monkeys(lines)
    monkeys = Dict{AbstractString,Monkey}()
    for line in lines
        m = match(r"(\w{4}): (\w{4}) ([-+*/]) (\w{4})", line)
        if !isnothing(m)
            name, left, op, right = m.captures
            monkeys[name] = MonkeyBranch(op, left, right)
        else
            name, n = match(r"(\w{4}): (\d+)", line).captures
            monkeys[name] = MonkeyLeaf(n)
        end
    end
    Monkeys(monkeys)
end

evaluate(ms, name::AbstractString="root") = evaluate(ms, ms[name])
evaluate(_, m::MonkeyLeaf) = m.n
evaluate(ms, m::MonkeyBranch) = getop(m.op)(evaluate(ms, m.left), evaluate(ms, m.right))

partone(filename::AbstractString) = evaluate(Monkeys(eachline(filename)))

struct MonkeyVariable <: Monkey end
Base.print(io::IO, ::MonkeyVariable) = print(io, '𝑥')

struct MonkeyTree{L<:Monkey,R<:Monkey} <: Monkey
    op::Function
    left::L
    right::R
end
MonkeyTree(op::Function, left::MonkeyLeaf, right::MonkeyLeaf) =
    MonkeyLeaf(op(left.n, right.n))

function Base.print(io::IO, mt::MonkeyTree)
    s = string(mt.op)
    if s == "div"
        s = "/"
    end
    print(io, '(', s, ' ', mt.left, ' ', mt.right, ')')
end

function Base.show(io::IO, mt::MonkeyTree)
    print(io, "MonkeyTree(\"", mt, "\")")
end

function MonkeyTree(ms::Monkeys, name="root")
    if name == "humn"
        return MonkeyVariable()
    end
    m = ms[name]
    if m isa MonkeyBranch
        MonkeyTree(
            name == "root" ? (==) : getop(m.op),
            MonkeyTree(ms, m.left),
            MonkeyTree(ms, m.right))
    else
        m
    end
end

function simplify(m::MonkeyTree)
    left = simplify(m.left)
    right = simplify(m.right)
    if left !== m.left || right !== m.right
        MonkeyTree(m.op, left, right)
    else
        m
    end
end

simplify(m) = m

invop(::typeof(+), ll::MonkeyLeaf, lr, r) = lr, r - ll # 1 + x = 3
invop(::typeof(+), ll, lr::MonkeyLeaf, r) = ll, r - lr # x + 1 = 3
invop(::typeof(-), ll::MonkeyLeaf, lr, r) = lr, ll - r # 1 - x = 3
invop(::typeof(-), ll, lr::MonkeyLeaf, r) = ll, r + lr # x - 1 = 3
invop(::typeof(*), ll::MonkeyLeaf, lr, r) = lr, r ÷ ll # 1 * x = 3
invop(::typeof(*), ll, lr::MonkeyLeaf, r) = ll, r ÷ lr # x * 1 = 3
invop(::typeof(div), ll::MonkeyLeaf, lr, r) = lr, r * ll # 1 ÷ x = 3
invop(::typeof(div), ll, lr::MonkeyLeaf, r) = ll, r * lr # x ÷ 1 = 3

function solve(mt::MonkeyTree)
    left, right = mt.left, mt.right
    while left isa MonkeyTree
        left, right = invop(left.op, left.left, left.right, right)
    end
    MonkeyTree(==, left, right)
end

function parttwo(filename::AbstractString)
    solve(simplify(MonkeyTree(Monkeys(eachline(filename)))))
end
