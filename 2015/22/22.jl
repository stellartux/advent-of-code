#!/usr/bin/env julia
# usage: julia 22.jl [FILENAME]

module AOC2015Day22

# priority queue

struct EmptyNode end
struct Node{P,D}
    priority::P
    data::D
    left::Union{EmptyNode,Node}
    right::Union{EmptyNode,Node}
end
Node(p::P, d::D, l::Union{EmptyNode,Node}=EmptyNode()) where {P,D} =
    Node{P,D}(p, d, l, EmptyNode())

Base.isempty(::EmptyNode) = true
Base.isempty(::Node) = false

unite(::EmptyNode, n) = n
unite(n::Node, ::EmptyNode) = n
function unite(n1::Node, n2::Node)
    if n1.priority <= n2.priority
        Node(n1.priority, n1.data, unite(n2, n1.right), n1.left)
    else
        Node(n2.priority, n2.data, unite(n1, n2.right), n2.left)
    end
end

insert(::EmptyNode, priority, data) = Node(priority, data)
insert(heap::Node{P,D}, priority::P, data::D) where {P,D} =
    unite(heap, Node(priority, data))

extractmin(::EmptyNode) = nothing
extractmin(n::Node) = (n.priority, n.data, unite(n.left, n.right))

# main logic

struct Battle
    playerhealth::Int
    playermana::Int
    bosshealth::Int
    bossdamage::Int
    shieldtimer::Int
    rechargetimer::Int
    poisontimer::Int
end

playerwins(b::Battle)::Bool = iszero(b.bosshealth)
playerloses(b::Battle)::Bool = iszero(b.playerhealth) || b.playermana < 53

function applyeffects(b::Battle)::Battle
    b = applypoison(b)
    if b.rechargetimer > 0
        Battle(
            b.playerhealth,
            b.playermana + 101,
            b.bosshealth,
            b.bossdamage,
            b.shieldtimer - Int(b.shieldtimer > 0),
            b.rechargetimer - 1,
            b.poisontimer
        )
    elseif b.shieldtimer > 0
        Battle(
            b.playerhealth,
            b.playermana,
            b.bosshealth,
            b.bossdamage,
            b.shieldtimer - 1,
            b.rechargetimer,
            b.poisontimer
        )
    else
        b
    end
end

function applypoison(b::Battle)::Battle
    if b.poisontimer > 0 && b.bosshealth > 0
        Battle(
            b.playerhealth,
            b.playermana,
            damageboss(b, 3),
            b.bossdamage,
            b.shieldtimer,
            b.rechargetimer,
            b.poisontimer - 1
        )
    else
        b
    end
end

function bossturn(b::Battle)::Battle
    b = applyeffects(b)
    if b.bosshealth > 0
        Battle(
            damageplayer(b),
            b.playermana,
            b.bosshealth,
            b.bossdamage,
            b.shieldtimer,
            b.rechargetimer,
            b.poisontimer
        )
    else
        b
    end
end

damageboss(b::Battle, damage::Int)::Int = max(0, b.bosshealth - damage)
damageplayer(b::Battle)::Int =
    max(0, b.playerhealth - (b.shieldtimer > 0 ? max(1, b.bossdamage - 7) : b.bossdamage))

canmagicmissile(b::Battle)::Bool = b.playermana >= 53
magicmissile(b::Battle)::Tuple{Int,Battle} =
    53, Battle(
        b.playerhealth,
        b.playermana - 53,
        damageboss(b, 4),
        b.bossdamage,
        b.shieldtimer,
        b.rechargetimer,
        b.poisontimer
    )

candrain(b::Battle)::Bool = b.playermana >= 73
drain(b::Battle)::Tuple{Int,Battle} =
    73, Battle(
        b.playerhealth + 2,
        b.playermana - 73,
        damageboss(b, 2),
        b.bossdamage,
        b.shieldtimer,
        b.rechargetimer,
        b.poisontimer
    )

canshield(b::Battle)::Bool = iszero(b.shieldtimer) && b.playermana >= 113
shield(b::Battle)::Tuple{Int,Battle} =
    113, Battle(
        b.playerhealth,
        b.playermana - 113,
        b.bosshealth,
        b.bossdamage,
        6,
        b.rechargetimer,
        b.poisontimer
    )

canpoison(b::Battle)::Bool = iszero(b.poisontimer) && b.playermana >= 173
poison(b::Battle)::Tuple{Int,Battle} =
    173, Battle(
        b.playerhealth,
        b.playermana - 173,
        b.bosshealth,
        b.bossdamage,
        b.shieldtimer,
        b.rechargetimer,
        6
    )

canrecharge(b::Battle)::Bool = iszero(b.rechargetimer) && b.playermana >= 229
recharge(b::Battle)::Tuple{Int,Battle} =
    229, Battle(
        b.playerhealth,
        b.playermana - 229,
        b.bosshealth,
        b.bossdamage,
        b.shieldtimer,
        5,
        b.poisontimer
    )

function play(playerhealth::Int, playermana::Int, bosshealth::Int, bossdamage::Int; difficulty=:easy)
    queue = Node(0, Battle(playerhealth, playermana, bosshealth, bossdamage, 0, 0, 0))
    while !isempty(queue)
        manaspent, battle, queue = extractmin(queue)
        if playerwins(battle)
            return manaspent
        end
        if difficulty == :hard
            battle = Battle(
                max(0, battle.playerhealth - 1),
                battle.playermana,
                battle.bosshealth,
                battle.bossdamage,
                battle.shieldtimer,
                battle.rechargetimer,
                battle.poisontimer
            )
        end
        battle = applyeffects(battle)
        if !playerloses(battle)
            for (condition, spell) in (
                (canmagicmissile, magicmissile),
                (candrain, drain),
                (canshield, shield),
                (canpoison, poison),
                (canrecharge, recharge)
            )
                if condition(battle)
                    manacost, result = spell(battle)
                    queue = insert(queue, manaspent + manacost, bossturn(result))
                end
            end
        end
    end
end

end # module
