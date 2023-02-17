if basename(pwd()) != "19"
    cd("2015/19")
end

function loadfile(filename)
    lines = eachline(filename)
    [Pair(split(s, " => ")...) for s in Iterators.takewhile(!isempty, lines)], first(lines)
end

function partone(filename)
    replacements, molecule = loadfile(filename)
    molecules = Set{AbstractString}()
    for (l, r) in replacements
        for m in eachmatch(Regex(l), molecule)
            push!(molecules,
                view(molecule, 1:m.offset-1) *
                    r *
                    view(molecule, m.offset+length(m.match):length(molecule)))
        end
    end
    length(molecules)
end
