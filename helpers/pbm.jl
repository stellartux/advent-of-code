"Converts a bitmatrix to plain PBM format."
function pbm(bitmatrix)
    join(
        (
            "P1",
            join(reverse(size(bitmatrix)), ' '),
            (
                join(map(b -> b ? '1' : '0', row), ' ')
                for row in Iterators.partition(transpose(bitmatrix), 34)
            )...
        ),
        '\n'
    ) * '\n'
end

pbm(filename::AbstractString, bitmatrix) = open(f -> write(f, pbm(bitmatrix)), filename, "w")
