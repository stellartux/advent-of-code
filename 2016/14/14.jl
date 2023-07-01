using MD5: md5

function day14(salt::AbstractString, extra=0)
    hashcache = Dict{Int,AbstractString}()
    hash(n) =
        get!(hashcache, n) do
            result = bytes2hex(md5(salt * string(n)))
            for _ = 1:extra
                result = bytes2hex(md5(result))
            end
            result
        end
    found = 0
    for i = Iterators.countfrom(0)
        m = match(r"(.)\1\1", hash(i))
        if !isnothing(m)
            re = Regex("$(m[1]){5}")
            if any(n -> occursin(re, hash(n)), i .+ (1:1000))
                found += 1
                if found == 64
                    return i
                end
            end
        end
    end
end
