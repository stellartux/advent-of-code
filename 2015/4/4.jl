if basename(pwd()) == "aoc"
    cd("2015/4")
end
using Pkg
Pkg.activate(".")
using MD5

function hashsearch(needle, key)
    for i in Iterators.countfrom(1)
        if startswith(bytes2hex(md5(key * string(i))), needle)
            return i
        end
    end
end

@assert hashsearch("00000", "abcdef") == 609043
@assert hashsearch("00000", "pqrstuv") == 1048970
hashsearch("00000", "yzbqklnj")
hashsearch("000000", "yzbqklnj")
