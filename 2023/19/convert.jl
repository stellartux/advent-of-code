lines = eachline(ARGS[1])

println("int accepted(int x, int m, int a, int s) {
    goto in;")

for line in lines
    if isempty(line)
        break
    end
    fs = split(line, r"[{},]"; keepempty=false)
    println(fs[1], ":")
    for f in fs[2:end-1]
        l, r = split(f, ':')
        println("    if (", l, ") goto ", r, ";")
    end
    println("    goto ", fs[end], ";")
end

println("""A:
    return x + m + a + s;
R:
    return 0;
}

#include <stdio.h>

int main(void) {
    int total = 0;""")

for line in lines
    println("    total += accepted(", filter(c -> isdigit(c) || c==',', line), ");")
end

println("""    printf("%d\\n", total);\n}""")
