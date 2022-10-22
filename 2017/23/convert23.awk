BEGIN {
    print "#include <stdio.h>\n#include <stdlib.h>\n"
    print "int main() {"
    print "  int a = 1, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0;"
}

{
    i += 1
    print "Line" i ":"
    switch ($1) {
        case "set":
            print "  " $2 " = " $3 ";"
            break
        case "sub":
            print "  " $2 " -= " $3 ";"
            break
        case "mul":
            print "  " $2 " *= " $3 ";"
            break
        case "jnz":
            print "  if (" $2 ") goto Line" (i + $3) ";"
            break
    }
}

END { 
    i += 1
    print "Line" i ":"
    print "  printf(\"%d\\n\", h);"
    print "}" 
}