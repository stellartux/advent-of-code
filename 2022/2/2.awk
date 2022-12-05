#!/usr/bin/awk -f

$2 ~ /X/      { score1 += 1 }
$2 ~ /Y/      { score1 += 2 }
$2 ~ /Z/      { score1 += 3 }
/A X|B Y|C Z/ { score1 += 3 }
/A Y|B Z|C X/ { score1 += 6 }

/B X/ { score2 += 1 }
/C X/ { score2 += 2 }
/A X/ { score2 += 3 }
/A Y/ { score2 += 4 }
/B Y/ { score2 += 5 }
/C Y/ { score2 += 6 }
/C Z/ { score2 += 7 }
/A Z/ { score2 += 8 }
/B Z/ { score2 += 9 }

END {
    print score1
    print score2
}
