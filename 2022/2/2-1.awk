$2 ~ /X/ { score = score + 1 }
$2 ~ /Y/ { score = score + 2 }
$2 ~ /Z/ { score = score + 3 }
$0 ~ /A Y/ || $0 ~ /B Z/ || $0 ~ /C X/ { score = score + 6 }
$0 ~ /A X/ || $0 ~ /B Y/ || $0 ~ /C Z/ { score = score + 3 }
END { print score }