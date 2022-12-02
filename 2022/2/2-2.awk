$0 ~ /A X/ { score = score + 3 }
$0 ~ /A Y/ { score = score + 1 + 3 }
$0 ~ /A Z/ { score = score + 2 + 6 }
$0 ~ /B X/ { score = score + 1 }
$0 ~ /B Y/ { score = score + 2 + 3 }
$0 ~ /B Z/ { score = score + 3 + 6 }
$0 ~ /C X/ { score = score + 2 }
$0 ~ /C Y/ { score = score + 3 + 3 }
$0 ~ /C Z/ { score = score + 1 + 6 }
END { print score }