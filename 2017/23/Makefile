.PHONY: build

build: solution23

23.c: convert23.awk input.txt
	awk -f convert23.awk input.txt >23.c

solution23: 23.c
	gcc -O3 -o solution23 23.c
