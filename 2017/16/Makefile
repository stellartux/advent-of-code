# to solve file "input.txt" run `make -s input`

tmp_%.c: 16.sh %.txt
	sh $^ >$@

tmp_%: tmp_%.c
	$(CC) -o $@ $<

%: tmp_%
	./$<
