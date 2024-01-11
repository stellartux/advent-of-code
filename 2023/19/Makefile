%.c: convert.jl %.txt
	julia $^ > $@

%.tmp: %.c
	cc -o $@ $<

.SILENT:
%: %.tmp
	./$<
