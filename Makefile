# needs cosmopolitan libc installed to /opt/cosmo
CC=cosmocc
COSMO=/opt/cosmo

2018/13/13.com: 2018/13/13.c
	$(CC) -o 2018/13/13.com \
	2018/13/13.c \
	$(COSMO)/libc/x/xslurp.c \
	$(COSMO)/third_party/linenoise/linenoise.c
