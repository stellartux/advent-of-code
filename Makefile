# needs cosmopolitan libc installed to /opt/cosmo
CC=cosmocc
COSMO=/opt/cosmo

2018/9/9-1.com: 2018/9/9.c
	$(CC) -o 2018/9/9-1.com \
	-D PARTONE \
	2018/9/9.c

2018/13/13-1.com: 2018/13/13.c
	$(CC) -o 2018/13/13-1.com \
	-D PARTONE \
	2018/13/13.c \
	$(COSMO)/libc/x/xslurp.c
	2018/13/13.c \
	$(COSMO)/libc/x/xslurp.c \
	$(COSMO)/third_party/linenoise/linenoise.c
