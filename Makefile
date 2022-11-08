# needs cosmopolitan libc installed to /opt/cosmo
CC=cosmocc
COSMO=/opt/cosmo

2015/21/21.com: 2015/21/21.c
	$(CC) -o 2015/21/21.com 2015/21/21.c

2015/25/25.com: 2015/25/25.c
	$(CC) -o 2015/25/25.com 2015/25/25.c

2017/17/17.com: 2017/17/17.c
	$(CC) -o 2017/17/17.com 2017/17/17.c

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

2019/2/2.com: 2019/2/2.c
	$(CC) -o $@ 2019/2/2.c
