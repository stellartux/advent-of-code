# gcc/cosmocc - Compile an Actually Portable Executable with `make`
# Don't have Cosmopolitan LibC? `make CFLAGS= LFLAGS=` to compile with glibc
COSMO=/opt/cosmo
CFLAGS=-g -Os -static -fno-pie -no-pie -nostdlib -nostdinc -gdwarf-4 -fno-omit-frame-pointer -pg -mnop-mcount -mno-tls-direct-seg-refs
LFLAGS=-Wl,--gc-sections -fuse-ld=bfd -Wl,--gc-sections -Wl,-T,$(COSMO)/o/$(MODE)/ape/ape.lds -include $(COSMO)/o/$(MODE)/cosmopolitan.h $(COSMO)/o/$(MODE)/libc/crt/crt.o $(COSMO)/o/$(MODE)/ape/ape-no-modify-self.o $(COSMO)/o/$(MODE)/cosmopolitan.a

2015/21/21.com: 2015/21/21.c
	$(CC) $(CFLAGS) -o $@ 2015/21/21.c $(LFLAGS)

2015/25/25.com: 2015/25/25.c
	$(CC) $(CFLAGS) -o $@ 2015/25/25.c $(LFLAGS)

2017/17/17.com: 2017/17/17.c
	$(CC) $(CFLAGS) -o $@ 2017/17/17.c $(LFLAGS)

2018/9/9-1.com: 2018/9/9.c
	$(CC) $(CFLAGS) -o $@ -D PARTONE 2018/9/9.c $(LFLAGS)

2018/13/13-1.com: 2018/13/13.c
	$(CC) $(CFLAGS) -o $@ -D PARTONE 2018/13/13.c $(COSMO)/libc/x/xslurp.c $(LFLAGS)


2019/2/2.com: 2019/2/2.c
	$(CC) $(CFLAGS) -o $@ 2019/2/2.c $(LFLAGS)

2019/24/24-1.com: 2019/24/24-1.c
	$(CC) $(CFLAGS) -o $@ 2019/24/24-1.c $(LFLAGS)

