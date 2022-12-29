# gcc/cosmocc - Compile an Actually Portable Executable with `make`
# Don't have Cosmopolitan LibC? `make CFLAGS= LDFLAGS=` to compile with glibc
COSMO=/opt/cosmo
CFLAGS=-g -Os -static -fno-pie -no-pie -nostdlib -nostdinc -gdwarf-4 -fno-omit-frame-pointer -pg -mnop-mcount -mno-tls-direct-seg-refs -I$(COSMO)
LDFLAGS=-Wl,--gc-sections -fuse-ld=bfd -Wl,--gc-sections -Wl,-T,$(COSMO)/o/$(MODE)/ape/ape.lds -include $(COSMO)/o/$(MODE)/cosmopolitan.h $(COSMO)/o/$(MODE)/libc/crt/crt.o $(COSMO)/o/$(MODE)/ape/ape-no-modify-self.o $(COSMO)/o/$(MODE)/cosmopolitan.a

PROGRAMS=2015/21/21 2015/25/25 2017/17/17 2018/19/19 2019/2/2 2019/24/24-1

all: $(foreach prog,$(PROGRAMS),$(prog).com)

%.com: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $< $(LDFLAGS)

2018/9/9-1.com: 2018/9/9.c
	$(CC) $(CFLAGS) -o $@ -D PARTONE 2018/9/9.c $(LFLAGS)

2018/13/13-1.com: 2018/13/13.c
	$(CC) $(CFLAGS) -o $@ -D PARTONE 2018/13/13.c $(COSMO)/libc/x/xslurp.c $(LFLAGS)


2019/2/2.com: 2019/2/2.c
	$(CC) $(CFLAGS) -o $@ 2019/2/2.c $(LFLAGS)


