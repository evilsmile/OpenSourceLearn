CXXFLAGS=-g -Wall -I../
LDFLAGS=-lrabbitmq
CC=g++

LIB=libutil.a

OBJS=common.o

all:${LIB}

${LIB}:${OBJS}
	ar crv $@ $^
	mv ${LIB} ../libs/

clean:
	rm -f ${LIB} *.o
