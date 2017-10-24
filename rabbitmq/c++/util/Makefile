include ../make.comm

LIB=libutil.a

SRCS=$(shell ls *.cpp)
OBJS=$(patsubst %.cpp,%.o,${SRCS})

all:${LIB}

${LIB}:${OBJS}
	ar crv $@ $^
	mv ${LIB} ../libs/

clean:
	rm -f ${LIB} *.o
