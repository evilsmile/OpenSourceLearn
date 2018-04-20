SRCS=$(shell ls *cpp)
OBJS=$(patsubst %.cpp,%.o,${SRCS})

CXXFLAGS=-g 
LDFLAGS=-lboost_regex
CC=g++

BIN=test_regex

all:${BIN}

${BIN}:${OBJS}
	${CC} ${CXXFLAGS} -o $@ $<  ${LDFLAGS}

clean:
	rm -f ${BIN} ${OBJS}
