CXXFLAGS=-g -Wall
LDFLAGS=-lrabbitmq
CC=g++

BIN=consumer producer

all:${BIN}

consumer:common.o consumer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

producer:common.o producer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

clean:
	rm -f ${BIN} *.o
