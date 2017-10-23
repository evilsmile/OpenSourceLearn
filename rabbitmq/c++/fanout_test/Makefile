CXXFLAGS=-g -Wall -I../
LDFLAGS=-lrabbitmq
CC=g++

BIN=fanout_consumer fanout_producer

all:${BIN}

fanout_consumer:../common.o fanout_consumer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

fanout_producer:../common.o fanout_producer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

clean:
	rm -f ${BIN} *.o
