CXXFLAGS=-g -Wall -I../
LDFLAGS=-lrabbitmq
CC=g++

BIN=alert_consumer alert_producer

all:${BIN}

alert_consumer:../common.o alert_consumer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

alert_producer:../common.o alert_producer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

clean:
	rm -f ${BIN} *.o
