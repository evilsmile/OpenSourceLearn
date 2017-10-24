include ../make.comm

BIN=fanout_consumer fanout_producer

all:${BIN}

fanout_consumer:fanout_consumer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

fanout_producer:fanout_producer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

clean:
	rm -f ${BIN} *.o
