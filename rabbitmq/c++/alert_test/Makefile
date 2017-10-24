include ../make.comm

BIN=alert_consumer alert_producer

all:${BIN}

alert_consumer:alert_consumer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

alert_producer:alert_producer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

clean:
	rm -f ${BIN} *.o
