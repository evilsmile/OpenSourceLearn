include ../make.comm

BIN=headers_consumer headers_producer

all:${BIN}

headers_consumer:headers_consumer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

headers_producer:headers_producer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

clean:
	rm -f ${BIN} *.o
