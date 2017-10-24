include ../make.comm

BIN=consumer producer logger

all:${BIN}

consumer:consumer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

logger:logger.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

producer:producer.o
	${CC} ${CXXFLAGS} -o $@ $^ ${LDFLAGS}

clean:
	rm -f ${BIN} *.o
