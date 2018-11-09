GO=go
GOSRC=$(shell ls *.go)
BIN=chatter

${BIN}:${GOSRC}
	${GO} build $^

clean:
	rm -f ${BIN}
