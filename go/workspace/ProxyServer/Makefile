.PHONY: clean

BIN=proxy_server
all: ${BIN}

${BIN}: main.go
	go build -o $@ $^

clean:
	rm -f ${BIN}
