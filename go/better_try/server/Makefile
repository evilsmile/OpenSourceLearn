GO_SRC=$(shell ls *.go)
GO_BIN=$(patsubst %.go,%,${GO_SRC})
all:${GO_BIN}

server: server.go
	go build $<

client: client.go
	go build $<


udp_server: udp_server.go
	go build $<

udp_cli: udp_cli.go
	go build $<

http_cli: http_cli.go
	go build $<

http_cli2: http_cli2.go
	go build $<

http_cli3: http_cli3.go
	go build $<
