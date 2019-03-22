package main

import (
	"github.com/RequestDispatcher/server"
)

const (
	LISTEN_IP   = "0.0.0.0"
	LISTEN_PORT = 9987

	BACKEND_HOST = "127.0.0.1:9999"
)

func main() {
	proxy := server.NewProxyServer(LISTEN_IP, LISTEN_PORT, BACKEND_HOST)
	proxy.Run()
}
