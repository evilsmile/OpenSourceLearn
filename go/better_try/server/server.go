package main

import (
	"fmt"
	"log"
	"net"
)

const (
	PORT = 6555
)

func handleConnection(conn net.Conn) {
	defer conn.Close()
	log.Println("handling....")
	buffer := make([]byte, 1024)
	// Read data from connection
	conn.Read(buffer)
	fmt.Println(string(buffer))

	log.Printf("received req:%s", buffer)

	conn.Write(buffer)
	log.Println("handle done.")
}

func main() {
	l, err := net.Listen("tcp", "127.0.0.1:6555")
	if err != nil {
		fmt.Println(err)
		return
	}

	defer l.Close()

	log.Printf("Tcp server start listening on %d....\n", PORT)

	for {
		log.Println("loop test")
		// accept and create connection
		conn, err := l.Accept()
		if err != nil {
			return
		}

		go handleConnection(conn)
	}
}
