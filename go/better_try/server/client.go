package main

import (
	"fmt"
	"net"
)

func main() {
	// net.Dial, get tcp connection
	conn, err := net.Dial("tcp", "127.0.0.1:6555")
	if err != nil {
		fmt.Println(err)
		return
	}

	defer conn.Close()

	conn.Write([]byte("echo data to server, then back to client!!!"))
	fmt.Println("test sever")

	buffer := make([]byte, 1024)
	// Read data from connection
	conn.Read(buffer)
	fmt.Println(string(buffer))
}
