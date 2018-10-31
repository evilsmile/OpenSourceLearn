package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"net"
	"reflect"
)

func main() {
	addr := "www.baidu.com:80"
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		log.Fatal(err)
		return
	}

	fmt.Println("Public IP Addr: ", conn.RemoteAddr().String())
	fmt.Printf("Client Conn Addr: %v\n", conn.LocalAddr())

	fmt.Println("conn.LocalAddr's type is ", reflect.TypeOf(conn.LocalAddr()))
	fmt.Println("conn.RemoteAddr's type is ", reflect.TypeOf(conn.RemoteAddr().String()))
	n, err := conn.Write([]byte("GET / HTTP/1.1\r\n\r\n"))
	if err != nil {
		log.Fatal(err)
		return
	}
	fmt.Println("Send to server with data size: ", n)

	r := bufio.NewReader(conn)
	for {
		line, err := r.ReadString('\n')
		if err == io.EOF {
			conn.Close()
			break
		}
		fmt.Print(line)
	}
}
