package main

import (
	"fmt"
	"net"
)

func main() {
	socket, err := net.DialUDP("udp4", nil, &net.UDPAddr{
		IP:   net.IPv4(127, 0, 0, 1),
		Port: 23452,
	})
	if err != nil {
		fmt.Println("connect failed!", err)
		return
	}

	defer socket.Close()

	sendata := []byte("Hello Client")
	_, err = socket.Write(sendata)
	if err != nil {
		fmt.Println("send data failed!", err)
		return
	}

	data := make([]byte, 4096)
	read, remoteAddr, err := socket.ReadFromUDP(data)
	if err != nil {
		fmt.Println("read data failed!", err)
		return
	}
	fmt.Println(read, remoteAddr)
	fmt.Printf("%s\n", data)
}
