package main

import (
	"fmt"
	"net"
)

func main() {
	socket, err := net.ListenUDP("udp4", &net.UDPAddr{
		IP:   net.IPv4(127, 0, 0, 1),
		Port: 23452,
	})
	if err != nil {
		fmt.Println("UDP listen failed!", err)
		return
	}
	fmt.Println("Start listen...")
	defer socket.Close()
	for {
		data := make([]byte, 4096)
		read, remoteAddr, err := socket.ReadFromUDP(data)
		if err != nil {
			fmt.Println("Read data failed!", err)
			continue
		}

		fmt.Println(read, remoteAddr)
		fmt.Printf("%s\n\n", data)

		sendata := []byte("Hello client!")
		_, err = socket.WriteToUDP(sendata, remoteAddr)
		if err != nil {
			fmt.Println("send data failed!", err)
			return
		}
	}
}
