package main

import (
	"fmt"
	"net"
	"sync"
	"time"
)

var wait sync.WaitGroup

func newAccess() {
	conn, err := net.Dial("tcp", "127.0.0.1:6555")
	if err != nil {
		fmt.Println(err)
		return
	}

	defer conn.Close()
	defer wait.Done()

	conn.Write([]byte("echo data to server, then back to client!!!"))

	buffer := make([]byte, 1024)
	// Read data from connection
	conn.Read(buffer)
	//	fmt.Println(string(buffer))
}

func main() {
	concurrent := 10
	// net.Dial, get tcp connection
	startSec := time.Now().Unix()
	sentReq := 0
	total := 0
	for {
		nowSec := time.Now().Unix()
		if nowSec == startSec && sentReq < concurrent {
			fmt.Printf("%d:%d\n", total, sentReq)
			sentReq++
			total++
			wait.Add(1)
			go newAccess()
		} else if nowSec > startSec {
			fmt.Println("Send another round -------------------->>>>>>")
			startSec = nowSec
			sentReq = 0
		}
	}

}
