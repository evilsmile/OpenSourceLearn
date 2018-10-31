package main

import (
	"fmt"
	"time"
)

var bufChnl chan int

func client(serverChnl chan int) {
	var i int
	for {
		select {
		case i = <-bufChnl:
			fmt.Println("get ", i)
		default:
			fmt.Println("use default -1")
			i = -1
		}
		serverChnl <- i
		time.Sleep(time.Second)
	}

}

func server(serverChnl chan int) {
	for {
		i := <-serverChnl
		select {
		case bufChnl <- i:
			fmt.Println("push ", i, " back")
		default:
			fmt.Println("buffer full!")
		}

		time.Sleep(5 * time.Second)
	}
}

func main() {
	bufChnl = make(chan int, 20)
	serverChnl := make(chan int, 20)

	for i := 0; i < 20; i++ {
		bufChnl <- i
	}

	go client(serverChnl)
	go server(serverChnl)

	time.Sleep(100 * time.Second)
}
