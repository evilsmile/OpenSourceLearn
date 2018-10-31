package main

import (
	"fmt"
	"time"
)

func subgo(chnl chan bool) {

	for {
		select {
		case <-chnl:
			fmt.Println("subgo gets channel msg, sleep 1s")
			chnl <- true
			fmt.Println("OK")
			return
		default:
			fmt.Print("a")
		}
	}

}

func main() {
	chl := make(chan bool)
	go subgo(chl)

	time.Sleep(time.Second)
	chl <- true
	//	<-chl
}
