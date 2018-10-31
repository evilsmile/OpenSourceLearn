package main

import (
	"fmt"
	"time"
)

func main() {
	chl := make(chan int, 10)
	cnt := 0

	for {
		chl <- 1
		go func() {
			cnt++
			fmt.Printf("get a request. handled.[%d]\n", cnt)
			time.Sleep(time.Second)
			<-chl
		}()
	}

}
