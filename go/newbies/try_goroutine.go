package main

import (
	"fmt"
	"time"
)

func printNum(start, end uint64, c chan int) {
	for i := start; i < end; i++ {
		fmt.Printf("%d\t", i)
		time.Sleep(time.Second)
	}
	c <- 0
}

func main() {

	c := make(chan int, 20)
	go printNum(1, 10, c)
	go printNum(11, 20, c)

	_ = <-c
	_ = <-c
}
