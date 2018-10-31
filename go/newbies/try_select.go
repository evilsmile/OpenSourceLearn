package main

import (
	"fmt"
)

func main() {
	ch1 := make(chan int, 1)
	ch2 := make(chan int, 1)

	for {
		select {
		case e1 := <-ch1:
			fmt.Println("case 1. e1=%v", e1)
		case e2 := <-ch2:
			fmt.Println("case 2. e2=%v", e2)
		default:
			fmt.Println("default")
		}
	}
}
