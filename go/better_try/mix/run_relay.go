package main

import (
	"fmt"
	"sync"
	"time"
)

var (
	wg sync.WaitGroup
)

func main() {

	barton := make(chan int)

	wg.Add(1)
	go run(barton)

	barton <- 1

	wg.Wait()
}

func run(barton chan int) {

	bartonIdx := <-barton

	time.Sleep(time.Second)

	fmt.Println(bartonIdx, " is running!")

	var bartonNewIdx int
	if bartonIdx != 4 {
		bartonNewIdx = bartonIdx + 1
		fmt.Println(bartonNewIdx, " is waiting on the line!")
		go run(barton)
	}

	if bartonIdx == 4 {
		wg.Done()
		fmt.Println("Running finished!")
		return
	}

	fmt.Println(bartonIdx, " pass barton to ", bartonNewIdx)

	barton <- bartonNewIdx
}
