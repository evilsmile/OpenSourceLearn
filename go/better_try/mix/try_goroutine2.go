package main

import (
	"fmt"
	"runtime"
	"sync"
)

func main() {

	// 为每个可用核心分配一个CPU
	runtime.GOMAXPROCS(runtime.NumCPU())

	var wg sync.WaitGroup

	wg.Add(2)

	go func() {
		defer wg.Done()
		SearchPrime("A")
	}()

	go func() {
		defer wg.Done()
		SearchPrime("B")
	}()

	wg.Wait()
}

func SearchPrime(who string) {
Next:
	for outer := 2; outer < 500000; outer++ {
		for inner := 2; inner < outer; inner++ {
			if outer%inner == 0 {
				continue Next
			}
		}
		fmt.Println(who, ": ", outer)
	}
	fmt.Println(who, " DONE")
}
