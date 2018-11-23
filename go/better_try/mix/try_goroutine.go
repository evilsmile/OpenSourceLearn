package main

import (
	"fmt"
	"runtime"
	"sync"
)

func main() {

	runtime.GOMAXPROCS(1)

	var wg sync.WaitGroup

	wg.Add(2)

	go func() {
		defer wg.Done()
		for cnt := 1; cnt < 3; cnt++ {
			for a := 'a'; a < 'a'+26; a++ {
				fmt.Printf("%c ", a)
			}
		}
	}()

	go func() {
		defer wg.Done()
		for cnt := 1; cnt < 3; cnt++ {
			for a := 'A'; a < 'A'+26; a++ {
				fmt.Printf("%c ", a)
			}
		}
	}()

	wg.Wait()
}
