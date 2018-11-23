package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

var (
	wg sync.WaitGroup
)

func init() {
	rand.Seed(time.Now().UnixNano())
}

func main() {

	ch := make(chan int)

	wg.Add(2)

	// 模仿两个人打网球
	go goPlay("Alice", ch)
	go goPlay("Bob", ch)

	ch <- 1

	wg.Wait()
}

func goPlay(name string, ch chan int) {
	defer wg.Done()
	for {
		ball, ok := <-ch
		if !ok {
			// channel closed
			fmt.Println(name, " Winned!")
			return
		}

		// 随机接球失败
		n := rand.Intn(100)
		if n%13 == 0 {
			fmt.Println(name, " missed")
			close(ch)
			return
		}

		fmt.Println(name, " hits ball!")
		ball++

		ch <- ball

	}
}
