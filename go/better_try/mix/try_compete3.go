package main

import (
	"fmt"
	"sync"
	"sync/atomic"
	"time"
)

var (
	stopFlag int64
	wg       sync.WaitGroup
)

func main() {
	wg.Add(2)

	go doWork("worker1")
	go doWork("worker2")

	time.Sleep(2 * time.Second)
	atomic.StoreInt64(&stopFlag, 1)

	wg.Wait()
}

func doWork(id string) {
	defer wg.Done()

	for {
		fmt.Println(id, " is doing work!")
		time.Sleep(250 * time.Millisecond)

		if atomic.LoadInt64(&stopFlag) == 1 {
			fmt.Println(id, " stops work.")
			return
		}
	}
}
