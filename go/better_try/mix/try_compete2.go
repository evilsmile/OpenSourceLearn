package main

import (
	"fmt"
	"runtime"
	"sync"
	"sync/atomic"
)

var (
	value int64
	wg    sync.WaitGroup
)

func main() {

	wg.Add(2)

	go count(1)
	go count(2)

	wg.Wait()
	fmt.Println("Count: ", value)
}

func count(id int) {
	defer wg.Done()
	// 这里会出现竞争，导致结果不对
	for i := 0; i < 2; i++ {
		atomic.AddInt64(&value, 1)
		runtime.Gosched()
	}
}
