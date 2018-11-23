package main

import (
	"fmt"
	"runtime"
	"sync"
)

var (
	value int
	wg    sync.WaitGroup
	mutex sync.Mutex
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
		mutex.Lock()

		v := value
		runtime.Gosched()
		v++

		value = v

		mutex.Unlock()
	}
}
