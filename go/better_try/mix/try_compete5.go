package main

import (
	"fmt"
	"runtime"
	"sync"
)

var (
	value int64
	wg    sync.WaitGroup
	inCh  chan int64
)

func main() {

	// 必须使用带缓冲的chan, 否则会死锁
	inCh = make(chan int64)
	//inCh = make(chan int64, 1)
	wg.Add(2)

	go count(1)
	go count(2)

	inCh <- value

	wg.Wait()

	value = <-inCh
	fmt.Println("Count: ", value)
}

func count(id int) {
	defer wg.Done()
	for i := 0; i < 2; i++ {
		v := <-inCh
		v++
		inCh <- v
		runtime.Gosched()
	}
}
