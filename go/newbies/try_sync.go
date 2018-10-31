package main

import (
	"fmt"
	"sync"
	"time"
)

func doWork(workId int, in chan interface{}, quit chan struct{}, wait *sync.WaitGroup) {
	defer wait.Done()

	for {
		select {
		case <-quit:
			fmt.Printf("worker[%d] is done\n", workId)
			return
		case m := <-in:
			fmt.Println("Get data ", m)
		default:
			time.Sleep(3 * time.Second)
		}
	}
}

func main() {

	dataChnl := make(chan interface{})
	quitChnl := make(chan struct{})
	var wait sync.WaitGroup

	for i := 0; i < 3; i++ {
		wait.Add(1)
		go doWork(i, dataChnl, quitChnl, &wait)
	}

	for i := 0; i < 3; i++ {
		dataChnl <- i
	}

	close(quitChnl)
	wait.Wait()
	close(dataChnl)

	fmt.Println("Main exit")
}
