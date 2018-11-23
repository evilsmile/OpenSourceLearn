package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

const (
	numOfWorkers = 4
	totalWork    = 10
)

var (
	wg sync.WaitGroup
)

func init() {
	rand.Seed(time.Now().Unix())
}

func main() {

	workCh := make(chan int, totalWork)

	wg.Add(numOfWorkers)
	for i := 0; i < numOfWorkers; i++ {
		go Worker(i, workCh)
	}

	for i := 0; i < totalWork; i++ {
		workCh <- i
	}

	close(workCh)

	wg.Wait()
}

func Worker(id int, workCh chan int) {
	fmt.Println(id, " ready for work!")

	defer wg.Done()

	for {
		workIdx, ok := <-workCh
		if !ok {
			fmt.Println("All works done. Worker ", id, " off work!")
			return
		}

		fmt.Println("worker ", id, " is handling ", workIdx)
		workDuration := rand.Int63n(100)
		time.Sleep(time.Duration(workDuration) * time.Millisecond)
		fmt.Println("worker ", id, " has finished work ", workIdx)
	}
}
