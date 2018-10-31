package main

import (
	"fmt"
	"time"
)

var _ = time.Sleep

type Request struct {
	args       []int
	f          func([]int) int
	resultChan chan int
}

func handle(queue chan *Request) {
	for req := range queue {
		req.resultChan <- req.f(req.args)
	}
}

func sum(values []int) int {
	sum := 0
	for _, v := range values {
		sum += v
	}
	return sum
}

func main() {

	clientRequest := make(chan *Request)

	req := &Request{[]int{1, 2, 3}, sum, make(chan int)}

	for i := 0; i < 10; i++ {
		go handle(clientRequest)
	}

	clientRequest <- req
	ret := <-req.resultChan
	fmt.Println("Sum result is ", ret)
}
