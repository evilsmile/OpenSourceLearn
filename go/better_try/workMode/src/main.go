package main

import (
	"fmt"
	"sync"
	"time"
	"work"
)

var names = []string{
	"Tom",
	"Bob",
	"Alice",
	"Cherry",
	"Donald",
	"Emma",
}

type namePrint struct {
	name string
}

func (n *namePrint) Task() {
	fmt.Println(n.name)
	time.Sleep(time.Second)
}

func main() {

	p := work.New(2)

	var wg sync.WaitGroup
	wg.Add(100 * len(names))

	for i := 0; i < 100; i++ {
		for _, name := range names {
			np := namePrint{name}
			go func() {
				p.Run(&np)
				wg.Done()
			}()
		}
	}
	wg.Wait()
	p.Shutdown()
}
