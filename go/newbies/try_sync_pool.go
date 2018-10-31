package main

import (
	"log"
	"sync"
)

type Balls struct {
	size uint64
	name string
}

func main() {
	var pipe = &sync.Pool{
		New: func() interface{} {
			return &Balls{
				size: 12,
				name: "basketball",
			}
		},
	}

	// provide a string to poll
	newBall := &Balls{size: 88, name: "football"}
	pipe.Put(newBall)

	// get what pool have
	ball1 := pipe.Get().(*Balls)
	log.Println((*ball1).name)
	// now it's empty, call 'New'
	ball1 = pipe.Get().(*Balls)
	log.Println((*ball1).name)
}
