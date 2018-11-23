package main

import (
	"fmt"
	"time"
)

func main() {

	for i := 0; i < 3; i++ {
		now := time.Now()
		fmt.Printf("%T %+v\n", now, now.Unix())
		time.Sleep(2 * time.Second)
	}
}
