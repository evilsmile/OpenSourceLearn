package main

import (
	"fmt"
	"time"
)

func main() {
	select {
	case <-time.After(time.Second * 2):
		fmt.Println("TIMEOUT")
	}
}
