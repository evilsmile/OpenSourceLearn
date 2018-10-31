package main

import (
	"fmt"
	"time"
)

func main() {
	timeout := time.NewTimer(0)

	<-timeout.C

	times := 0

loop:
	for {
		timeout.Reset(time.Second * 2)
		select {
		case <-timeout.C:
			fmt.Println("Timout detected")
			times++
			if times > 3 {
				fmt.Println("Timout cancel")
				timeout.Stop()
				break loop
			}
		}
	}
}
