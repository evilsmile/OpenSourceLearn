package main

import "time"
import "fmt"

func main() {
	var start, elapsed time.Duration

	start = monotime.Now()
	time.Sleep(time.Millisecond)
	elapsed = monotime.Since(start)

	fmt.Println(elapsed)
}
