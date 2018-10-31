package main

import (
	"fmt"
)

func main() {
	const (
		a = iota
		b = 8
		c = iota
	)
	fmt.Println(c)
}
