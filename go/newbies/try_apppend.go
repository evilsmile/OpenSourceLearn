package main

import (
	"fmt"
)

func main() {
	a := []int{1, 2, 3, 4}
	b := append(a[:1], a[2:]...)
	fmt.Printf("b: %v\n", b)
}
