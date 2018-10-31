package main

import (
	"fmt"
)

func main() {
	var slice []int
	slice = append(slice, []int{1, 2, 3, 4, 5, 6}...)

	slice = slice[:copy(slice, slice[5:])]
	fmt.Printf("left new slice %+v\n", slice)
}
