package main

import (
	"fmt"
)

func main() {
	data := []*struct{ num int }{{1}, {2}, {3}}

	for _, d := range data {
		d.num *= 10
	}
	fmt.Printf("+%v\n", data[0])

}
