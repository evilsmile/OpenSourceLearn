package main

import (
	"fmt"
	"time"
)

func main() {
	ages01 := map[string]int{
		"alice": 31,
		"bob":   20,
	}

	ages02 := make(map[string]int)
	ages02["chris"] = 20
	ages02["paul"] = 30

	//m1 := make(map[string]int)
	//m2 := map[string]int{}

	for name, age := range ages01 {
		fmt.Printf("%s\t%d\n", name, age)
	}

	for name, age := range ages02 {
		fmt.Printf("%s\t%d\n", name, age)
	}

	for name := range ages02 {
		fmt.Printf("%s\n", name)
	}

	start := time.Now()
	fmt.Println(start)
}
