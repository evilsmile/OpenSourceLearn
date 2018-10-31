package main

import (
	"fmt"
)

type Data struct {
	name string
}

type Info map[string]Data

func (i Info) print() {

	for k, v := range i {
		fmt.Println(k, ":", v)
	}
}

type Info2 map[string]*Data

func (i Info2) print() {

	for k, v := range i {
		fmt.Println(k, ":", v)
	}
}
func main() {
	m := make(Info)
	m["d1"] = Data{"Value1"}
	m["d2"] = Data{"Value2"}
	m.print()

	// Compile error! Can't change value of struct within a map
	// m["d1"].name = "Changed Value1"
	// Unless switch total struct
	m["d1"] = Data{"Changed value1"}
	m.print()

	// Or use struct pointer
	m2 := make(Info2)
	m2["2d1"] = &Data{"2Value1"}
	m2["2d2"] = &Data{"2Value2"}
	m2.print()
	m2["2d1"].name = "Chagned 2value1"
	m2.print()

}
