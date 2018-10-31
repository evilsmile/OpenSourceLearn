package main

import (
	"fmt"
)

type Printer interface {
	print()
}

type Data struct {
	name string
}

func (d *Data) print() {
	fmt.Println("Hello there!")
}

func main() {
	d1 := Data{"Data1"}
	fmt.Printf("d1: %T\n", d1)
	d1.print()

	var d2 Printer = &Data{"Data2"}
	fmt.Printf("d2: %T\n", d2)
	d2.print()
}
