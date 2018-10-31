package main

import (
	"fmt"
)

func main() {

	var data *byte     // nil
	var in interface{} // nil

	fmt.Println(data, data == nil) // true
	fmt.Println(in, in == nil)     // true

	in = data
	// in == nil only when it's nil of type and value
	fmt.Println(in, in == nil) // in has value, not nil any more
}
