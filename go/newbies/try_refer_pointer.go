package main

import (
	"fmt"
)

type MyData struct {
	num   int
	key   *string
	items map[string]string
}

func (d *MyData) Pointer() {
	d.num = 10
}

func (d MyData) Refer() {
	d.num = 22
	*d.key = "New Refer Key"
	d.items["date"] = "2018.10.23"
}

func main() {
	key := "InitalKEY"
	data := MyData{1, &key, make(map[string]string)}

	fmt.Printf("NUM: %v, key:%v, items:%v\n", data.num, *data.key, data.items)

	data.Pointer()
	fmt.Printf("NUM: %v, key:%v, items:%v\n", data.num, *data.key, data.items)

	data.Refer()
	fmt.Printf("NUM: %v, key:%v, items:%v\n", data.num, *data.key, data.items)

}
