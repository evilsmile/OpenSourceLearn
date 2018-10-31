package main

import (
	"encoding/json"
	"fmt"
)

type MyData struct {
	Name string
	Aget uint
}

func main() {
	d1 := MyData{"Ian", 20}

	encode, err := json.Marshal(d1)
	if err != nil {
		fmt.Println("error marshall!")
		return
	}

	fmt.Println("encode json: ", string(encode))

	var out MyData
	json.Unmarshal(encode, &out)
	fmt.Printf("%+v\n", out)
}
