package main

import (
	"fmt"
)

type Duration int64

func (d *Duration) Pretty() {
	fmt.Println("duration: ", *d)
}

func main() {
	// 这里有编译错误，Pretty定义的接收者是指针，但是Duration(64)不能确定指针地址
	Duration(64).Pretty()
}
