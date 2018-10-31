package main

import (
	"fmt"
)

func get() []byte {
	buf := make([]byte, 1000)
	fmt.Printf("%v %v %v\n", len(buf), cap(buf), &buf[0])

	// 分配了1000bytes的空间， 但只使用了3 bytes，浪费。 因为切片使用的是相同的底层
	return buf[:3]
}

func get2() (res []byte) {
	buf := make([]byte, 10000)
	fmt.Printf("%v %v %v\n", len(buf), cap(buf), &buf[0])

	// 拷贝临时数据， 而不是重新切片
	res = make([]byte, 3)
	copy(res, buf)

	return
}

func main() {
	buf := get()
	fmt.Printf("%v %v %v\n", len(buf), cap(buf), &buf[0])

	buf = get2()
	fmt.Printf("%v %v %v\n", len(buf), cap(buf), &buf[0])
}
