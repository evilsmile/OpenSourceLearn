package main

import (
	"fmt"
	"reflect"
)

type Data struct {
	name    string
	age     int
	f       float32
	complex complex64
	char    rune
	flag    bool
	events  <-chan string
	handle  interface{}
	ref     *byte
	raw     [10]byte
	//	info    map[string]string    // map can't be compared, or compile error
}

type Data2 struct {
	info map[string]string // 需要使用relfect.DeepEqual()来比较
}

func main() {
	d1 := Data{}
	d2 := Data{}

	fmt.Println(d1 == d2)

	dd1 := Data2{}
	dd2 := Data2{}
	fmt.Println(reflect.DeepEqual(dd1, dd2))

	s1 := []int{1, 2, 3}
	s2 := []byte{1, 2, 3}
	fmt.Println(reflect.DeepEqual(s1, s2))

}
