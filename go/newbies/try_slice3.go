package main

import (
	"bytes"
)

func badSliceAppend() {
	path := []byte("AAAAA/123456")
	idx := bytes.IndexByte(path, '/')
	println(idx)

	dir1 := path[:idx]
	dir2 := path[idx+1:]
	println(string(dir1))
	println(string(dir2))

	// WRONG!
	// dir1和dir2引用的都是相同的底层数据，修改dir1就修改了dir2
	dir1 = append(dir1, "suffix"...)
	println(string(path))

	path = bytes.Join([][]byte{dir1, dir2}, []byte("//"))
	println(string(path))
}

func goodSliceAppend() {
	path := []byte("AAAAA/123456")
	idx := bytes.IndexByte(path, '/')
	println("path[0]:", &path[0])

	// 这里不同， 指定了第三个参数，表示cap，再往里添加新元素时会重新分配
	dir1 := path[:idx:idx]
	println("origin dir1[0]:", &dir1[0])
	dir2 := path[idx+1:]
	println(string(dir1))

	dir1 = append(dir1, "suffix"...)
	println("appended dir1[0]:", &dir1[0])

	path = bytes.Join([][]byte{dir1, dir2}, []byte("//"))
	println(string(path))

}

func main() {
	badSliceAppend()
	println("-----------")
	goodSliceAppend()

}
