package main

import "fmt"
import "unsafe"
import "time"

// 变量
// 自推导类型
var a = "Newbies"

// 定义变量、类型及初始值
var b string = "newbies.com"

// 定义变量及类型
var c bool

// 多变量声明
var x, y int

// 这种因式分解关键字的写法一般用于声明全局变量
var (
	g_a int
	g_b bool
)

var c2, d int = 1, 2
var e, f = 123, "Hello"

const (
	a31 = "a31"
	b31 = len(a31)
	// 字符串类型在 go 里是个结构, 包含指向底层数组的指针和长度,这两部分每部分都是 8 个字节，所以字符串类型大小为 16 个字节
	c31 = unsafe.Sizeof(a31)
)

type Books struct {
	title  string
	author string
}

func test() {
	panic("panic!")
}

func loop() {
	for ii := 0; ii < 10; ii++ {
		fmt.Printf("%d\n", ii)
	}
}

func main() {
	//test()
	fmt.Println("Hello World!")

	// 这种不带声明格式的只能在函数体中出现
	g, h := 123, "hello"

	fmt.Println(a, b, c)

	fmt.Println(g_a, g_b, c2, d, e, f, g, h)

	const LENGTH int = 10
	const WIDTH int = 5
	var area int
	// 多重赋值
	const a21, b21, c21 = 1, false, "str"

	area = LENGTH * WIDTH
	fmt.Printf("area is : %d", area)
	println()
	println(a21, b21, c21)

	println(a31, b31, c31)

	const (
		a41 = iota // 0
		b41        // 1
		c41        // 2
		d41 = "ha" // 独立值, iota+=1
		e41        // "ha" iota+=1
		f41 = 100  // iota+=1
		g41        // 100 iota+=1
		h41 = iota // 7, 恢复计数
		i41        // 8
	)
	fmt.Println(a41, b41, c41, d41, e41, f41, g41, h41, i41)

	const (
		i51 = 1 << iota // 1<<0 = 1
		j51 = 3 << iota // 3<<1 = 6
		k51             // 3<<2 = 12
		l51             // 3<<3 = 24
	)
	fmt.Println(i51, j51, k51, l51)

	var balance = [5]float32{1000.0, 2.0, 3.4, 7.0, 50.0}
	var n [10]int // n is an array of 10
	var i, j int

	for i = 0; i < 10; i++ {
		n[i] = i + 100
	}

	for j = 0; j < 10; j++ {
		fmt.Printf("element[%d] = %d\n", j, n[j])
	}

	for j = 0; j < 5; j++ {
		fmt.Printf("balance[%d] = %f\n", j, balance[j])
	}

	var ip *int // 声明指针变量
	ip = &area
	if ip != nil {
		fmt.Printf("address of area is:%x\n", ip)
	}

	var book1, book2 Books
	book1.title = "golang"
	book1.author = "www.runoob.com"
	book2.title = "python"
	book2.author = "www.runoob.com"
	printBook(&book1)
	printBook(&book2)

	// slice
	var numbers []int
	printSlice(numbers)

	//append to empty slice
	numbers = append(numbers, 0)
	printSlice(numbers)

	// append more elemens
	numbers = append(numbers, 2, 3, 4)
	printSlice(numbers)

	// create a slice with 2*cap of numbers
	numbers1 := make([]int, len(numbers), (cap(numbers))*2)

	copy(numbers1, numbers)
	printSlice(numbers1)

	// Range
	for i, num := range numbers {
		fmt.Printf("index: %d num: %d\n", i, num)
	}

	kvs := map[string]string{"a": "apple", "b": "banana"}
	for k, v := range kvs {
		fmt.Printf("%s -> %s\n", k, v)
	}

	for i, c := range "go" {
		fmt.Println(i, c)
	}

	go loop()
	loop()

	time.Sleep(time.Second)
}

func printBook(book *Books) {
	fmt.Printf("Book title : %s\n", book.title)
	fmt.Printf("Book author : %s\n", book.author)
}

func printSlice(x []int) {
	fmt.Printf("len=%d cap=%d slice=%v\n", len(x), cap(x), x)
}
