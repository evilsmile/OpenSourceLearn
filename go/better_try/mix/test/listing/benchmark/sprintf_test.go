package main

import (
	"fmt"
	"strconv"
	"testing"
)

// go test -v -run="none" -bench=.  -benchtime="3s" -benchmem
// "none" 保证在运行基准测试前没有其它测试会被运行

func BenchmarkSprintf(b *testing.B) {
	number := 10

	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// 转换为字符串
		fmt.Sprintf("%d", number)
	}
}

func BenchmarkFormat(b *testing.B) {
	number := (int64)(10)
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// 转换为字符串
		strconv.FormatInt(number, 10)
	}
}

func BenchmarkItoa(b *testing.B) {
	number := 10
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		// 转换为字符串
		strconv.Itoa(number)
	}
}
