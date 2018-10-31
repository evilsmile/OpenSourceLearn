package main

import (
	"fmt"
	"sort"
)

type IntArrays []int

func (ia IntArrays) Len() int           { return len(ia) }
func (ia IntArrays) Less(i, j int) bool { return ia[i] < ia[j] }
func (ia IntArrays) Swap(i, j int)      { ia[i], ia[j] = ia[j], ia[i] }

func main() {
	testData := &IntArrays{8, 3, 2, 9, 2}
	sort.Sort(testData)

	for _, i := range *testData {
		fmt.Printf("%d ", i)
	}
	fmt.Println()
}
