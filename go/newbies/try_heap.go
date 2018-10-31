/* package 'heap' supplies heap operations for any type which implements heap interface.
 * (small) heap is a tree whose root has the smallest value compared with subtree.
 *
 * func Fix(h Interface, i int)  //  在修改第i个元素后，调用本函数修复堆，比删除第i个元素后插入新元素更有效率。复杂度O(log(n))，其中n等于h.Len()。
 * func Init(h Interface)  //初始化一个堆。一个堆在使用任何堆操作之前应先初始化。Init函数对于堆的约束性是幂等的（多次执行无意义），并可能在任何时候堆的约束性被破坏时被调用。本函数复杂度为O(n)，其中n等于h.Len()。
 * func Pop(h Interface) interface{}  //删除并返回堆h中的最小元素（不影响约束性）。复杂度O(log(n))，其中n等于h.Len()。该函数等价于Remove(h, 0)。
 * func Push(h Interface, x interface{})  //向堆h中插入元素x，并保持堆的约束性。复杂度O(log(n))，其中n等于h.Len()。
 * func Remove(h Interface, i int) interface{}  //删除堆中的第i个元素，并保持堆的约束性。复杂度O(log(n))，其中n等于h.Len()。

 */
package main

import (
	"container/heap"
	"fmt"
)

type IntHeap []int

func (h IntHeap) Len() int           { return len(h) }
func (h IntHeap) Less(i, j int) bool { return h[i] < h[j] }
func (h IntHeap) Swap(i, j int)      { h[i], h[j] = h[j], h[i] }

func (h *IntHeap) Push(x interface{}) {
	*h = append(*h, x.(int))
}

func (h *IntHeap) Pop() interface{} {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

func main() {
	h := &IntHeap{2, 1, 5, 20, 342}
	heap.Init(h)
	heap.Push(h, 3)
	heap.Fix(h, 3)
	fmt.Printf("mininum: %d\n", (*h)[0])

	for h.Len() > 0 {
		fmt.Printf("%d ", heap.Pop(h))
	}
}
