package main

import "fmt"

var keys = [3]byte{90, 91, 92}

type node struct {
	value       int
	left, right *node
}

var root *node = nil

func addNode(nodeToInsert *node, val int) {
	if nodeToInsert == nil {
		newNode := new(node)
		newNode.value = val
		newNode.left = nil
		newNode.right = nil
		root = newNode
	} else if val < nodeToInsert.value {
		if nodeToInsert.left == nil {
			newNode := new(node)
			newNode.value = val
			newNode.left = nil
			newNode.right = nil
			nodeToInsert.left = newNode
		} else {
			addNode(nodeToInsert.left, val)
		}
	} else if val > nodeToInsert.value {
		if nodeToInsert.right == nil {
			newNode := new(node)
			newNode.value = val
			newNode.left = nil
			newNode.right = nil
			nodeToInsert.right = newNode
		} else {
			addNode(nodeToInsert.right, val)
		}
	} else {
		fmt.Println("value exist! ignore ", val)
	}
}

func traverse(nodeTarget *node) {
	if nodeTarget == nil {
		return
	}

	if nodeTarget.right != nil {
		traverse(nodeTarget.right)
	}

	fmt.Println(nodeTarget.value, " ")
	if nodeTarget.left != nil {
		traverse(nodeTarget.left)
	}
}

func main() {
	var keys2 []byte
	keys2 = append(keys2, keys[1:3]...)

	var nibbles = make([]byte, 20)
	nibbles[19] = 'a'

	fmt.Println("keys:", keys2[:2], " nibble:", nibbles)

	addNode(root, 1)
	addNode(root, 2)
	addNode(root, 3)
	addNode(root, 4)
	addNode(root, 4)

	traverse(root)
}
