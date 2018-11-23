package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	tmpDir, err := ioutil.TempDir("", "ioutil.tmp")
	if err != nil {
		fmt.Println("err: ", err)
		return
	}
	defer os.RemoveAll(tmpDir)

	fmt.Println("Create temp dir: ", tmpDir)
}
