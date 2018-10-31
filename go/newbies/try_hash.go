package main

import (
	"crypto/md5"
	"crypto/sha1"
	"crypto/sha256"
	"crypto/sha512"
	"flag"
	"fmt"
	"hash"
	"io"
	"os"
)

var style = flag.String("s", "sha256", "Use hash-functions: sha1, sha256")
var filename = flag.String("f", "", "file name to calc hash")

type hasher func(dst []byte, data []byte)

type newHash struct {
	hash.Hash
}

func (nh newHash) Read(out []byte) (n int, err error) {
	fmt.Println("Done")
	n = 0
	err = nil
	return
}

func makeHasher(h hash.Hash) hasher {
	type readerHash interface {
		hash.Hash
		Read([]byte) (int, error)
	}

	// h.(interface) 类型判断
	rh, ok := h.(readerHash)
	if !ok {
		panic("can't find Read method on hash")
	}

	outputLen := rh.Size()
	return func(dest []byte, data []byte) {
		rh.Reset()
		rh.Write(data)
		rh.Read(dest[:outputLen])
	}
}

func main() {
	flag.Parse()
	var hs hash.Hash
	switch *style {
	case "md5":
		hs = md5.New()
	case "sha1":
		hs = sha1.New()
	case "sha512":
		hs = sha512.New()
	default:
		hs = sha256.New()
	}

	if len(*filename) == 0 {
		filein, err := os.Open(flag.Args()[len(flag.Args())-1])
		if err != nil {
			return
		} else {
			io.Copy(hs, filein)
		}
	} else {
		filein, err := os.Open(*filename)
		if err != nil {
			return
		} else {
			io.Copy(hs, filein)
		}
	}
	hashString := hs.Sum(nil)
	fmt.Printf("%x\n", hashString)

	//	var nh newHash
	//	makeHasher(nh)
	//	fmt.Println("Done")
}
