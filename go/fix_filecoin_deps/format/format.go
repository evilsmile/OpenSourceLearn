package format

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"strings"
)

type HashNameGetter interface {
	GetHashName() (string, string, error)
}

// ----------------------------   Format 1 --------------------------
// file content :
// ./QmVPeMNK9DfG.....tYtWrNdcW/go-blockservice/package.json:12:      "hash": "QmXRp....pA2NrbNaY3DU1Y5K",
// ./QmVPeMNK9DfG.....tYtWrNdcW/go-blockservice/package.json-13-      "name": "go-bitswap",
type Format1Getter struct {
	fName string
	r     *bufio.Reader
}

func NewFormat1Getter(fName string) *Format1Getter {
	return &Format1Getter{fName: fName}
}

func (f1 *Format1Getter) GetHashName() (hash, name string, err error) {
	if f1.r == nil {
		f, err := os.Open(f1.fName)
		if err != nil {
			log.Fatalln("Error open file: ", err)
		}
		f1.r = bufio.NewReader(f)
	}

	if hash, err = extractFieldOfLine("hash", f1.r); err != nil {
		return
	}

	if name, err = extractFieldOfLine("name", f1.r); err != nil {
		return
	}

	return
}

// ----------------------------   Format 2 --------------------------
// file content :
// [get ] [fetch]         QmYrWiWM4qtrnCeT3R14jY3ZZyirDNJgwK57q4qFYePgbd go-libp2p-host
type Format2Getter struct {
	fName  string
	r      *bufio.Reader
	lineno uint
}

func NewFormat2Getter(fName string) *Format2Getter {
	return &Format2Getter{fName: fName}
}

func (f2 *Format2Getter) GetHashName() (hash, name string, err error) {
	if f2.r == nil {
		f, err := os.Open(f2.fName)
		if err != nil {
			log.Fatalln("Error open file: ", err)
		}
		f2.r = bufio.NewReader(f)
	}

	line, err := f2.r.ReadBytes('\n')
	if err != nil {
		return
	}
	f2.lineno++
	line = line[:len(line)-1]
	splited := strings.Split(string(line), " ")
	if len(splited) < 5 {
		err = errors.New(fmt.Sprintf("Invalid size: %d of line:[%d] '%s'", len(splited), f2.lineno, line))
		return
	}

	return splited[3], splited[4], nil
}

// ----------------------- Format END -----------------------------

func extractFieldOfLine(fieldName string, r *bufio.Reader) (string, error) {
	line, err := r.ReadBytes('\n')
	if err != nil {
		return "", err
	}
	splited := strings.Split(string(line), "\"")
	if len(splited) < 4 {
		return "", errors.New("Invalid size")
	}

	if splited[1] != fieldName {
		return "", errors.New(fmt.Sprintf("Field '%s' expected!! But get '%s'", fieldName, splited[1]))
	}
	return splited[3], nil

}
