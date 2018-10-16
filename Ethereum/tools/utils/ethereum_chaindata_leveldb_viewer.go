package main

import (
	"bytes"
	"flag"
	"fmt"
	"strings"

	"github.com/syndtr/goleveldb/leveldb"
)

var (
	data_type string
	hash      string
	number    string

	dbpath string
)

func init() {
	flag.StringVar(&data_type, "prefix", "", "data data_type")
	flag.StringVar(&hash, "hash", "", "given block hash")
	flag.StringVar(&number, "number", "", "given block number")
	flag.StringVar(&dbpath, "dbpath", "", "leveldb path")
}

func _toHex(b byte) byte {
	if b >= 0x0 && b <= 9 {
		b += 0x30
	} else if b >= 10 && b <= 15 {
		b += ('a' - 10)
	}
	return b
}

func toHex(bytes []byte) []byte {

	var str []byte
	for _, b := range bytes {
		str = append(str, _toHex((b&0xF0)>>4))
		str = append(str, _toHex(b&0x0F))
	}
	return str
}

func _fromBin(b byte) byte {
	if b >= '0' && b <= '9' {
		b -= '0'
	} else if b >= 'a' && b <= 'f' {
		b -= 'a'
		b += 0xa
	}
	return b
}

func toBin(bytes []byte) []byte {
	var bin []byte
	for i := 0; i < len(bytes); i += 2 {
		bin = append(bin, _fromBin(bytes[i])&0xFF<<4|_fromBin(bytes[i+1]&0xFF))
	}
	return bin
}

func parseKey(hexkey string) (string, bool) {
	hexkeylen := len(hexkey)
	// need hex value in data cause it's a string
	hex_value := false
	newkey := hexkey
	handled := true
	if hexkeylen == 30 && string(toBin([]byte(hexkey[:]))) == "DatabaseVersion" {
		newkey = "DatabaseVersion"
	} else if hexkeylen == 20 && string(toBin([]byte(hexkey[:]))) == "LastHeader" {
		newkey = "LastHeader"
	} else if hexkeylen == 18 && string(toBin([]byte(hexkey[:]))) == "LastBlock" {
		newkey = "LastBlock"
	} else if hexkeylen == 16 && string(toBin([]byte(hexkey[:]))) == "LastFast" {
		newkey = "LastFast"
	} else if hexkeylen == 16 && string(toBin([]byte(hexkey[:]))) == "TrieSync" {
		newkey = "TrieSync"
	} else if hexkeylen >= 22 && string(toBin([]byte(hexkey[:22]))) == "secure-key-" {
		newkey = "secure-key-" + hexkey[22:]
	} else if hexkeylen >= 32 && string(toBin([]byte(hexkey[:32]))) == "ethereum-config-" {
		hex_value = true
		newkey = "ethereum-config-" + hexkey[32:]
	} else if hexkeylen >= 4 && string(toBin([]byte(hexkey[:4]))) == "iB" {
		if string(toBin([]byte(hexkey[4:14]))) == "count" {
			newkey = "iBcount(valid sections)"
		} else if string(toBin([]byte(hexkey[4:14]))) == "shead" {
			newkey = "iBshead(last block hash of a processed section)" + hexkey[14:]
		}
	} else {
		handled = false
	}

	if handled {
		return newkey, hex_value
	}

	header1 := string(toBin([]byte(hexkey[0:2])))
	if header1 == "h" {
		if len(hexkey) == 20 {
			newkey = "h(eaderPrefix)-hash:" + hexkey[2:18] + "-(T)d"
		} else {
			newkey = "h(eaderPrefix)-number:" + hexkey[2:18] + "-hash:" + hexkey[18:]
		}
	} else if header1 == "H" {
		newkey = "H(eaderNumPrefix)-number:" + hexkey[2:]
	} else if header1 == "b" {
		newkey = "b(lockBodyPrefix)-number:" + hexkey[2:18] + "-hash:" + hexkey[18:]
	} else if header1 == "r" {
		newkey = "r(eciptsPrefix)-number:" + hexkey[2:18] + "-hash:" + hexkey[18:]
	} else if header1 == "l" {
		newkey = "(tx)l(ookupPrefix)-hash:" + hexkey[2:]
	} else if header1 == "B" {
		newkey = "B(loombitsPrefix)-bit:" + hexkey[2:6] + "-section:" + hexkey[6:22] + "-hash:" + hexkey[22:]
	}
	return newkey, hex_value
}

func readRecord(db *leveldb.DB) {

	prefix := ""
	if data_type != "" {
		var prefixBuf bytes.Buffer

		prefixBuf.Write(toHex([]byte(data_type)))

		if number != "" {
			number_b := []byte(number)
			for len(number_b) < 16 {
				number_b = append([]byte{'0'}, number_b...)
			}
			prefixBuf.Write(number_b)
		}

		if hash != "" {
			prefixBuf.WriteString(hash)
		}

		prefix = prefixBuf.String()
	}

	iter := db.NewIterator(nil, nil)
	defer iter.Release()
	for iter.Next() {
		hexkey := string(toHex(iter.Key()))

		if prefix != "" {
			if strings.HasPrefix(hexkey, prefix) {
				value_b := toHex(iter.Value())
				key, hex_value := parseKey(hexkey)
				if !hex_value {
					fmt.Printf("Key[%v] ===> Value[%v]\n\n", key, string(value_b))
				} else {
					fmt.Printf("Key[%v] ===> Value[%v]\n\n", key, string(toBin(value_b)))
				}
			}
		} else if hash != "" {
			if strings.Contains(hexkey, hash) {
				value_b := toHex(iter.Value())
				key, hex_value := parseKey(hexkey)
				if !hex_value {
					fmt.Printf("Key[%v] ===> Value[%v]\n\n", key, string(value_b))
				} else {
					fmt.Printf("Key[%v] ===> Value[%v]\n\n", key, string(toBin(value_b)))
				}
			}
		}
	}
}

func main() {
	flag.Parse()

	if dbpath == "" {
		fmt.Printf("Error: empty path\n")
		return
	}

	db, err := leveldb.OpenFile(dbpath, nil)
	if err != nil {
		fmt.Printf("ERROR: open db [%s], error=[%v]\n", dbpath, err)
		return
	}
	defer db.Close()

	readRecord(db)
}
