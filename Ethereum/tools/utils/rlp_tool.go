package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"

	"github.com/ethereum/go-ethereum/core/rawdb"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/rlp"
)

var (
	body_data     string
	head_data     string
	receipts_data string
	txlookup_data string
)

func init() {
	flag.StringVar(&body_data, "body", "", "rlp body data")
	flag.StringVar(&head_data, "head", "", "rlp head data")
	flag.StringVar(&receipts_data, "receipt", "", "rlp receipt data")
	flag.StringVar(&txlookup_data, "txlookup", "", "rlp txlookup data")
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

func parseBody(data_b []byte) {
	body := new(types.Body)
	if err := rlp.Decode(bytes.NewReader(data_b), body); err != nil {
		log.Println("Invalid block body RLP", "err", err)
		return
	}
	log.Printf("block body RLP decode succ: tx_len[%v] uncle_len[%v]", len(body.Transactions), len(body.Uncles))

	var signer types.Signer = types.FrontierSigner{}
	for i, tx := range body.Transactions {
		if tx.Protected() {
			signer = types.NewEIP155Signer(tx.ChainId())
		}
		from, _ := types.Sender(signer, tx)
		json, err := tx.MarshalJSON()
		if err != nil {
			log.Printf("error marshal json")
			continue
		}
		log.Printf("====> tx[%v] from: %v, %v ", i, string(toHex(from.Bytes())), string(json))
	}
}

func parseHead(head_b []byte) {

	header := new(types.Header)
	if err := rlp.Decode(bytes.NewReader(head_b), header); err != nil {
		log.Println("Invalid block header RLP", "err", err)
		return
	}
	log.Printf("block head RLP decode succ")

	json, err := header.MarshalJSON()
	if err != nil {
		log.Println("header marshal error")
		return
	}
	fmt.Printf("header json: %v", string(json))
}

func parseReceipts(receipts_b []byte) {

	storageReceipts := []*types.ReceiptForStorage{}
	if err := rlp.DecodeBytes(receipts_b, &storageReceipts); err != nil {
		log.Println("Invalid receipt array RLP", "err", err)
		return
	}
	receipts := make(types.Receipts, len(storageReceipts))
	for i, receipt := range storageReceipts {
		receipts[i] = (*types.Receipt)(receipt)
		json, err := receipts[i].MarshalJSON()
		if err != nil {
			log.Println("receipts[%v] marshal error", i)
			continue
		}
		fmt.Printf("receipt json[%v]: %v\n", i, string(json))
	}
}

func parseTxLookup(txlookup_b []byte) {
	var entry rawdb.TxLookupEntry
	if err := rlp.DecodeBytes(txlookup_b, &entry); err != nil {
		log.Println("Invalid transaction lookup entry RLP", "err", err)
		return
	}
	fmt.Println(string(toHex(entry.BlockHash.Bytes())), entry.BlockIndex, entry.Index)
}

func main() {
	flag.Parse()

	if body_data != "" {
		parseBody(toBin([]byte(body_data)))
	}

	if head_data != "" {
		parseHead(toBin([]byte(head_data)))
	}

	if receipts_data != "" {
		parseReceipts(toBin([]byte(receipts_data)))
	}

	if txlookup_data != "" {
		parseTxLookup(toBin([]byte(txlookup_data)))
	}
}
