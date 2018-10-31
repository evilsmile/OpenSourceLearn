package main

import (
	"fmt"
	"os"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/p2p"
)

const messageId = 0

type Message string

func MyProtocol() p2p.Procotol {
	return p2p.Procotol{
		Name:    "MyProtocol",
		Version: 1,
		Length:  1,
		Run:     msgHandler,
	}
}

func main() {
	nodekey, _ := crypto.GenerateKey()
	srv := p2p.Server{
		MaxPeers:   10,
		PrivateKey: nodekey,
		Name:       "my node",
		ListenAddr: ":30303",
		Protocols:  []p2p.Procotol{MyProtocol()},
	}

	if err := srv.Start(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	select {}
}

func msgHandler(peer *p2p.Peer, ws p2p.MsgReadWriter) error {
	for {
		msg, err := ws.ReadMsg()
		if err != nil {
			return err
		}

		var myMessage Message
		err = msg.Decode(&myMessage)
		if err != nil {
			continue
		}

		switch myMessage {
		case "foo":
			err := p2p.SendItems(ws, messageId, "bar")
			if err != nil {
				return err
			}
		default:
			fmt.Println("recv:", myMessage)
		}
	}
	return nil
}
