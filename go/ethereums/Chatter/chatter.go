package main

import (
	"crypto/ecdsa"
	"fmt"
	"log"
	"os"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/p2p"
	"github.com/ethereum/go-ethereum/p2p/enode"
	"gopkg.in/urfave/cli.v1"
)

var (
	port     int
	bootnode string
	keyFile  string

	nodeKey *ecdsa.PrivateKey
)

const ()

func main() {
	app := cli.NewApp()
	app.Usage = "p2p chatter by ethereum-p2p"
	app.Action = startP2PNode
	app.Flags = []cli.Flag{
		cli.IntFlag{Name: "port", Value: 11200, Usage: "listen port", Destination: &port},
		cli.StringFlag{Name: "bootnode", Value: "", Usage: "boot node", Destination: &bootnode},
		cli.StringFlag{Name: "keyfile", Value: "nodekey", Usage: "key file", Destination: &keyFile},
	}

	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}

func getKey() error {
	var err error
	nodeKey, err = crypto.LoadECDSA(keyFile)
	if err == nil {
		return nil
	}

	nodeKey, err = crypto.GenerateKey()
	if err != nil {
		return err
	}

	crypto.SaveECDSA(keyFile, nodeKey)
	return nil
}

func startP2PNode(c *cli.Context) error {
	emitter := NewEmitter()
	if err := getKey(); err != nil {
		return err
	}
	node := p2p.Server{
		Config: p2p.Config{
			MaxPeers:   100,
			PrivateKey: nodeKey,
			Name:       "p2pChatter",
			ListenAddr: fmt.Sprintf(":%d", port),
			Protocols:  []p2p.Protocol{emitter.Protocol()},
		},
	}

	bootNode, err := enode.ParseV4(bootnode)
	if err != nil {
		return err
	}

	node.Config.BootstrapNodes = []*enode.Node{bootNode}

	if err := node.Start(); err != nil {
		return err
	}

	emitter.self = node.NodeInfo().ID[:8]

	commander := NewCommander(emitter)
	console := NewConsole(emitter, commander)

	pubAddr := crypto.FromECDSAPub(&nodeKey.PublicKey)[1:]
	log.Printf("Start chat with [%x@:%d]\n", pubAddr, port)
	go console.Start()
	select {}

	return nil
}
