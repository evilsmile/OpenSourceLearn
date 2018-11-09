package main

import (
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
)

func main() {
	app := cli.NewApp()
	app.Usage = "p2p chatter by ethereum-p2p"
	app.Action = startP2PNode
	app.Flags = []cli.Flag{
		cli.IntFlag{Name: "port", Value: 11200, Usage: "listen port", Destination: &port},
		cli.StringFlag{Name: "bootnode", Value: "", Usage: "boot node", Destination: &bootnode},
	}

	if err := app.Run(os.Args); err != nil {
		log.Fatal(err)
	}
}

func startP2PNode(c *cli.Context) error {
	emitter := NewEmitter()
	nodeKey, _ := crypto.GenerateKey()
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
