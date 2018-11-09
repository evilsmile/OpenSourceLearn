package main

import (
	"fmt"
)

type Commander struct {
	cmdCh   chan string
	emitter *Emitter
}

func NewCommander(emitter *Emitter) *Commander {
	return &Commander{
		emitter: emitter,
		cmdCh:   make(chan string),
	}
}

func (c *Commander) handleCmd(input string) {
	switch input {
	case "list":
		fmt.Println("============ Peer List ===========")
		for _, peer := range c.emitter.peers {
			fmt.Println(peer.Info().Enode)
		}
	default:
		fmt.Println("Support Command: [ list ]")
	}
}

func (c *Commander) loop() {
	for {
		select {
		case cmd := <-c.cmdCh:
			c.handleCmd(cmd)
		}
	}
}
