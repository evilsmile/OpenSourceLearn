package main

import (
	"github.com/ethereum/go-ethereum/p2p"
)

type peer struct {
	*p2p.Peer
	ws     p2p.MsgReadWriter
	unread []string
}

func NewPeer(p *p2p.Peer, ws p2p.MsgReadWriter) *peer {
	return &peer{
		Peer: p,
		ws:   ws,
	}
}
