package main

import (
	"fmt"
	"log"
	"sync"
	"time"

	"github.com/ethereum/go-ethereum/p2p"
)

const (
	msgTalk   = 0
	msgLength = iota
)

const timestamp_format = "2006-01-02 15:04:05"

type Emitter struct {
	self  string
	peers map[string]*peer

	sendMsgCh chan string

	sync.Mutex
}

func NewEmitter() *Emitter {
	return &Emitter{
		peers:     make(map[string]*peer),
		sendMsgCh: make(chan string),
	}
}

func (e *Emitter) addPeer(p *p2p.Peer, ws p2p.MsgReadWriter) *peer {
	e.Lock()
	defer e.Unlock()
	id := fmt.Sprintf("%x", p.ID().String()[:8])
	newPeer := NewPeer(p, ws)
	e.peers[id] = newPeer
	log.Println("add new peer: ", p.ID().String())
	return newPeer
}

func (e *Emitter) broadcastNewMsg(msg string) {
	for _, p := range e.peers {
		if err := p2p.SendItems(p.ws, msgTalk, msg); err != nil {
			log.Println("Emitter.loopSendMsg p2p.SendItems err", err, "peer id", p.ID())
			continue
		}
	}
}

func (e *Emitter) loop() {
	for {
		select {
		case msgToSend := <-e.sendMsgCh:
			now := time.Now().Format(timestamp_format)
			e.broadcastNewMsg(now + " " + msgToSend)
		}
	}
}

func (e *Emitter) msgHandler(peer *p2p.Peer, ws p2p.MsgReadWriter) error {
	p := e.addPeer(peer, ws)
	if p == nil {
		log.Println("Add peer failed")
		return ErrAddPeer
	}

	peerID := peer.ID().String()
	for {
		msg, err := ws.ReadMsg()
		if err != nil {
			return err
		}

		switch msg.Code {
		case msgTalk:
			var myMessage []string
			if err := msg.Decode(&myMessage); err != nil {
				log.Println("decode msg err", err)
			} else {
				if currentMode != ChatMode {
					p.unread = append(p.unread, myMessage[0])
				} else {
					for unreadMsg := range p.unread {
						fmt.Printf("\nmsg from %s:\"%s\"\n", peerID, unreadMsg)
					}
					fmt.Printf("\nmsg from %s: \"%s\"\n", peerID, myMessage[0])
				}
			}
		default:
			log.Println("unknown msg code:", msg.Code)
		}
	}
	return nil
}

func (e *Emitter) Protocol() p2p.Protocol {
	return p2p.Protocol{
		Name:    "rad",
		Version: 1,
		Length:  msgLength,
		Run:     e.msgHandler,
	}
}
