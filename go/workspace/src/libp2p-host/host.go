// For most applications, the host is the basic building block you'll need to get started.
// This guide shows you how to construct and use a simple host
//
// The host is an abstraction that manages services on top of a swarm. It provides a clean
// interface to connect to a service on a given remote peer.

package main

import (
	"context"
	"crypto/rand"
	"fmt"

	"gx/ipfs/QmNiJiXwWE3kRhZrC5ej3kSjWHm337pYfhjLGSCDNKJP2s/go-libp2p-crypto"
	"gx/ipfs/QmVvV8JQmmqPCwXAaesWJPheUiEFQJ9HWRhWhuFuxVQxpR/go-libp2p"
)

func main() {

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	h, err := libp2p.New(ctx)
	if err != nil {
		panic(err)
	}

	fmt.Printf("Hello world! My host Id is %s. I have random addr port\n", h.ID())

	for i, a := range h.Addrs() {
		fmt.Printf("addr[%d]: %s\n", i, a.String())
	}

	priv, _, err := crypto.GenerateEd25519Key(rand.Reader)
	if err != nil {
		panic(err)
	}

	h2, err := libp2p.New(ctx, libp2p.Identity(priv), libp2p.ListenAddrStrings("/ip4/0.0.0.0/tcp/9000"))
	if err != nil {
		panic(err)
	}

	fmt.Printf("\nMy second host Id is %s\n", h2.ID())

	for i, a := range h2.Addrs() {
		fmt.Printf("addr[%d]: %s\n", i, a.String())
	}

	<-make(chan struct{})
}
