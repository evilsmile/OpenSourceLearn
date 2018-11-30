///////////////////////////////////////////////////////////////////////////
// This example shows hwo to use multicodecs (i.e. protobufs) to encode  //
// and transmit information between libp2p hosts using libp2p streams.   //
// Multicodes present a common interface, makeing it very easy to swap   //
// the codec implementation if needed.                                   //
///////////////////////////////////////////////////////////////////////////
package main

import (
	"context"
	"fmt"
	"log"
	"math/rand"

	crypto "gx/ipfs/QmNiJiXwWE3kRhZrC5ej3kSjWHm337pYfhjLGSCDNKJP2s/go-libp2p-crypto"
	ma "gx/ipfs/QmRKLtwMw131aK7ugC3G7ybpumMz78YrJe5dzneyindvG1/go-multiaddr"
	ps "gx/ipfs/QmUymf8fJtideyv3z727BcZUifGBjMZMpCJqu3Gxk5aRUk/go-libp2p-peerstore"
	libp2p "gx/ipfs/QmXnpYYg2onGLXVxM4Q5PEFcx29k8zeJQkPeLAk9h9naxg/go-libp2p"
	gologging "gx/ipfs/QmcaSwFc5RBg8yCq54QURwEU4nwjfCpjbpmaAm4VbdGLKv/go-logging"
	golog "gx/ipfs/QmcuXC5cxs79ro2cUuHs4HQ2bkDLJUYokwL8aivcX6HW3C/go-log"
)

func makeRandomHost(port int, done chan bool) *Node {
	priv, _, _ := crypto.GenerateKeyPair(crypto.Secp256k1, 256)
	listen, _ := ma.NewMultiaddr(fmt.Sprintf("/ip4/127.0.0.1/tcp/%d", port))
	host, _ := libp2p.New(context.Background(), libp2p.ListenAddrs(listen), libp2p.Identity(priv))

	return NewNode(host, done)
}

func main() {
	golog.SetAllLoggers(gologging.DEBUG)

	rand.Seed(666)
	port1 := rand.Intn(100) + 10000
	port2 := port1 + 1

	done := make(chan bool)

	h1 := makeRandomHost(port1, done)
	h2 := makeRandomHost(port2, done)

	h1.Peerstore().AddAddrs(h2.ID(), h2.Addrs(), ps.PermanentAddrTTL)
	h2.Peerstore().AddAddrs(h1.ID(), h1.Addrs(), ps.PermanentAddrTTL)

	log.Printf("This is a covnersion between %s and %s\n", h1.ID(), h2.ID())

	h1.Ping(h2.Host)
	h2.Ping(h1.Host)
	h1.Echo(h2.Host)
	h2.Echo(h1.Host)

	for i := 0; i < 4; i++ {
		<-done
	}
}
