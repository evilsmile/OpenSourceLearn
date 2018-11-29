package main

import (
	"bufio"
	"context"
	"crypto/rand"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	mrand "math/rand"
	"strings"

	crypto "gx/ipfs/QmNiJiXwWE3kRhZrC5ej3kSjWHm337pYfhjLGSCDNKJP2s/go-libp2p-crypto"
	ma "gx/ipfs/QmRKLtwMw131aK7ugC3G7ybpumMz78YrJe5dzneyindvG1/go-multiaddr"
	net "gx/ipfs/QmRKbEchaYADxSCyyjhDh4cTrUby8ftXUb8MRLBTHQYupw/go-libp2p-net"
	pstore "gx/ipfs/QmUymf8fJtideyv3z727BcZUifGBjMZMpCJqu3Gxk5aRUk/go-libp2p-peerstore"
	host "gx/ipfs/QmVrjR2KMe57y4YyfHdYa3yKD278gN8W7CTiqSuYmxjA7F/go-libp2p-host"
	libp2p "gx/ipfs/QmXnpYYg2onGLXVxM4Q5PEFcx29k8zeJQkPeLAk9h9naxg/go-libp2p"
	gologging "gx/ipfs/QmcaSwFc5RBg8yCq54QURwEU4nwjfCpjbpmaAm4VbdGLKv/go-logging"
	peer "gx/ipfs/QmcqU6QUDSXprb1518vYDGczrTJTyGwLG9eUa5iNX4xUtS/go-libp2p-peer"
	golog "gx/ipfs/QmcuXC5cxs79ro2cUuHs4HQ2bkDLJUYokwL8aivcX6HW3C/go-log"
)

/// It creates a BasicHost object, whic wraps go-libp2p-swarm and should be used preferentially.
/// A go-libp2p-swarm Network is a 'swarm' which complies to the go-libp2p-net Network interface,
/// and takes care of maintaining streams, connections, multiplexing different protocols on them,
/// handling incoming connections etc.
func makeBasicHost(listenPort int, secio bool, randseed int64) (host.Host, error) {

	var r io.Reader
	if randseed == 0 {
		r = rand.Reader
	} else {
		r = mrand.New(mrand.NewSource(randseed))
	}

	priv, _, err := crypto.GenerateKeyPairWithReader(crypto.RSA, 2048, r)
	if err != nil {
		return nil, err
	}

	// To create a 'swarm' (BasicHost), we need an <ipfs-protocol ID> like 'QmNt...xc'
	// We generate a key pair on every run and uses and ID extracted from the public key.
	// We also need a <multiaddress> indicating how to reach this peer.
	opts := []libp2p.Option{
		libp2p.ListenAddrStrings(fmt.Sprintf("/ip4/127.0.0.1/tcp/%d", listenPort)),
		libp2p.Identity(priv),
	}

	if !secio {
		opts = append(opts, libp2p.NoSecurity)
	}

	basicHost, err := libp2p.New(context.Background(), opts...)
	if err != nil {
		return nil, err
	}

	hostAddr, _ := ma.NewMultiaddr(fmt.Sprintf("/ipfs/%s", basicHost.ID().Pretty()))

	var addr ma.Multiaddr
	for _, a := range basicHost.Addrs() {
		if strings.Contains(a.String(), "p2p-circuit") {
			continue
		}
		addr = a
		break
	}

	// concat addr + hostAddr
	fullAddr := addr.Encapsulate(hostAddr)
	log.Printf("I am %s\n", fullAddr)
	if secio {
		log.Printf("Now run \"./echo -l %d -d %s -secio\" on a different terminal\n", listenPort+1, fullAddr)
	} else {
		log.Printf("Now run \"./echo -l %d -d %s \" on a different terminal\n", listenPort+1, fullAddr)
	}

	return basicHost, nil
}

func main() {
	// libp2p code uses golog to log messages. They log with different string IDs (i.e. "swarm").
	// We can control the verbosity level for all loggers
	golog.SetAllLoggers(gologging.INFO)

	listenF := flag.Int("l", 0, "wait for incoming connections")
	target := flag.String("d", "", "target peer to dial")
	secio := flag.Bool("secio", false, "enable secio")
	seed := flag.Int64("seed", 0, "set random seed for id generation")
	flag.Parse()

	if *listenF == 0 {
		log.Fatal("Please provide a port to bind on with -l")
	}

	ha, err := makeBasicHost(*listenF, *secio, *seed)
	if err != nil {
		panic(err)
	}

	ha.SetStreamHandler("/evilecho/1.0.10", func(s net.Stream) {
		log.Println("Got a new stream")
		if err := doEcho(s); err != nil {
			log.Println(err)
			s.Reset()
		} else {
			s.Close()
		}
	})

	if *target == "" {
		log.Println("listening for connections")
		select {}
	}

	ipfsaddr, err := ma.NewMultiaddr(*target)
	if err != nil {
		panic(err)
	}

	pid, err := ipfsaddr.ValueForProtocol(ma.P_IPFS)
	if err != nil {
		panic(err)
	}
	peerid, err := peer.IDB58Decode(pid)
	if err != nil {
		panic(err)
	}

	// Decapsulate the /ipfs/<peerID> part from the target
	// /ip4/<a.b.c.d>/ipfs/<peer> becomes /ip4/<a.b.c.d>
	targetPeerAddr, _ := ma.NewMultiaddr(fmt.Sprintf("/ipfs/%s", peer.IDB58Encode(peerid)))
	targetAddr := ipfsaddr.Decapsulate(targetPeerAddr)

	// go-libp2p-peerstore, which is used as a address book which matches node IDs to the multiaddress
	// through which they can be contacted. This peerstore gets autopopulated when manually opening a
	// connection (with 'Connect()', or manually 'AddAddr()')
	ha.Peerstore().AddAddr(peerid, targetAddr, pstore.PermanentAddrTTL)

	log.Println("opening strewam")

	// a BasicHost can now open streams (bi-directional channel between to peers) using 'NewStream'
	// and use them to send and receive data tagged with a 'Protocol.ID'. The host can also listen for
	// incoming connections for a given 'Protocol' with 'SetStreamHandler()'
	s, err := ha.NewStream(context.Background(), peerid, "/evilecho/1.0.10")
	if err != nil {
		panic(err)
	}

	_, err = s.Write([]byte("Hello, world!\n"))
	if err != nil {
		panic(err)
	}

	out, err := ioutil.ReadAll(s)
	if err != nil {
		panic(err)
	}

	log.Printf("read reply: %q\n", out)
}

func doEcho(s net.Stream) error {
	buf := bufio.NewReader(s)
	str, err := buf.ReadString('\n')
	if err != nil {
		return err
	}

	log.Printf("read: %s\n", str)
	_, err = s.Write([]byte(str))
	return err
}
