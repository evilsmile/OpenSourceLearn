package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"time"

	cid "gx/ipfs/QmR8BauakNcBa3RbE4nbQu76PDiJgoQgz8AJdhJuiU4TAw/go-cid"
	inet "gx/ipfs/QmRKbEchaYADxSCyyjhDh4cTrUby8ftXUb8MRLBTHQYupw/go-libp2p-net"
	iaddr "gx/ipfs/QmUSE3APe1pMFVsUBZUZaKQKERiPteCWvTAERtVQmtXzgE/go-ipfs-addr"
	pstore "gx/ipfs/QmUymf8fJtideyv3z727BcZUifGBjMZMpCJqu3Gxk5aRUk/go-libp2p-peerstore"
	libp2p "gx/ipfs/QmVvV8JQmmqPCwXAaesWJPheUiEFQJ9HWRhWhuFuxVQxpR/go-libp2p"
	dht "gx/ipfs/QmadRyQYRn64xHb5HKy2jRFp2Der643Cgo7NEjFgs4MX2k/go-libp2p-kad-dht"
	mh "gx/ipfs/QmerPMzPk1mJVowm8KgmoknWa4yCYvvugMPsgWmDNUvDLW/go-multihash"
)

// IPFS bootstrp nodes. Used to find other peers in the network
var bootstrapPeers = []string{
	"/ip4/104.131.131.82/tcp/4001/ipfs/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ",
	"/ip4/104.236.179.241/tcp/4001/ipfs/QmSoLPppuBtQSGwKDZT2M73ULpjvfd3aZ6ha4oFGL1KrGM",
	"/ip4/104.236.76.40/tcp/4001/ipfs/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64",
	"/ip4/128.199.219.111/tcp/4001/ipfs/QmSoLSafTMBsPKadTEgaXctDQVcqN88CNLHXMkTNwMKPnu",
	"/ip4/178.62.158.247/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd",
}

var rendezvous = "meet _evilsmile_ here"

func handleStream(stream inet.Stream) {
	log.Println("Got a new stream!")

	// Create a buffer stream for non blocking read and write
	rw := bufio.NewReadWriter(bufio.NewReader(stream), bufio.NewWriter(stream))

	go readData(rw)
	go writeData(rw)

	// 'stream' will stay open util you close it
}

func readData(rw *bufio.ReadWriter) {
	for {
		str, _ := rw.ReadString('\n')

		if str == "" {
			log.Println("empty msg")
			return
		}
		if str != "\n" {
			// Green console color: \x1b[32m
			// Reset console color: \x1b[0m
			fmt.Printf("\x1b[32m%s\x1b[0m> ", str)
		}
	}
}

func writeData(rw *bufio.ReadWriter) {
	stdReader := bufio.NewReader(os.Stdin)

	for {
		fmt.Println("> ")
		sendData, err := stdReader.ReadString('\n')

		if err != nil {
			panic(err)
		}

		rw.WriteString(fmt.Sprintf("%s\n", sendData))
		rw.Flush()
	}
}

func main() {

	help := flag.Bool("h", false, "Display help")
	rendezvousString := flag.String("r", rendezvous, "Unique string to identify group of nodes. Share this with your friends to your friends to let them connect with you")
	flag.Parse()

	if *help {
		fmt.Printf("This program demonstrate a simple p2p chat application using libp2p\n\n")
		fmt.Printf("Usage: Run './chat in two different terminals. Let them connect to the bootstrap nodes, announce themselves and connect to the peers\n")

		os.Exit(0)
	}

	ctx := context.Background()

	/////// ================================= 1. START a P2P Host ==================================

	// libp2p.New constructs a new libp2p Host. It's the constructor for libp2p node.
	// It creates a host with given configuration. Right now, all the operation are default.
	// Other options can be added here
	host, err := libp2p.New(ctx)
	if err != nil {
		panic(err)
	}

	/////// ======================== 2. Set default handler for incoming connections ================

	// Set stream handler. It's called when a peer initiate a connection and starts a stream with this peer
	host.SetStreamHandler("/chat/1.0.0", handleStream)

	/////// ===================== 3. Initiate a new DHT client with 'host' as Local Peer ============

	kadDht, err := dht.New(ctx, host)
	if err != nil {
		panic(err)
	}

	// ======================= 4. Connect to IPFS bootstrap nodes ==============================

	// First connect to the bootstrap nodes first. They will tell us about the other nodes in the network
	for _, peerAddr := range bootstrapPeers {
		addr, _ := iaddr.ParseString(peerAddr)
		peerinfo, _ := pstore.InfoFromP2pAddr(addr.Multiaddr())

		if err := host.Connect(ctx, *peerinfo); err != nil {
			fmt.Println(err)
		} else {
			fmt.Println("Connection establieshed with bootstrap node: ", *peerinfo)
		}
	}

	/////// ======================= 5. Announce your presence using a rendezvous point ==============

	// Use a rendezvous point to announce our location.
	// This is like telling your friends to meet you at the Eiffel tower
	v1b := cid.V1Builder{Codec: cid.Raw, MhType: mh.SHA2_256}
	rendezvousPoint, _ := v1b.Sum([]byte(*rendezvousString))
	fmt.Printf("our rendezvousPoint is [%s]\n", rendezvousPoint)

	fmt.Println("announcing ourselves .... ")
	tctx, cancel := context.WithTimeout(ctx, time.Second*10)
	defer cancel()
	if err := kadDht.Provide(tctx, rendezvousPoint, true); err != nil {
		panic(err)
	}

	//////// ================================= 6. Find peers nearby ==================================

	//NOTE: Although 'dht.Provide' and 'dht.FindProviders' works for a rendezvous peer discovery, this
	//      is not the right way of doing it. Libp2p is currently working on an actual rendezvous protocol
	//      which can be used for bootstrap purpose, real time peer discovery and application specific routing.

	// Now look for others who have announced
	// This is like your friends telling you the location to meet you
	fmt.Println("searching for other peers...")
	tctx, cancel = context.WithTimeout(ctx, time.Second*10)
	defer cancel()
	// 'FindProviders' will return 'PeerInfo' of all the peers which
	// have 'Provide' or announed themselves previously
	peers, err := kadDht.FindProviders(tctx, rendezvousPoint)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Found %d peers!\n", len(peers))

	for _, p := range peers {
		fmt.Println("Peer: ", p)
	}

	/////// ================================= 7. Open streams to peers found ========================
	for _, p := range peers {
		if p.ID == host.ID() || len(p.Addrs) == 0 {
			continue
		}

		stream, err := host.NewStream(ctx, p.ID, "/chat/1.0.0")

		if err != nil {
			fmt.Println(err)
		} else {
			rw := bufio.NewReadWriter(bufio.NewReader(stream), bufio.NewWriter(stream))

			go writeData(rw)
			go readData(rw)
		}

		fmt.Println("Connected to: ", p)
	}

	select {}
}
