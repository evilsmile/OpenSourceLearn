//                                                                                                       XXX
//                                                                                                      XX  XXXXXX
//	   										                                                     X         XX
//                                                                                           XXXXXXX  XX          XX XXXXXXXXXX
//                        +----------------+                +-----------------+            XXX      XXX            XXX        XXX
//       HTTP Request     |                |                |                 |           XX                                    XX
//      +----------------->                | libp2p stream  |                 |  HTTP     X                                      X
//	                      |  Local peer    <---------------->  Remote peer    <------------->   HTTP SERVER - THE INTERNET      XX
//      <-----------------+                |                |                 | Req & Resp XX                                   X
//        HTTP Response   |  libp2p host   |                |  libp2p host    |             XXXX XXXX XXXXXXXXXXXXXXXXXXXX   XXXX
//                        +----------------+                +-----------------+                                          XXXXX
//    NOTE:
//      一个Local peer在9900端口监听HTTP请求。然后通过libp2p stream发送到 remote peer，做真正的HTTP发送。
//      remote peer收到请求后返回给remote peer，再返回流给local peer，递交给User
//

package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"strings"

	manet "gx/ipfs/QmQVUtnrNGtCRkCMpXgpApfzQjc8FDaDVxHqWH8cnZQeh5/go-multiaddr-net"
	ma "gx/ipfs/QmRKLtwMw131aK7ugC3G7ybpumMz78YrJe5dzneyindvG1/go-multiaddr"
	inet "gx/ipfs/QmRKbEchaYADxSCyyjhDh4cTrUby8ftXUb8MRLBTHQYupw/go-libp2p-net"
	ps "gx/ipfs/QmUymf8fJtideyv3z727BcZUifGBjMZMpCJqu3Gxk5aRUk/go-libp2p-peerstore"
	host "gx/ipfs/QmVrjR2KMe57y4YyfHdYa3yKD278gN8W7CTiqSuYmxjA7F/go-libp2p-host"
	libp2p "gx/ipfs/QmXnpYYg2onGLXVxM4Q5PEFcx29k8zeJQkPeLAk9h9naxg/go-libp2p"
	peer "gx/ipfs/QmcqU6QUDSXprb1518vYDGczrTJTyGwLG9eUa5iNX4xUtS/go-libp2p-peer"
)

// Protocol defiens the libp2p protocol that we will use for the libp2p proxy
// service that we are going to provide. This will tag the stream used for
// this service. Streams are multiplexed and their protocol tag helps
// libp2p handle them to the right handler function.
const Protocol = "/evil-proxy/0.0.100"

// create a libp2p host with a randomly generated identity.
func makeRandomHost(port int) host.Host {
	host, err := libp2p.New(context.Background(), libp2p.ListenAddrStrings(fmt.Sprintf("/ip4/127.0.0.1/tcp/%d", port)))
	if err != nil {
		log.Fatalln(err)
	}
	return host
}

// ProxyService provides HTTP proxying on top of libp2p by launching an
// HTTP server which tunnels the requests to a destination peer running
// ProxyService too
type ProxyService struct {
	host      host.Host
	dest      peer.ID
	proxyAddr ma.Multiaddr
}

// NewProxyService attaches a proxy service to the given libp2p host.
// The proxyAddr parameter specifies the address on which the HTTP proxy
// server listens. The dest parameter specifies the peer ID of the remote
// peer in charge of performing the HTTP requests.
//
// ProxyAddr/dest may be nil/"" it is not necessary that this host provides
// a listening HTTP server (and instead its only function is to perform the
// proxied http request it receives from a different peer.
//
// The addresses for the dest peer should be part of the host's peerstore.
func NewProxyService(h host.Host, proxyAddr ma.Multiaddr, dest peer.ID) *ProxyService {
	h.SetStreamHandler(Protocol, streamHandler)

	fmt.Println("Proxy server is ready")
	fmt.Println("libp2p-peer addresses:")
	for _, a := range h.Addrs() {
		fmt.Printf("%s/ipfs/%s\n", a, peer.IDB58Encode(h.ID()))
	}

	return &ProxyService{
		host:      h,
		dest:      dest,
		proxyAddr: proxyAddr,
	}
}

// streamHandler is our function to handle any libp2p-net streams that belong to
// our protocol. The streams should contain an HTTP request which we need to parse,
// make on behalf of the original node, and then write the response on the stream,
// before closing it.
// 从 ServeHTTP 收到请求，转换成http请求，发送出去，然后直接把返回写给 Peer
func streamHandler(stream inet.Stream) {
	// Remember to close the stream when we are done
	defer stream.Close()

	// Create a new buffered reader, as ReadRequest needs one.
	// The buffered reader reads from our stream, on which we
	// have sent the HTTP request ( see ServeHTTP())
	buf := bufio.NewReader(stream)
	// Read the HTTP request from the buffer
	req, err := http.ReadRequest(buf)
	if err != nil {
		stream.Reset()
		log.Println(err)
		return
	}
	defer req.Body.Close()

	// We need to reset these fields in the request URL as they are not maintained.
	req.URL.Scheme = "http"
	hp := strings.Split(req.Host, ":")
	if len(hp) > 1 && hp[1] == "443" {
		req.URL.Scheme = "https"
	} else {
		req.URL.Scheme = "http"
	}

	req.URL.Host = req.Host

	outreq := new(http.Request)
	*outreq = *req

	// We now make the request
	fmt.Printf("Making request to %s\n", req.URL)
	resp, err := http.DefaultTransport.RoundTrip(outreq)
	if err != nil {
		stream.Reset()
		log.Println(err)
		return
	}

	resp.Write(stream)
}

// Serve listens on the ProxyService's proxy address. This effectively allows to
// set the listening address as http proxy.
func (p *ProxyService) Serve() {
	// 从 /ip4/127.0.0.1/tcp/9900 中取出 127.0.0.1:9900 作为监听地址,传给http.ListenAndServe。
	// 收到的请求由 ServeHTTP 处理。
	_, serveArgs, _ := manet.DialArgs(p.proxyAddr)
	fmt.Println("proxy listening on ", serveArgs)
	if p.dest != "" {
		http.ListenAndServe(serveArgs, p)
	}
}

// ServeHTTP implements the http.Handler interface.
// WARNING: this is the simplest approach to a proxy. Therefore we do not do
// any of the things that should be done when implementing a reverse proxy,
// like handling headers correctly.
//
// ServeHTTP opens a stream to the dest peer for every HTTP request.
// Streams are multiplxed over single connections so, unlike connections
// themselves, they are cheap to create and dispose of.
func (p *ProxyService) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	fmt.Printf("proxying request for %s to peer %s\n", r.URL, p.dest.Pretty())

	// We need to send the request to the remote libp2p peer, so we open a stream to it
	stream, err := p.host.NewStream(context.Background(), p.dest, Protocol)
	if err != nil {
		log.Println(err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	defer stream.Close()

	// r.Write() writes the HTTP request to the stream.
	err = r.Write(stream)
	if err != nil {
		stream.Reset()
		log.Println(err)
		http.Error(w, err.Error(), http.StatusServiceUnavailable)
		return
	}

	// Now we read the response that was sent from the dest peer
	buf := bufio.NewReader(stream)
	resp, err := http.ReadResponse(buf, r)
	if err != nil {
		stream.Reset()
		log.Println(err)
		http.Error(w, err.Error(), http.StatusServiceUnavailable)
		return
	}

	// Copy any headers
	for k, v := range resp.Header {
		for _, s := range v {
			w.Header().Add(k, s)
		}
	}

	// Write response status and headers
	w.WriteHeader(resp.StatusCode)

	// Finally copy the body
	io.Copy(w, resp.Body)
	resp.Body.Close()
}

// addAddrToPeerstore parses a peer multiaddress and adds it to the given
// host's peerstore, so it knows how to contact it. It returns the peer
// ID of the remote peer.
func addAddrToPeerstore(h host.Host, addr string) peer.ID {
	ipfsaddr, err := ma.NewMultiaddr(addr)
	if err != nil {
		log.Fatalln(err)
	}
	pid, err := ipfsaddr.ValueForProtocol(ma.P_IPFS)
	if err != nil {
		log.Fatalln(err)
	}

	peerid, err := peer.IDB58Decode(pid)
	if err != nil {
		log.Fatalln(err)
	}

	targetPeerAddr, _ := ma.NewMultiaddr(fmt.Sprintf("/ipfs/%s", peer.IDB58Encode(peerid)))
	targetAddr := ipfsaddr.Decapsulate(targetPeerAddr)

	h.Peerstore().AddAddr(peerid, targetAddr, ps.PermanentAddrTTL)
	return peerid
}

const help = `
This example creates a simple HTTP Proxy using two libp2p peers. The first peer
provides an HTTP server locally which tunnels the HTTP request with libp2p 
to a remote peer. The remote peer performs the requests and
send the sends the response back.

Usage: Start remote peer first with: ./proxy
       Then start the local peer with: ./proxy -d <remote-peer-multiaddress>

	   Then you can do something like: curl -x "localhost:9900" "http://ipfs.io".
	   This proxies sends the request through the local peer, which proxies it to
	   then remote peer, which makes it and sends the response back.
	   `

func main() {
	flag.Usage = func() {
		fmt.Println(help)
		flag.PrintDefaults()
	}

	destPeer := flag.String("d", "", "destination peer address")
	port := flag.Int("p", 9900, "proxy port")

	p2pport := flag.Int("l", 12000, "libp2p listen port")
	flag.Parse()

	if *destPeer != "" {
		host := makeRandomHost(*p2pport + 1)
		destPeerID := addAddrToPeerstore(host, *destPeer)
		proxyAddr, err := ma.NewMultiaddr(fmt.Sprintf("/ip4/127.0.0.1/tcp/%d", *port))
		if err != nil {
			log.Fatalln(err)
		}

		proxy := NewProxyService(host, proxyAddr, destPeerID)
		proxy.Serve()
	} else {
		host := makeRandomHost(*p2pport)
		_ = NewProxyService(host, nil, "")
		<-make(chan struct{})
	}
}
