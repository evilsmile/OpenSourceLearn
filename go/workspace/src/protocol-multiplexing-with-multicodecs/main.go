package main

import (
	"bufio"
	"context"
	"fmt"
	"log"
	"math/rand"
	"time"

	inet "gx/ipfs/QmRKbEchaYADxSCyyjhDh4cTrUby8ftXUb8MRLBTHQYupw/go-libp2p-net"
	ps "gx/ipfs/QmUymf8fJtideyv3z727BcZUifGBjMZMpCJqu3Gxk5aRUk/go-libp2p-peerstore"
	host "gx/ipfs/QmVrjR2KMe57y4YyfHdYa3yKD278gN8W7CTiqSuYmxjA7F/go-libp2p-host"
	libp2p "gx/ipfs/QmXnpYYg2onGLXVxM4Q5PEFcx29k8zeJQkPeLAk9h9naxg/go-libp2p"
	multicodec "gx/ipfs/QmYMiyZRYDmhMr2phMc4FGrYbsyzvR751BgeobnWroiq2z/go-multicodec"
	json "gx/ipfs/QmYMiyZRYDmhMr2phMc4FGrYbsyzvR751BgeobnWroiq2z/go-multicodec/json"
)

const proto = "/evil-multiproto-multicodecs/1.0.0"

type Message struct {
	Msg    string
	Index  int
	HangUp bool
}

type WrappedStream struct {
	stream inet.Stream
	enc    multicodec.Encoder
	dec    multicodec.Decoder
	w      *bufio.Writer
	r      *bufio.Reader
}

func WrapStream(s inet.Stream) *WrappedStream {
	w := bufio.NewWriter(s)
	r := bufio.NewReader(s)

	dec := json.Multicodec(false).Decoder(r)
	enc := json.Multicodec(false).Encoder(w)

	return &WrappedStream{
		stream: s,
		enc:    enc,
		dec:    dec,
		w:      w,
		r:      r,
	}
}

var conversationMsg = []string{
	"Hello!",
	"Hey!",
	"How are you doing?",
	"Very good! It is great that you can send data on a stream to me!",
	"Not only that, the data is encoded in a JSON object.",
	"Yeah, and we are using the multicodecs interface to encode and decode.",
	"This way we could swap it easily for, say, cbor, or msgpack!",
	"Let's leave that as an excercise for the reader...",
	"Agreed, our last message should activate the HangUp flag",
	"Yes, and the example code will close streams. So sad :(. Bye!",
}

func makeRandomHost(port int) host.Host {
	// 设置明文传输
	h, err := libp2p.New(context.Background(), libp2p.ListenAddrStrings(fmt.Sprintf("/ip4/127.0.0.1/tcp/%d", port)), libp2p.NoSecurity)
	if err != nil {
		panic(err)
	}

	return h
}

func main() {
	rand.Seed(5)
	port1 := rand.Intn(100) + 10000
	port1 = 8232
	port2 := port1 + 1

	h1 := makeRandomHost(port1)
	h2 := makeRandomHost(port2)

	h1.Peerstore().AddAddrs(h2.ID(), h2.Addrs(), ps.PermanentAddrTTL)
	h2.Peerstore().AddAddrs(h1.ID(), h1.Addrs(), ps.PermanentAddrTTL)

	log.Printf("Begin conversation between '%s' and '%s'", h1.ID(), h2.ID())

	h1.SetStreamHandler(proto, func(stream inet.Stream) {
		log.Printf("%s: receive a stream", h1.ID())
		wrappedStream := WrapStream(stream)
		defer stream.Close()
		handleStream(wrappedStream)
	})

	stream, err := h2.NewStream(context.Background(), h1.ID(), proto)
	if err != nil {
		log.Fatal(err)
	}

	wrappedStream := WrapStream(stream)
	sendMessage(0, wrappedStream)

	handleStream(wrappedStream)
	stream.Close()
}

func receiveMessage(ws *WrappedStream) (*Message, error) {
	var msg Message
	err := ws.dec.Decode(&msg)
	if err != nil {
		return nil, err
	}
	return &msg, nil
}

func sendMessage(index int, ws *WrappedStream) error {
	msg := &Message{
		Msg:    conversationMsg[index],
		Index:  index,
		HangUp: index >= len(conversationMsg)-1,
	}

	err := ws.enc.Encode(msg)
	ws.w.Flush()
	return err
}

func handleStream(ws *WrappedStream) {
	for {
		msg, err := receiveMessage(ws)
		if err != nil {
			break
		}
		pid := ws.stream.Conn().LocalPeer()
		log.Printf("%s says: '%s'", pid, msg.Msg)
		time.Sleep(500 * time.Millisecond)
		if msg.HangUp {
			break
		}

		err = sendMessage(msg.Index+1, ws)
		if err != nil {
			break
		}
	}
}
