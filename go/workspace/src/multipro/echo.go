package main

import (
	"bufio"
	"context"
	"fmt"
	"log"

	uuid "github.com/google/uuid"
	inet "gx/ipfs/QmRKbEchaYADxSCyyjhDh4cTrUby8ftXUb8MRLBTHQYupw/go-libp2p-net"
	"gx/ipfs/QmVrjR2KMe57y4YyfHdYa3yKD278gN8W7CTiqSuYmxjA7F/go-libp2p-host"
	protobufCodec "gx/ipfs/QmYMiyZRYDmhMr2phMc4FGrYbsyzvR751BgeobnWroiq2z/go-multicodec/protobuf"
	pb "multipro/pb"
)

const echoRequest = "/echo/evil-echoreq/0.0.1"
const echoResponse = "/echo/evil-echoresp/0.0.1"

type EchoProtocol struct {
	node     *Node
	requests map[string]*pb.EchoRequest
	done     chan bool
}

func NewEchoProtocol(node *Node, done chan bool) *EchoProtocol {
	e := EchoProtocol{node: node, requests: make(map[string]*pb.EchoRequest), done: done}
	node.SetStreamHandler(echoRequest, e.onEchoRequest)
	node.SetStreamHandler(echoResponse, e.onEchoResponse)

	return &e
}

func (e *EchoProtocol) onEchoRequest(s inet.Stream) {
	data := &pb.EchoRequest{}
	decoder := protobufCodec.Multicodec(nil).Decoder(bufio.NewReader(s))
	err := decoder.Decode(data)
	if err != nil {
		return
	}

	valid := e.node.authenticateMessage(data, data.MessageData)
	if !valid {
		log.Println("Failed to authenticate message")
		return
	}

	req, ok := e.requests[data.MessageData.Id]
	if ok {
		delete(e.requests, data.MessageData.Id)
	} else {
		log.Println("Failed to locate request data object for response")
		return
	}

	if req.Message != data.Message {
		log.Fatalln("Expect echo to respond with request message")
	}

	log.Printf("%s: Received echo response from %s. Message id: %s. Message: %s.", s.Conn().LocalPeer(), s.Conn().RemotePeer(), data.MessageData.Id, data.Message)
	e.done <- true
}

func (e *EchoProtocol) onEchoResponse(s inet.Stream) {
	data := &pb.EchoResponse{}
	decode := protobufCodec.Multicodec(nil).Decoder(bufio.NewReader(s))
	err := decode.Decode(data)
	if err != nil {
		return
	}

	valid := e.node.authenticateMessage(data, data.MessageData)
	if !valid {
		log.Println("Failed to authenticate message")
		return
	}
	req, ok := e.requests[data.MessageData.Id]
	if ok {
		delete(e.requests, data.MessageData.Id)
	} else {
		log.Println("Failed to locate request data bobject for response")
		return
	}

	if req.Message != data.Message {
		log.Fatalln("Expected echo to respond with request message")
	}

	log.Printf("%s: Received echo response from %s. Message id: %s. Message: %s.", s.Conn().LocalPeer(), s.Conn().RemotePeer(), data.MessageData.Id, data.Message)
	e.done <- true
}

func (e *EchoProtocol) Echo(host host.Host) bool {
	log.Printf("%s: Sending echo to: %s....", e.node.ID(), host.ID())

	req := &pb.EchoRequest{
		MessageData: e.node.NewMessageData(uuid.New().String(), false),
		Message:     fmt.Sprintf("Echo from %s", e.node.ID()),
	}

	signature, err := e.node.signProtoMessage(req)
	if err != nil {
		log.Println("failed to sign message")
		return false
	}

	req.MessageData.Sign = signature

	s, err := e.node.NewStream(context.Background(), host.ID(), echoRequest)
	if err != nil {
		log.Println(err)
		return false
	}

	ok := e.node.sendProtoMessage(req, s)

	if !ok {
		return false
	}

	e.requests[req.MessageData.Id] = req
	log.Printf("%s: Echo to: %s was sent. Message Id: %s, Message: %s", e.node.ID(), host.ID(), req.MessageData.Id, req.Message)
	return true
}
