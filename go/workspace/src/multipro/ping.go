// 调用提供的外部接口 Ping(...) 向其它节点发送 protobuf数据的 Stream
// 目标节点设置协议处理函数 onPingRequest(...), 接收ping请求验证签名(数据包中有公钥),并返回 pong回复包.
// 调用节点也设置了协议处理函数 onPingResponse(...), 解protobuf返回,验证签名,找到缓存中对应的请求,打印结果

package main

import (
	"bufio"
	"context"
	"fmt"
	"log"

	uuid "github.com/google/uuid"
	inet "gx/ipfs/QmRKbEchaYADxSCyyjhDh4cTrUby8ftXUb8MRLBTHQYupw/go-libp2p-net"
	host "gx/ipfs/QmVrjR2KMe57y4YyfHdYa3yKD278gN8W7CTiqSuYmxjA7F/go-libp2p-host"
	protobufCodec "gx/ipfs/QmYMiyZRYDmhMr2phMc4FGrYbsyzvR751BgeobnWroiq2z/go-multicodec/protobuf"
	pb "multipro/pb"
)

const pingRequest = "/ping/evil-pingreq/0.0.1"
const pingResponse = "/ping/evil-pingresp/0.0.1"

type PingProtocol struct {
	node     *Node
	requests map[string]*pb.PingRequest
	done     chan bool
}

func NewPingProtocol(node *Node, done chan bool) *PingProtocol {
	p := &PingProtocol{node: node,
		requests: make(map[string]*pb.PingRequest),
		done:     done,
	}

	node.SetStreamHandler(pingRequest, p.onPingRequest)
	node.SetStreamHandler(pingResponse, p.onPingResponse)
	return p
}

func (p *PingProtocol) onPingRequest(s inet.Stream) {
	data := &pb.PingRequest{}
	decoder := protobufCodec.Multicodec(nil).Decoder(bufio.NewReader(s))
	err := decoder.Decode(data)
	if err != nil {
		log.Println(err)
		return
	}

	log.Printf("%s: Received ping response from %s. Message id: %s. Message: %s.", s.Conn().LocalPeer(), s.Conn().RemotePeer(), data.MessageData.Id, data.Message)

	valid := p.node.authenticateMessage(data, data.MessageData)

	if !valid {
		log.Println("Failed to authenticate message")
		return
	}

	log.Printf("%s: Sending ping response to %s. Message id: %s...", s.Conn().LocalPeer(), s.Conn().RemotePeer(), data.MessageData.Id)

	resp := &pb.PingResponse{MessageData: p.node.NewMessageData(data.MessageData.Id, false),
		Message: fmt.Sprintf("Ping resposne from %s", p.node.ID())}

	signature, err := p.node.signProtoMessage(resp)
	if err != nil {
		log.Println("failed to sign response")
		return
	}

	resp.MessageData.Sign = signature

	s, respErr := p.node.NewStream(context.Background(), s.Conn().RemotePeer(), pingResponse)
	if respErr != nil {
		log.Println(respErr)
		return
	}

	ok := p.node.sendProtoMessage(resp, s)
	if ok {
		log.Printf("%s: Ping response to %s sent.", s.Conn().LocalPeer().String(), s.Conn().RemotePeer().String())
	}
}

func (p *PingProtocol) onPingResponse(s inet.Stream) {
	data := &pb.PingResponse{}
	decoder := protobufCodec.Multicodec(nil).Decoder(bufio.NewReader(s))
	err := decoder.Decode(data)
	if err != nil {
		return
	}

	valid := p.node.authenticateMessage(data, data.MessageData)
	if !valid {
		log.Println("Failed to authenticate message")
		return
	}

	_, ok := p.requests[data.MessageData.Id]
	if ok {
		delete(p.requests, data.MessageData.Id)
	} else {
		log.Println("Failed to locate request data bobject for response")
		return
	}

	log.Printf("%s: Received ping response from %s. Message id:%s. Message: %s.", s.Conn().LocalPeer(), s.Conn().RemotePeer(), data.MessageData.Id, data.Message)

	p.done <- true
}

func (p *PingProtocol) Ping(host host.Host) bool {
	log.Printf("%s: Sending ping to :%s...", p.node.ID(), host.ID())

	req := &pb.PingRequest{MessageData: p.node.NewMessageData(uuid.New().String(), false),
		Message: fmt.Sprintf("Ping from %s", p.node.ID())}

	signature, err := p.node.signProtoMessage(req)
	if err != nil {
		log.Println("failed to sign pb data")
		return false
	}

	req.MessageData.Sign = signature

	s, err := p.node.NewStream(context.Background(), host.ID(), pingRequest)
	if err != nil {
		log.Println(err)
		return false
	}

	ok := p.node.sendProtoMessage(req, s)
	if !ok {
		return false
	}

	p.requests[req.MessageData.Id] = req
	log.Printf("%s: Ping to: %s was sent. Message id: %s, Message: %s", p.node.ID(), host.ID(), req.MessageData.Id, req.MessageData)

	return true
}
