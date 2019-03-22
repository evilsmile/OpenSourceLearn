package server

import (
	"fmt"
	"log"
	"net"
)

const (
	MSGLEN = 1024
)

type ProxyServer struct {
	listenIP    string
	listenPort  int
	backendAddr string
}

func NewProxyServer(listenIP string, listenPort int, backendAddr string) *ProxyServer {
	return &ProxyServer{
		backendAddr: backendAddr,
		listenIP:    listenIP,
		listenPort:  listenPort,
	}
}

func (p *ProxyServer) handle(conn *net.TCPConn, id int) {

	log.Printf("request: %d\n", id)

	// Read in request
	data := make([]byte, MSGLEN)
	n, err := conn.Read(data)
	if err != nil {
		log.Printf("Read data error from '%s':[%s]. Disconnect.", conn.RemoteAddr().String(), err)
		conn.Close()
		return
	}

	// Connect backend
	backendConn, err := net.Dial("tcp", p.backendAddr)
	if err != nil {
		log.Println("Connect backend error:", err)
		return
	}
	defer backendConn.Close()

	// Send to backend
	req := data[0:n]
	_, err = backendConn.Write([]byte(req))
	if err != nil {
		log.Println("Send data to backend error:", err)
		return
	}

	// Recv from backedn
	buf := make([]byte, MSGLEN)
	n, err = backendConn.Read(buf)
	if err != nil {
		log.Println("Read data from backend error: ", err)
		return
	}

	_, err = conn.Write(buf[0:n])
	if err != nil {
		log.Println("Reply to client error:", err)
		return
	}
}

func (p *ProxyServer) Run() error {

	var err error
	listen, err := net.ListenTCP("tcp", &net.TCPAddr{net.ParseIP(p.listenIP), p.listenPort, ""})
	if err != nil {
		return err
	}
	fmt.Printf("Listen '%s:%d'.\n", p.listenIP, p.listenPort)

	id := 0
	for {
		conn, err := listen.AcceptTCP()
		if err != nil {
			return err
		}
		log.Println("Connection from " + conn.RemoteAddr().String())
		go p.handle(conn, id)

	}
}
