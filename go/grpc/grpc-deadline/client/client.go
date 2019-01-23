package main

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"io/ioutil"
	"log"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/status"

	pb "github.com/grpc-tls-ca/proto"
)

const PORT = "9001"

func main() {
	cert, err := tls.LoadX509KeyPair("conf/client/client.pem", "conf/client/client.key")
	if err != nil {
		log.Fatalf("tls.LoadX509KeyPair err: %v", err)
	}

	certPool := x509.NewCertPool()
	ca, err := ioutil.ReadFile("conf/ca.pem")
	if err != nil {
		log.Fatalf("ioutil.ReadFile err: %v", err)
	}

	if ok := certPool.AppendCertsFromPEM(ca); !ok {
		log.Fatalf("certPool.AppendCertsFromPEM err")
	}

	// Client通过请求得到Server端证书
	// Client使用CA认证的根证书对Server端的证书进行可靠性、有效性等校验
	// 检验ServerName是否可用、有效
	c := credentials.NewTLS(&tls.Config{
		Certificates: []tls.Certificate{cert},
		ServerName:   "evilsmile", // 与生成server端证书时设置的Common Name一致
		RootCAs:      certPool,
	})

	conn, err := grpc.Dial(":"+PORT, grpc.WithTransportCredentials(c))
	if err != nil {
		log.Fatalf("grpc.Dial err: %v", err)
	}
	defer conn.Close()

	ctx, cancel := context.WithDeadline(context.Background(), time.Now().Add(time.Duration(1*time.Second)))
	defer cancel()

	client := pb.NewSearchServiceClient(conn)
	resp, err := client.Search(ctx, &pb.SearchRequest{
		Request: "gGG",
	})
	if err != nil {
		statusErr, ok := status.FromError(err)
		if ok {
			if statusErr.Code() == codes.DeadlineExceeded {
				log.Fatalln("client.Search err: dealine")
			}
		}
		log.Fatalf("grpc.Search err: %v", err)
	}

	log.Printf("resp: %s", resp.GetResponse())
}
