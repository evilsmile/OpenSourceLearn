package main

import (
	"context"
	"log"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"

	pb "github.com/grpc-tls/proto"
)

const PORT = "9001"

func main() {
	c, err := credentials.NewClientTLSFromFile("conf/server.pem", "evilsmile")
	if err != nil {
		log.Fatalf("credentials.NewServerTLSFromFile err: %v", err)
	}

	conn, err := grpc.Dial(":"+PORT, grpc.WithTransportCredentials(c))
	if err != nil {
		log.Fatalf("grpc.Dial err: %v", err)
	}
	defer conn.Close()

	client := pb.NewSearchServiceClient(conn)
	resp, err := client.Search(context.Background(), &pb.SearchRequest{
		Request: "gGG",
	})
	if err != nil {
		log.Fatalf("grpc.Search err: %v", err)
	}

	log.Printf("resp: %s", resp.GetResponse())
}
