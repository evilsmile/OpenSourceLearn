package main

import (
	"context"
	pb "github.com/grpc-stream-example/proto"
	"google.golang.org/grpc"
	"io"
	"log"
)

const (
	PORT = "9002"
)

func main() {
	conn, err := grpc.Dial(":"+PORT, grpc.WithInsecure())
	if err != nil {
		log.Fatalf("grpc.Dail err: %v", err)
	}

	defer conn.Close()

	client := pb.NewStreamServiceClient(conn)

	err = printList(client, &pb.StreamRequest{Pt: &pb.StreamPoint{Name: "gRPC stream client: List", Value: 2018}})
	if err != nil {
		log.Fatalf("printList err: %v", err)
	}

	err = printRecord(client, &pb.StreamRequest{Pt: &pb.StreamPoint{Name: "gRPC stream client: Record", Value: 2018}})
	if err != nil {
		log.Fatalf("printRecord err: %v", err)
	}

	err = printRoute(client, &pb.StreamRequest{Pt: &pb.StreamPoint{Name: "gRPC stream client: Route", Value: 2018}})
	if err != nil {
		log.Fatalf("printRoute err: %v", err)
	}
}

func printList(client pb.StreamServiceClient, r *pb.StreamRequest) error {
	stream, err := client.List(context.Background(), r)
	if err != nil {
		return err
	}

	for {
		resp, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		log.Printf("resp: pj.name: %s, pt.value: %d", resp.Pt.Name, resp.Pt.Value)
	}

	return nil
}

func printRecord(client pb.StreamServiceClient, r *pb.StreamRequest) error {
	stream, err := client.Record(context.Background())
	if err != nil {
		return err
	}

	for n := 0; n < 6; n++ {
		err := stream.Send(r)
		if err != nil {
			return err
		}
	}

	stream.CloseSend()

	log.Printf("Record: all requests sent")
	resp, err := stream.Recv()
	if err != nil {
		return err
	}

	log.Printf("resp: pj.name: %s, pt.value: %d", resp.Pt.Name, resp.Pt.Value)

	return nil
}

func printRoute(client pb.StreamServiceClient, r *pb.StreamRequest) error {

	stream, err := client.Route(context.Background())
	if err != nil {
		return err
	}

	for n := 0; n < 6; n++ {
		err = stream.Send(r)
		if err != nil {
			return err
		}

		resp, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}

		log.Printf("resp: pj.name: %s, pt.value: %d", resp.Pt.Name, resp.Pt.Value)
	}

	stream.CloseSend()

	return nil
}
