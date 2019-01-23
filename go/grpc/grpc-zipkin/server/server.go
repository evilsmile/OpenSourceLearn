/*
 *   gRPC + Opentracing + Zipkin 分布式链路追踪, 查看整个系统的链路、性能等指标
 */
package main

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"io/ioutil"
	"log"
	"net"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	"github.com/grpc-ecosystem/grpc-opentracing/go/otgrpc"
	zipkin "github.com/openzipkin/zipkin-go-opentracing"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"

	pb "github.com/grpc-zipkin/proto"
)

type SearchService struct{}

func (s *SearchService) Search(ctx context.Context, r *pb.SearchRequest) (*pb.SearchResponse, error) {
	return &pb.SearchResponse{Response: r.GetRequest() + " Server"}, nil
}

const (
	PORT = "9001"

	SERVICE_NAME              = "evilsmile"
	ZIPKIN_HTTP_ENDPOINT      = "http://127.0.0.1:9411/api/v1/spans"
	ZIPKIN_RECORDER_HOST_PORT = "127.0.0.1:9000"
)

func main() {
	// 创建一个 Zipkin HTTP 后端收集器
	collector, err := zipkin.NewHTTPCollector(ZIPKIN_HTTP_ENDPOINT)
	if err != nil {
		log.Fatalf("zipkin.NewHTTPCollector err: %v", err)
	}

	// 创建一个基于Zipkin收集器的记录器
	recorder := zipkin.NewRecorder(collector, true, ZIPKIN_RECORDER_HOST_PORT, SERVICE_NAME)

	// 一个Trace代表一个事务或者流程在分布式系统中的执行过程
	// 一个span代表在分布式系统中完成的单个工作单元。也包含其他span的引用，这允许将多个spans组合成一个完整的Trace
	// 每个span根据OPenTracing 规范封装了：
	//  + 操作名称
	//  + 开始时间和结束时间
	//  + key:value span Tags   # 跨度标签，可以理解为用户自定义的Span注释
	//  + key:value span Logs   # 跨度日志，可以记录Span内特定时间或事件的日志信息。主要用于捕获特定Span的日志信息及应用程序本身的其他调试或信息输出
	//  + SpanContext       # 代表跨越进程边界，传递到子级Span的状态
	// 创建一个OpenTracing 跟踪器
	tracer, err := zipkin.NewTracer(
		recorder, zipkin.ClientServerSameSpan(false),
	)
	if err != nil {
		log.Fatalf("zipkin.NewTrace err: %v", err)
	}

	cert, err := tls.LoadX509KeyPair("conf/server/server.pem", "conf/server/server.key")
	if err != nil {
		log.Fatalf("tls.LoadX509KeyPair err: %v", err)
	}

	certPool := x509.NewCertPool()
	ca, err := ioutil.ReadFile("conf/ca.pem")
	if err != nil {
		log.Fatal("ioutil.ReadFile err: %v", err)
	}

	if ok := certPool.AppendCertsFromPEM(ca); !ok {
		log.Fatalf("certPool.AppendCertsFromPEM err")
	}

	// Server 使用CA认证的根证书对Client端的证书进行可靠性、有效性检查
	c := credentials.NewTLS(&tls.Config{
		Certificates: []tls.Certificate{cert},
		ClientAuth:   tls.RequireAndVerifyClientCert,
		ClientCAs:    certPool,
	})

	opts := []grpc.ServerOption{
		grpc.Creds(c),
		grpc_middleware.WithUnaryServerChain(
			otgrpc.OpenTracingServerInterceptor(tracer, otgrpc.LogPayloads()),
		),
	}

	server := grpc.NewServer(opts...)
	pb.RegisterSearchServiceServer(server, &SearchService{})

	lis, err := net.Listen("tcp", ":"+PORT)
	if err != nil {
		log.Fatalf("net.Listen error: %v", err)
	}

	server.Serve(lis)
}
