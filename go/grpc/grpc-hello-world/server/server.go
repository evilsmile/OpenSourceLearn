package server

import (
	"crypto/tls"
	"log"
	"net"
	"net/http"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"golang.org/x/net/context"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"

	"github.com/grpc-hello-world/pkg/util"
	pb "github.com/grpc-hello-world/proto"
)

var (
	ServerPort  string
	CertName    string
	CertPemPath string
	CertKeyPath string
	EndPoint    string
)

func Serve() (err error) {
	EndPoint = ":" + ServerPort
	conn, err := net.Listen("tcp", EndPoint)
	if err != nil {
		log.Printf("TCP listen err: %v\n", err)
		return err
	}

	tlsConfig := util.GetTLSConfig(CertPemPath, CertKeyPath)
	srv := createInternaServer(conn, tlsConfig)

	log.Printf("gRPC and https listen on: %s\n", ServerPort)

	// NewListener 创建一个Listener
	// srv.Serve 服务开始接收请求
	if err = srv.Serve(tls.NewListener(conn, tlsConfig)); err != nil {
		log.Printf("ListenAndServe: %v\n", err)
	}
	return err
}

func createInternaServer(conn net.Listener, tlsConfig *tls.Config) *http.Server {

	var opts []grpc.ServerOption
	// 创建grpc的TLS认证凭证
	// credentials包实现了grpc库的各种凭证,其封装了客户机需要的所有状态,以便与
	// 服务器进行身份验证并进行各种断言,如关于客户机的身份,角色或是否授权调用
	creds, err := credentials.NewServerTLSFromFile(CertPemPath, CertKeyPath)
	if err != nil {
		log.Printf("Failed to create server TLS credentials %v", err)
		return nil
	}

	// 设置GRPC ServerOption
	opts = append(opts, grpc.Creds(creds))
	// 创建 grpc 服务端
	grpcServer := grpc.NewServer(opts...)

	// 注册 rpc 服务
	pb.RegisterHelloWorldServer(grpcServer, NewHelloService())

	// 创建  grpc-gateway 关联组件
	ctx := context.Background()
	dcreds, err := credentials.NewClientTLSFromFile(CertPemPath, CertName)
	if err != nil {
		log.Printf("Failed to create client TLS credentials %v", err)
		return nil
	}

	// WithTransportCredentials 配置一个连接级别的安全凭证
	dopts := []grpc.DialOption{grpc.WithTransportCredentials(dcreds)}
	// 创建 HTTP NewServeMux 及注册 grpc-gateway 逻辑
	// ServeMux 是 grpc-gateway 的一个请求多路复用器.它将与 http 请求模式匹配并调用
	gwmux := runtime.NewServeMux()

	// 注册服务
	if err := pb.RegisterHelloWorldHandlerFromEndpoint(ctx, gwmux, EndPoint, dopts); err != nil {
		log.Printf("Failed to register gw server: %v\n", err)
		return nil
	}

	mux := http.NewServeMux()
	mux.Handle("/", gwmux)

	return &http.Server{
		Addr:      EndPoint,
		Handler:   util.GrpcHandlerFunc(grpcServer, mux),
		TLSConfig: tlsConfig,
	}
}
