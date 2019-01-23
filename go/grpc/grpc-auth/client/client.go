package main

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"io/ioutil"
	"log"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials"

	pb "github.com/grpc-auth/proto"
)

const PORT = "9001"

type Auth struct {
	AppKey    string
	AppSecret string
}

// PerRPCCredentials interface 是gRPC默认提供用于自定义认证的接口
// 它的作用是将所需的安全认证信息添加到每个RPC方法的上下文中.
// 需要实现两个方法:
//   + GetRequestMetadata(...)       // 获取当前请求认证所需的元数据
//   + RequireTransportSecurity()    // 是否需要基于TLS认证进行传输
func (a *Auth) GetRequestMetadata(ctx context.Context, uri ...string) (map[string]string, error) {
	return map[string]string{"app_key": a.AppKey, "app_secret": a.AppSecret}, nil
}

func (a *Auth) RequireTransportSecurity() bool {
	return true
}

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

	auth := Auth{
		AppKey:    "evilsmile-auth",
		AppSecret: "20180123",
	}
	conn, err := grpc.Dial(":"+PORT, grpc.WithTransportCredentials(c), grpc.WithPerRPCCredentials(&auth))
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
