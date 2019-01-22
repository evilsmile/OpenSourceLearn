package util

import (
	"crypto/tls"
	"io/ioutil"
	"log"

	"golang.org/x/net/http2"
)

// 处理从证书凭证文件(PEM),最终获取tls.Config作为HTTP/2的参数
func GetTLSConfig(certPemPath, certKeyPath string) *tls.Config {
	var certKeyPair *tls.Certificate
	cert, _ := ioutil.ReadFile(certPemPath)
	key, _ := ioutil.ReadFile(certKeyPath)

	// 从一对PEM编码的数据中解析公钥/私钥对
	pair, err := tls.X509KeyPair(cert, key)
	if err != nil {
		log.Println("TLS KeyPair err: %v\n", err)
	}

	certKeyPair = &pair

	return &tls.Config{
		Certificates: []tls.Certificate{*certKeyPair},
		// NextProtoTLS 是谈判期间的NPN/ALPN协议,用于HTTP/2的TLS设置
		NextProtos: []string{http2.NextProtoTLS},
	}
}
