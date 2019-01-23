#!/bin/bash

if [ ! -d conf ]; then
    mkdir conf
fi

cd conf

gen_ca() {
    # 生成Key
    openssl genrsa -out ca.key 2048

    # 生成密钥
    openssl req -new -x509 -days 7200 -subj "/commonName=evil/countryName=CN/stateOrProvinceName=Shenzhen/organizationName=evil/organizationalUnitName=evilsmile/" -key ca.key -out ca.pem

}

gen_server_key() {
    if [ ! -d server ]; then
        mkdir server
    fi
    cd server
    # 生成服务端证书
    openssl ecparam -genkey -name secp384r1 -out server.key
    # 生成CSR [Certificate Signing Request]
    # 证书请求文件, CA会利用CSR文件进行签名使得攻击者无法伪装或篡改原有证书
    openssl req -new -key server.key -out server.csr

    # 基于CA签发
    ################ 设置 "common name" 为 "evilsmile" ################
    #  与client.go中的 ServerName一致
    #  subject信息不需要与下面的 client证书配置一致
    openssl x509 -req -sha256 -CA ../ca.pem -CAkey ../ca.key -CAcreateserial -days 3650 -in server.csr -out server.pem
    cd ..

}

gen_client_key() {
    if [ ! -d client ]; then
        mkdir client
    fi
    cd client
    openssl ecparam -genkey -name secp384r1 -out client.key
    openssl req -new -key client.key -out client.csr
    openssl x509 -req -sha256 -CA ../ca.pem -CAkey ../ca.key -CAcreateserial -days 3650 -in client.csr -out client.pem
    cd ..
}

gen_ca
gen_server_key
gen_client_key
