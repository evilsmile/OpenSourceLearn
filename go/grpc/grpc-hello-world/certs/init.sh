#!/bin/bash

# 生成RSA私钥
#openssl genrsa -out server.key 2048

# 生成ECC私钥,选择曲线secp384r1
openssl ecparam -genkey -name secp384r1 -out server.key

# 生成自签名证书
openssl req -new -x509 -sha256 -key server.key -out server.pem -days 3650
