#!/bin/bash

# 创建初始化目录
# 从openssl.cnf中读取根路径
InitCADirs() {
    cfg_path=$(openssl version -a | grep OPENSSLDIR | awk -F'[ ":]' '{print $(NF-1)}')
    path=$(grep -A 2 CA_default $cfg_path/openssl.cnf | grep -w dir | awk '{  if ($3~/^\/.*/){                      \
                                                                            print $3                          \
                                                                        } else if($3 ~ /^\.\/.*/){           \
                                                                            print "'$cfg_path'/"$3           \
                                                                        } else {                             \
                                                                            print "unkown"                   \
                                                                        }                                    \
                                                                    }')
    if [ "$path" == "unkown" ]; then
        echo "Unkown [CA_default] path"
        exit -1
    fi

    if [ ! -d $path ]; then
        mkdir $path
    fi

    ### ====================> Switch path
    cd $path

    if [ -d certs ]; then
        echo "======= $path already initialized! ====>"
        return
    fi

    echo "Init $path ..."
    mkdir certs          # issued certs
    mkdir newcerts       # new certs
    mkdir private        # private keys
    mkdir crl            # revoked certs
    touch index.txt      # text database of signed and issued certs
    echo "01" > serial   # sequence ref file when sign certs

    echo "Init done."
    ls -l .
}

################################ 根证书相关 BEGIN ###############################
GenRootPrivateKey() {
    # 为key设置密码, 密码使用 aes128 加密保存
    openssl genrsa -aes128 -out rootCA.priv.key 2048
}

GenRootPublicKey() {
    openssl rsa -in rootCA.priv.key -pubout -out rootCA.pub.key    
}

GenSignReq() {
    # generated csr file used to send to Authority and wait for its authentication
    openssl req -new -key rootCA.priv.key -out rootCA.csr
}

GenRootCert() {
    openssl x509 -req -days 365 -in rootCA.csr -signkey rootCA.priv.key -out rootCA.crt
}
################################ 根证书相关 END ###############################

initRootCA() {
    GenRootPrivateKey
    GenRootPublicKey
    GenSignReq
    GenRootCert
}

# 用自签根证书 给用户证书签名
issueNewServerCert() {
    # 生成新的https用的服务端证书
    newserver=/tmp/server2
    openssl genrsa -out ${newserver}.key 1024
    # 生成证书请求
    openssl req -new -key ${newserver}.key -out ${newserver}.csr
    openssl ca -in ${newserver}.csr -out ${newserver}.crt -cert rootCA.crt -keyfile rootCA.priv.key
}

InitCADirs
#initRootCA
issueNewServerCert
