一、 总结使用 protobuf 格式进行消息通讯的过程:
     1. 编写 proto 接口文件

     2. 使用 protoc --go_out=. pb/*.proto 生成接口文件

     3. 在需要包操作的代码里引入 proto文件, 和 protobufCodec "go-multicodec/protobuf" 包

     4. 包操作

       + 4.1 组包

        - 4.1.1 生成包数据
        &p2p.MessageData{
            ClientVersion: clientVersion,
            NodeId:        peer.IDB58Encode(n.ID()),
            NodePubKey:    nodePubKey,
            Timestamp:     time.Now().Unix(),
            Id:            messageId,
            Gossip:        gossip,
        }

        - 4.1.2 序列化 (可选, 用于签名)
	        data, err := proto.Marshal(message), 然后使用KEY把data 签名

        - 4.1.3 生成 proto 数据, 并发送
            // s is 'inet.Stream'
            writer := bufio.NewWriter(s)
            enc := protobufCodec.Multicodec(nil).Encoder(writer)
            err := enc.Encode(data)
            writer.Flush()

       + 4.2 解包

         - 4.2.1 生成解码器
	       data := &pb.PingRequest{}
           // s is 'inet.Stream'
           decoder := protobufCodec.Multicodec(nil).Decoder(bufio.NewReader(s))

         - 4.2.2 解码
           err := decoder.Decode(data)

         - 4.2.3 使用 data 即可
           注意: 如果是别的格式, 则先声明data类型为别的格式, 要和数据对应. 格式来源于 pb包, 即用 protoc 生成的文件
           --->  data := &pb.PingResponse{}
                 decoder := protobufCodec.Multicodec(nil).Decoder(bufio.NewReader(s))
                 err := decoder.Decode(data)
