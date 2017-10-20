#!/usr/bin/env python
#-*- coding: utf-8 -*-

import pika, sys
from pika import spec

# 1. 建立到代理服务器的连接
credentials = pika.PlainCredentials("guest", "guest")
conn_params = pika.ConnectionParameters("localhost", credentials = credentials)
connection = pika.BlockingConnection(conn_params)

# 2. 获得信道
channel = connection.channel()

# 3. 发送方确认模式处理器
def confirm_handler(frame):
    if type(frame.method) == spec.Confirm.SelectOk:    # 当第一次将信道设置为confirm模式时，rabbitmq会发送一条Confirm.SelectOk类型的确认消息
        print "Channel in 'confirm' mode."             # 这是通知你信道已经准备就绪来接收发送方确认消息了
    elif type(frame.method) == spec.Basci.Nack:        # Nack类型消息指明了消息由于RabbitMQ内部错误而丢失了
        if frame.method.delivery_tag in msg_ids:       # 先检查一下delivery_tag确认是丢失了
            print "Message Lost"
    elif type(frame.method) == spec.Basci.Ack:         # 确认该消息已经成功入队
        if frame.method.delivery_tag in msg_ids:
            print "Confirm received!"
            msg_ids.remove(frame.method.delivery_tag)  # 随后将其从等待投递确认的ID列表中删除

#channel.confirm_delivery(callback = confirm_handler)
# 4. 声明交换器
channel.exchange_declare(exchange = "hello1-exchange",
                          type="direct",
                          durable=True,
                          auto_delete=False)

# 5. 从文件中读入消息
msgcontent_file = sys.argv[1]
msg_sendcnt = int(sys.argv[2])
input = open(msgcontent_file, 'r')
msg=""
try:
    msg=input.read()
finally:
    input.close()

msg_props = pika.BasicProperties()
msg_props.content_type = "text/plain"
msg_ids = []

count=0
while count < msg_sendcnt:
    # 6. 发布消息
    channel.basic_publish(body=msg, 
                      exchange='hello1-exchange', 
                      properties = msg_props,
                      routing_key='hola')
    # 7. 将ID添加到追踪列表
    #msg_ids.append(len(msg_ids) + 1)    
    print count
    count+=1

channel.close()
