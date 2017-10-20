#!/usr/bin/env python
#-*- coding: utf-8 -*-

import pika, sys

# 1. 建立到代理服务器的连接
credentials = pika.PlainCredentials("evilsmile", "evilsmile")
#credentials = pika.PlainCredentials("guest", "guest")
conn_params = pika.ConnectionParameters("localhost", credentials = credentials)
connection = pika.BlockingConnection(conn_params)

# 2. 获得信道
channel = connection.channel()

# 3. 声明交换器
channel.exchange_declare(exchange = "hello1-exchange",
                          type="direct",
                          durable=True,
                          auto_delete=False)

# 4. 声明队列
channel.queue_declare(queue="hello1-queue")

# 5. 通过键"halo"将队列和交换器绑定
channel.queue_bind(queue="hello1-queue",
                   exchange="hello1-exchange",
                   routing_key="hola")

# 6. 用于处理传入的消息的函数
def msg_consumer(channel, method, header, body):
    channel.basic_ack(delivery_tag = method.delivery_tag)   # 消息确认
    if body == "quit":
        channel.basic_cancel(consumer_tag = "hello1-consumer")
        channel.stop_consuming()                # 停止消费并退出
    else:
        print body
        
    return

# 7. 订阅消费者
channel.basic_consume(msg_consumer,
                      queue = "hello1-queue",
                      consumer_tag = "hello1-consumer")
# 8. 开始消费
channel.start_consuming()
