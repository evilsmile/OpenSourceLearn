#!/usr/bin/env python
#-*- coding: utf-8 -*-

import pika
import time

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

exchange_name='logs'

# fanout 就是广播模式，会将所有的Message都放到它所知道的queue中. 其它的模式是 topic, direct
channel.exchange_declare(exchange=exchange_name, type='fanout')

# 临时队列
new_queue=channel.queue_declare(exclusive=True) # 当Consumer关闭连接时，这个queue要被deleted。加个exclusive参数
# rabbitmq帮我们取了名字
queue_name=new_queue.method.queue 

# 绑定exchange和要发送的queue. 这样exchange才知道要发送到哪个Queue
channel.queue_bind(exchange=exchange_name,
                      queue=queue_name) 
  
print "[*] Waiting for Messages. To exit press CTRL+C"

def callback(ch, method, properties, body):
    print "[X] Received %r" % (body, )
    time.sleep(1)
    #time.sleep(body.count('.'))
    print "[X] Done"
    # 必需要确认。 屏蔽
    #ch.basic_ack(False)

# 使得每个Consumer在同一个时间点最多处理一个Message。
# 也就是说， 在接收到该Consumer的ack前，他不会将新的Message分发给它
channel.basic_qos(prefetch_count=1)
channel.basic_consume(callback, queue=queue_name, no_ack=True)
channel.start_consuming()
