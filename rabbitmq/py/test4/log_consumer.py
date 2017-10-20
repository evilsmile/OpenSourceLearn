#!/usr/bin/env python
#-*- coding: utf-8 -*-

# 根据命令行参数启动不同的consumer消费不同的错误等级日志
# 如 ./log_consumer.py error warning 订阅error/warning日志

import pika
import time
import sys

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

exchange_key='direct_logs'

# 模式是 direct. 另外还有两种模式 fanout(广播), topic
channel.exchange_declare(exchange=exchange_key, type='direct')

# 临时队列
new_queue=channel.queue_declare(exclusive=True) # 当Consumer关闭连接时，这个queue要被deleted。加个exclusive参数
# rabbitmq帮我们取了名字
queue_name=new_queue.method.queue 

severities = sys.argv[1:]
if not severities:
    print >> sys.stderr, "Usage: %s [info/warning/error]" % (sys.argv[0],)
    sys.exit(1)

for severity in severities:
    # 绑定exchange和要发送的queue. 这样exchange才知道要发送到哪个Queue
    # 告诉exchange本queue对你感兴趣
    channel.queue_bind(exchange=exchange_key,
                      queue=queue_name,
                      routing_key=severity) 
  
print "[*] Waiting for logs. To exit press CTRL+C"

def callback(ch, method, properties, body):
    print "[X] Received %r:%r" % (method.routing_key, body, )
    print "[X] Done"

# 使得每个Consumer在同一个时间点最多处理一个Message。
# 也就是说， 在接收到该Consumer的ack前，他不会将新的Message分发给它
channel.basic_qos(prefetch_count=1)
channel.basic_consume(callback, queue=queue_name, no_ack=True)
channel.start_consuming()
