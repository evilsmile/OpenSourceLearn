#!/usr/bin/env python
#-*- coding: utf-8 -*-

# Topic模式由于有"*"和"#"，变得非常强大并且可以转化为其他的exchange
# 如果binding_key是"#"  - 它会接收所有的Message，不管routing_key是什么，就像是fanout exchange
# 如果*和#都没有被使用，那么topic exchange就变成了direct exchange

# 启动3个consumer: 
# 1. ./log_consumer.py "#"   // 监听所有
# 2. ./log_consumer.py "kernel.*"   // 监听kernel相关日志
# 3. ./log_consumer.py "*.critical"   // 监听critial级别日志

# 由publisher 发布 "kernel.crital" 测试

import pika
import time
import sys

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

exchange_key='topic_logs'

# 模式是 topic. 
channel.exchange_declare(exchange=exchange_key, type='topic')

# 临时队列
new_queue=channel.queue_declare(exclusive=True) # 当Consumer关闭连接时，这个queue要被deleted。加个exclusive参数
# rabbitmq帮我们取了名字
queue_name=new_queue.method.queue 

binding_keys = sys.argv[1:]
if not binding_keys:
    print >> sys.stderr, "Usage: %s [binding_key] ... " % (sys.argv[0],)
    sys.exit(1)

for binding_key in binding_keys:
    # 绑定exchange和要发送的queue. 这样exchange才知道要发送到哪个Queue
    # 告诉exchange本queue对你感兴趣
    channel.queue_bind(exchange=exchange_key,
                      queue=queue_name,
                      routing_key=binding_key) 
  
print "[*] Waiting for logs. To exit press CTRL+C"

def callback(ch, method, properties, body):
    print "[X] Received %r:%r" % (method.routing_key, body, )
    print "[X] Done"

# 使得每个Consumer在同一个时间点最多处理一个Message。
# 也就是说， 在接收到该Consumer的ack前，他不会将新的Message分发给它
channel.basic_qos(prefetch_count=1)
channel.basic_consume(callback, queue=queue_name, no_ack=True)
channel.start_consuming()
