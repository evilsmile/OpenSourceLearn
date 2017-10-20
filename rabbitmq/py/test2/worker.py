#!/usr/bin/python

import pika
import time

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

queue_name='task_queue'
channel.queue_declare(queue=queue_name, durable= True)

print "[*] Waiting for Messages. To exit press CTRL+C"

def callback(ch, method, properties, body):
    print "[X] Received %r" % (body, )
    time.sleep(1)
    #time.sleep(body.count('.'))
    print "[X] Done"
    ch.basic_ack(delivery_tag = method.delivery_tag)

# 使得每个Consumer在同一个时间点最多处理一个Message。
# 也就是说， 在接收到该Consumer的ack前，他不会将新的Message分发给它
channel.basic_qos(prefetch_count=1)
channel.basic_consume(callback, queue=queue_name, no_ack=True)
channel.start_consuming()
