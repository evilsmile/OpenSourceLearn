#!/usr/bin/env python

import pika
import sys

exchange_key='direct_logs'

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

channel.exchange_declare(exchange=exchange_key, type='direct')

severity = sys.argv[1] if len(sys.argv) > 1 else 'info'

message = ' '.join(sys.argv[2:]) or "Hello World!"

# 根据命令行参数推送不同的日志等级
channel.basic_publish(exchange=exchange_key, 
                        routing_key=severity,
                        body=message,
                        properties=pika.BasicProperties(
                            delivery_mode = 2, # make message persistent
                            ))

print "[X] Send %r:%r" % (severity, message)
