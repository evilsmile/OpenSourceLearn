#!/usr/bin/env python
#-*- coding: utf-8 -*-

import pika
import sys

exchange_key='topic_logs'

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

channel.exchange_declare(exchange=exchange_key, type='topic')

routing_key = sys.argv[1] if len(sys.argv) > 1 else 'anonymous.info'

message = ' '.join(sys.argv[2:]) or "Hello World!"

# 根据命令行参数推送不同的日志等级
channel.basic_publish(exchange=exchange_key, 
                        routing_key=routing_key,
                        body=message,
                        properties=pika.BasicProperties(
                            delivery_mode = 2, # make message persistent
                            ))

print "[X] Send %r:%r" % (routing_key, message)
