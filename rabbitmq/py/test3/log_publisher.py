#!/usr/bin/env python

import pika
import sys

exchange_name='logs'

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

channel.exchange_declare(exchange=exchange_name, type='fanout')

message = ' '.join(sys.argv[1:]) or "Hello World!"
channel.basic_publish(exchange=exchange_name, 
                        routing_key='',
                        body=message,
                        properties=pika.BasicProperties(
                            delivery_mode = 2, # make message persistent
                            ))
print "[X] Send %r" % (message, )
