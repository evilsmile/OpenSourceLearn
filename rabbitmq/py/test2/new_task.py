#!/usr/bin/python

import pika
import sys

queue_name='task_queue'

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

channel.queue_declare(queue=queue_name, durable= True)

message = ' '.join(sys.argv[1:]) or "Hello World!"
channel.basic_publish(exchange='', 
                        routing_key=queue_name,
                        body=message,
                        properties=pika.BasicProperties(
                            delivery_mode = 2, # make message persistent
                            ))
print "[X] Send %r" % (message, )
