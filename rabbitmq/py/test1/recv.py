#!/usr/bin/python

import pika

connection = pika.BlockingConnection(pika.ConnectionParameters('localhost'))
channel = connection.channel()

channel.queue_declare(queue='hello')

print "[*] Waiting for Messages. To exit press CTRL+C"

def callback(ch, method, properties, body):
    print "[X] Received %r" % (body, )

channel.basic_consume(callback, queue='hello', no_ack=True)
channel.start_consuming()
