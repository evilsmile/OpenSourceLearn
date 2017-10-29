#include "common.h"
#include "names.h"
#include "rabbitmq_util.h"
#include "mqthread.h"

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc > 1) {
        rate_limit = atoi(argv[1]);
    }

    RabbitMQ rabbitMQ(USER, PASSWD, HOST_IP, HOST_PORT, LOCAL_CHANNEL_ID);
    rabbitMQ.exchange_declare(LOCAL_EXCHANGE, LOCAL_EXCHANGE_TYPE, true, false);
    rabbitMQ.queue_declare_and_bind_and_consume(LOCAL_QUEUE_1, true, false, false, LOCAL_EXCHANGE, "");
    rabbitMQ.queue_declare_and_bind_and_consume(LOCAL_QUEUE_2, true, false, false, LOCAL_EXCHANGE, "");
    rabbitMQ.close();

    RabbitMQThread rabbitMQThread(USER, PASSWD, HOST_IP, HOST_PORT, LOCAL_CHANNEL_ID);
    rabbitMQThread.set_ratelimit(rate_limit);
    rabbitMQThread.consume_loop();
    rabbitMQThread.close();

    return 0;
}
