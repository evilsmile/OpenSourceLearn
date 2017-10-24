#include "common.h"
#include "names.h"
#include "rabbitmq_util.h"

#include <unistd.h>

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc > 1) {
        rate_limit = atoi(argv[1]);
    }

    RabbitMQ rabbitMQ(USER, PASSWD, HOST_IP, HOST_PORT, CHANNEL_ID);
    rabbitMQ.set_ratelimit(rate_limit);

    rabbitMQ.exchange_declare(EXCHANGE_NAME, EXCHANGE_TYPE, true, false);

    rabbitMQ.queue_declare_and_bind_and_consume(QUEUE_NAME, true, false, false, EXCHANGE_NAME, ROUTER_NAME);

    rabbitMQ.rabbit_consume_loop();

    rabbitMQ.rabbit_close();

    return 0;
}
