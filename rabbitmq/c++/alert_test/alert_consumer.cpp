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

    RabbitMQ rabbitMQ(USER, PASSWD, HOST_IP, HOST_PORT, ALERT_CHANNEL_ID);
    rabbitMQ.set_ratelimit(rate_limit);

    rabbitMQ.exchange_declare(ALERT_EXCHANGE, ALERT_EXCHANGE_TYPE, true, false);

    rabbitMQ.queue_declare_and_bind_and_consume(CRITICAL_QUEUE, false, true, false, ALERT_EXCHANGE, CRITICAL_ROUTER);
    //rabbitMQ.queue_declare_and_bind_and_consume(RATING_QUEUE, false, true, false, ALERT_EXCHANGE, RATING_ROUTER);

    rabbitMQ.rabbit_consume_loop();

    rabbitMQ.rabbit_close();

    return 0;
}
