#include "common.h"
#include "rabbitmq_util.h"

#include <unistd.h>

static const std::string LOG_EXCHANGE_NAME = "amq.rabbitmq.log";
static const std::string ERR_QUEUE_NAME = "rmq_error_queue";
static const std::string WARN_QUEUE_NAME = "rmq_warn_queue";
static const std::string INFO_QUEUE_NAME = "rmq_info_queue";
static const int LOG_CHANNEL_ID = 2888;

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc > 1) {
        rate_limit = atoi(argv[1]);
    }

    RabbitMQ rabbitMQ(USER, PASSWD, HOST_IP, HOST_PORT, LOG_CHANNEL_ID);
    rabbitMQ.set_ratelimit(rate_limit);

    rabbitMQ.queue_declare_and_bind_and_consume(ERR_QUEUE_NAME, false, true, false, LOG_EXCHANGE_NAME, "error");
    rabbitMQ.queue_declare_and_bind_and_consume(WARN_QUEUE_NAME, false, true, false, LOG_EXCHANGE_NAME, "warn");
    rabbitMQ.queue_declare_and_bind_and_consume(INFO_QUEUE_NAME, false, true, false, LOG_EXCHANGE_NAME, "info");
    rabbitMQ.consume_loop();

    rabbitMQ.close();

    return 0;
}
