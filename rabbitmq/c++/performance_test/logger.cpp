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

    amqp_connection_state_t conn;

    rabbit_init(conn, USER, PASSWD, HOST_IP, HOST_PORT, LOG_CHANNEL_ID);

    queue_declare_and_bind_and_consume(conn, ERR_QUEUE_NAME, false, true, false, LOG_CHANNEL_ID, LOG_EXCHANGE_NAME, "error");
    queue_declare_and_bind_and_consume(conn, WARN_QUEUE_NAME, false, true, false, LOG_CHANNEL_ID, LOG_EXCHANGE_NAME, "warn");
    queue_declare_and_bind_and_consume(conn, INFO_QUEUE_NAME, false, true, false, LOG_CHANNEL_ID, LOG_EXCHANGE_NAME, "info");
    rabbit_consume_loop(conn, rate_limit);

    rabbit_close(conn);

    return 0;
}
