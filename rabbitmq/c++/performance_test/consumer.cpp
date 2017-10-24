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

    amqp_connection_state_t conn;

    rabbit_init(conn, USER, PASSWD, HOST_IP, HOST_PORT, CHANNEL_ID);

    exchange_declare(conn, EXCHANGE_NAME, EXCHANGE_TYPE, true, false, CHANNEL_ID);

    queue_declare_and_bind_and_consume(conn, QUEUE_NAME, true, false, false, CHANNEL_ID, EXCHANGE_NAME, ROUTER_NAME);

    rabbit_consume_loop(conn, rate_limit);

    rabbit_close(conn);

    return 0;
}
