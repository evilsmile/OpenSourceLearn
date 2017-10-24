#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include "common.h"
#include "names.h"
#include "rabbitmq_util.h"

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc > 1) {
        rate_limit = atoi(argv[1]);
    }

    int send_cnt = SEND_MSG_CNT;

    amqp_connection_state_t conn;

    rabbit_init(conn, USER, PASSWD, HOST_IP, HOST_PORT, CHANNEL_ID);

    rabbit_publish(conn, EXCHANGE_NAME, QUEUE_NAME, ROUTER_NAME, CHANNEL_ID, rate_limit, send_cnt);

    rabbit_close(conn);
   
    return 0;
}
