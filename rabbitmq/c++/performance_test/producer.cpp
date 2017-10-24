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

    RabbitMQ rabbitMQ(USER, PASSWD, HOST_IP, HOST_PORT, CHANNEL_ID);
    rabbitMQ.set_ratelimit(rate_limit);

    std::string msg = "oooo";

    rabbitMQ.rabbit_publish(EXCHANGE_NAME, QUEUE_NAME, ROUTER_NAME, msg, send_cnt);

    rabbitMQ.rabbit_close();
   
    return 0;
}
