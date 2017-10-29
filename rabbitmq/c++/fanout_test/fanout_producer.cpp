#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include "common.h"
#include "names.h"
#include "rabbitmq_util.h"
#include "mqthread.h"

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc < 3) {
        std::cout << "Usage: router_key msg" << std::endl;
        return -1;
    }

    std::string router_key = argv[1];
    std::string msg = argv[2];

    int send_cnt = SEND_MSG_CNT;

    RabbitMQThread rabbitMQThread(USER, PASSWD, HOST_IP, HOST_PORT, LOCAL_CHANNEL_ID);
    rabbitMQThread.set_ratelimit(rate_limit);

    rabbitMQThread.publish(LOCAL_EXCHANGE, "", router_key, msg, send_cnt);

    rabbitMQThread.close();
   
    return 0;
}
