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
        std::cout << "Usage: msg ratelimit" << std::endl;
        return -1;
    }

    std::string msg = argv[1];
    rate_limit = atoi(argv[2]);

    int send_cnt = SEND_MSG_CNT;

#if 0
    RabbitMQ rabbitMQ;
    rabbitMQ.init(USER, PASSWD, HOST_IP, HOST_PORT, LOCAL_CHANNEL_ID);
    rabbitMQ.set_ratelimit(rate_limit);
    rabbitMQ.publish(LOCAL_EXCHANGE, "", msg, send_cnt);
    rabbitMQ.close();
#else
    // 使用新线程发送
    RabbitMQPublisherThread rabbitMQThread;
    rabbitMQThread.init(USER, PASSWD, HOST_IP, HOST_PORT, LOCAL_CHANNEL_ID);
    rabbitMQThread.set_ratelimit(rate_limit);

    st_publish_args_t args;
    args.msg_cnt = send_cnt;
    args.msg = msg;
    args.route_key = "";
    args.exchange_name = LOCAL_EXCHANGE;
    rabbitMQThread.set_thread_role(PUBLIHSER);
    rabbitMQThread.set_publish_args(&args);
    rabbitMQThread.run();
    rabbitMQThread.join();

#endif
   
    return 0;
}
