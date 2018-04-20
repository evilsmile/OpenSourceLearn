#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include "common.h"
#include "names.h"
#include "rabbitmq_util.h"
#include "mqthread.h"
#include "configparser.h"

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc < 3) {
        std::cout << "Usage: msg ratelimit" << std::endl;
        return -1;
    }

    std::string msg = argv[1];
    rate_limit = atoi(argv[2]);

    int send_cnt = 10;

#if 0
    RabbitMQ rabbitMQ;
    rabbitMQ.init(USER, PASSWD, HOST_IP, HOST_PORT, LOCAL_CHANNEL_ID);
    rabbitMQ.set_ratelimit(rate_limit);
    rabbitMQ.publish(LOCAL_EXCHANGE, "", msg, send_cnt);
    rabbitMQ.close();
#else

    ConfigParser config_parser;
    if (config_parser.init("../config/config.txt") == false) {
        return -1;
    }

    std::string user = config_parser.getString("rabbitmq_username", "");
    std::string passwd = config_parser.getString("rabbitmq_password", "");
    std::string host_ip = config_parser.getString("rabbitmq_hostip", "");
    int host_port = config_parser.getInt32("rabbitmq_port", -1);

    RabbitMQPublisherThread rabbitMQThread;
    rabbitMQThread.init(user, passwd, host_ip, host_port);

    rabbitMQThread.set_ratelimit(rate_limit);
    st_publish_args_t args(LOCAL_EXCHANGE, "", msg, send_cnt, LOCAL_CHANNEL_ID);

    if (rabbitMQThread.init_publish_args(&args) == false) {
        return -2;
    }

    std::cout << "start running...." << std::endl;
    rabbitMQThread.run();
    rabbitMQThread.join();


#endif
   
    return 0;
}
