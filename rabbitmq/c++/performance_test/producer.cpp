#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <signal.h>

#include "common.h"
#include "names.h"
#include "rabbitmq_util.h"
#include "configparser.h"

RabbitMQ rabbitMQ;

static void ctrlc_handler(int)
{
    rabbitMQ.close();
}

void ctrl_c(void)
{
    signal(SIGTERM, ctrlc_handler);    
}

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc > 1) {
        rate_limit = atoi(argv[1]);
    }

    int send_cnt = SEND_MSG_CNT;

    ConfigParser config_parser;
    if (config_parser.init("../config/config.txt") == false) {
        return -1;
    }

    std::string user = config_parser.getString("rabbitmq_username", "");
    std::string passwd = config_parser.getString("rabbitmq_password", "");
    std::string host_ip = config_parser.getString("rabbitmq_hostip", "");
    int host_port = config_parser.getInt32("rabbitmq_port", -1);

    rabbitMQ.init(user, passwd, host_ip, host_port, CHANNEL_ID);

    rabbitMQ.set_ratelimit(rate_limit);

    std::string msg = "oooo";

    rabbitMQ.publish(EXCHANGE_NAME, QUEUE_NAME, ROUTER_NAME, msg, send_cnt);

    rabbitMQ.close();
   
    return 0;
}
