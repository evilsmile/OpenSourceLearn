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
    signal(SIGINT, ctrlc_handler);    
    signal(SIGQUIT, ctrlc_handler);    
}

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc > 1) {
        rate_limit = atoi(argv[1]);
    }

    ctrl_c();

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

    std::string msg = "phone=18589060708&clientId=b01221&smsId=a0b3d8b5-554c-4056-ab3a-1b4960c7e60d&userName=aWFuX3Rlc3Q=&content=IOadpeS6hg==&sid=&smsfrom=6&smsType=0&paytype=1&sign=5pGp5oucLui9pg==&userextpendport=&signextendport=&showsigntype=1&accessid=1001552&csid=1008552&csdate=20171020174342&area=97&channelid=3893&salefee=0.000000&costfee=0.200000&ucpaasport=29&operater=2&signport_ex=&userextno_ex=&showsigntype_ex=1&ids=73a641a6-ce60-44c8-8d06-2b79a0c5f00f&process_times=1&oauth_url=&oauth_post_data=&templateid=&channel_tempid=&temp_params=";
    int send_cnt = 2000000;

    rabbitMQ.publish(EXCHANGE_NAME, QUEUE_NAME, ROUTER_NAME, msg, send_cnt);

    rabbitMQ.close();
   
    return 0;
}
