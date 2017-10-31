#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <signal.h>
#include <unistd.h>
#include <fstream>

#include "common.h"
#include "names.h"
#include "mqthread.h"
#include "configparser.h"

RabbitMQPublisherThread rabbitMQThread;

static void ctrlc_handler(int)
{
    std::cout << "Rabbitmq Existing!" << std::endl;
    rabbitMQThread.stop();
}

void ctrl_c(void)
{
    signal(SIGINT, ctrlc_handler);    
}

void usage()
{
    std::cout << "Usage: "  << std::endl
              << "       -m message" << std::endl
              << "       -f msg_file" << std::endl
              << "       -r router" << std::endl
              << "       -q queue" << std::endl
              << "       -e exchange" << std::endl
              << "       -R ratelimit" << std::endl
              << "       -s msg_size" << std::endl
              << "       -S send_cnt" << std::endl
              ;
    exit(-1);
}

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    std::string exchange = EXCHANGE_NAME;
    std::string queue = QUEUE_NAME;
    std::string router = ROUTER_NAME;
    std::string msg;
    //std::string msg = "default messge.";
    std::string msgfile;
    int send_cnt = 10000;
    //int send_cnt = 2000000;

    char ch;
    while ((ch = getopt(argc, argv, "hm:r:q:f:e:R:s:S:")) != EOF) {
        switch(ch) {
            case 'h':
                usage();
                break;
            case 'm':
                msg = optarg;
                break;
            case 'r':
                router = optarg;
                break;
            case 'f':
                msgfile = optarg;
                break;
            case 'q':
                queue = optarg;
                break;
            case 'e':
                exchange = optarg; 
                break;
            case 's':
                msg.resize(atoi(optarg), '0');
                break;
            case 'S':
                send_cnt = atoi(optarg);
                break;
            case 'R':
                rate_limit = atoi(optarg);
                break;
            default:
                usage();
                break;
        } 
    }

    if (!msgfile.empty()) {
        std::ifstream in;
        in.open(msgfile.c_str(), std::ios::in);

        if (!in.is_open()) {
            std::cerr << "open file " << msgfile << " failed.\n";        
        } else {
            getline(in, msg);
        }
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

    rabbitMQThread.init(user, passwd, host_ip, host_port, CHANNEL_ID);

    rabbitMQThread.set_ratelimit(rate_limit);
    rabbitMQThread.enable_msg_persistent();


    st_publish_args_t args(exchange, router, msg, send_cnt);

    rabbitMQThread.set_publish_args(&args);
    rabbitMQThread.run();
    rabbitMQThread.join();

    return 0;
}
