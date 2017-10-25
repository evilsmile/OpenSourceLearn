#include "common.h"
#include "names.h"
#include "mqthread.h"
#include "rabbitmq_util.h"
#include "configparser.h"

#include <signal.h>
#include <unistd.h>

RabbitMQThread rabbitMQThread;
RabbitMQ rabbitMQ;

int rate_limit = RATE_LIMIT;
std::string exchange = EXCHANGE_NAME;
std::string queue = QUEUE_NAME;
std::string router = ROUTER_NAME;

static void ctrlc_handler(int)
{
    std::cout << "Rabbitmq Existing!" << std::endl;
    rabbitMQThread.stop();
}

void ctrl_c(void)
{
    signal(SIGTERM, ctrlc_handler);    
    signal(SIGINT, ctrlc_handler);    
    signal(SIGQUIT, ctrlc_handler);    
}

void usage()
{
    std::cout << "Usage: "  << std::endl
              << "       -r router" << std::endl
              << "       -q queue" << std::endl
              << "       -e exchange" << std::endl
              << "       -R ratelimit" << std::endl
              ;
    exit(-1);
}

void handle_args(int argc, char *argv[])
{
    char ch;
    while ((ch = getopt(argc, argv, "hr:q:e:R:")) != EOF) {
        switch(ch) {
            case 'h':
                usage();
                break;
            case 'r':
                router = optarg;
                break;
            case 'q':
                queue = optarg;
                break;
            case 'e':
                exchange = optarg; 
                break;
            case 'R':
                rate_limit = atoi(optarg);
                break;
            default:
                usage();
                break;
        } 
    }
}

int main(int argc, char *argv[])
{
    ctrl_c();

    handle_args(argc, argv);

    ConfigParser config_parser;
    if (config_parser.init("../config/config.txt") == false) {
        return -1;
    }

    WorkThread work_thread("message_handler_thread");
    work_thread.start();

    std::string user = config_parser.getString("rabbitmq_username", "");
    std::string passwd = config_parser.getString("rabbitmq_password", "");
    std::string host_ip = config_parser.getString("rabbitmq_hostip", "");
    int host_port = config_parser.getInt32("rabbitmq_port", -1);

    // create queue if needed
    /*
    rabbitMQ.init(user, passwd, host_ip, host_port, CHANNEL_ID);
    rabbitMQ.exchange_declare(exchange, EXCHANGE_TYPE, true, false);
    rabbitMQ.queue_declare_and_bind_and_consume(queue, true, false, false, exchange, router);
    rabbitMQ.close();
    */

    rabbitMQThread.init(user, passwd, host_ip, host_port, CHANNEL_ID);
    rabbitMQThread.set_ratelimit(rate_limit);
    rabbitMQThread.set_queue_consume(queue, true);
    rabbitMQThread.set_workthread(&work_thread);
    rabbitMQThread.run();
    rabbitMQThread.join();

    return 0;
}
