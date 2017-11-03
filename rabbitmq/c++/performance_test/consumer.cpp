#include "common.h"
#include "names.h"
#include "mqthread.h"
#include "rabbitmq_util.h"
#include "configparser.h"

#include <signal.h>
#include <unistd.h>

RabbitMQConsumerThread rabbitMQThread;
RabbitMQ rabbitMQ;
WorkThread work_thread("message_handler_thread");

int rate_limit = RATE_LIMIT;
std::string exchange = EXCHANGE_NAME;
std::string queue = QUEUE_NAME;
std::string router = ROUTER_NAME;
std::string queue2 = QUEUE_NAME2;
std::string router2 = ROUTER_NAME2;

static void ctrlc_handler(int)
{
    std::cout << "Rabbitmq Existing!" << std::endl;
    rabbitMQThread.stop();
    work_thread.stop();
}

void ctrl_c(void)
{
    signal(SIGINT, ctrlc_handler);    
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

    std::string user = config_parser.getString("rabbitmq_username", "");
    std::string passwd = config_parser.getString("rabbitmq_password", "");
    std::string host_ip = config_parser.getString("rabbitmq_hostip", "");
    int host_port = config_parser.getInt32("rabbitmq_port", -1);

#if 0
    // create queue if needed
    rabbitMQ.init(user, passwd, host_ip, host_port, CHANNEL_ID);
    rabbitMQ.exchange_declare(exchange, EXCHANGE_TYPE, true, false);
    rabbitMQ.queue_declare_and_bind(queue, true, false, false, exchange, router);
    rabbitMQ.queue_declare_and_bind(queue2, true, false, false, exchange, router2);
#else
    work_thread.start();

    rabbitMQThread.init(user, passwd, host_ip, host_port);
    rabbitMQThread.set_ratelimit(rate_limit);
#if 1
    rabbitMQThread.set_queue_consume(queue, false /* ack */, true /* exclusive */, CHANNEL_ID);
    rabbitMQThread.set_queue_consume(queue2, false /* ack */, true /* exclusive */, CHANNEL_ID2);
    rabbitMQThread.set_workthread(&work_thread);
    rabbitMQThread.run();

    /*
    usleep(20);
    ptr_base_req_t req(new PauseconsumerReq(MQ_CMD_PAUSE_CONSUMER, CHANNEL_ID));
    rabbitMQThread.add_request(req);
    usleep(20);
    req.reset(new PauseconsumerReq(MQ_CMD_PAUSE_CONSUMER, CHANNEL_ID2));
    rabbitMQThread.add_request(req);

    usleep(2 * 60 * 1000 * 1000);
    req.reset(new ResumeconsumerReq(MQ_CMD_RESUME_CONSUMER, CHANNEL_ID));
    rabbitMQThread.add_request(req);
    usleep(20 * 1000 * 1000);
    req.reset(new ResumeconsumerReq(MQ_CMD_RESUME_CONSUMER, CHANNEL_ID2));
    rabbitMQThread.add_request(req);
    */

    rabbitMQThread.join();

#else
    rabbitMQThread.get_one_msg(queue, false /* ack */);
#endif  /* get or consume mode */

#endif /* declare or run queue */

    return 0;
}
