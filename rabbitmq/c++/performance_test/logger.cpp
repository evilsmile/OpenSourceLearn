#include "common.h"
#include "rabbitmq_util.h"
#include "mqthread.h"
#include "thread.h"
#include "configparser.h"

#include <unistd.h>

static const std::string LOG_EXCHANGE_NAME = "amq.rabbitmq.log";
static const std::string ERR_QUEUE_NAME = "rmq_error_queue";
static const std::string WARN_QUEUE_NAME = "rmq_warn_queue";
static const std::string INFO_QUEUE_NAME = "rmq_info_queue";
static const int LOG_CHANNEL_ID = 2888;

RabbitMQConsumerThread rabbitMQThread;
WorkThread work_thread("message_handler_thread");

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc > 1) {
        rate_limit = atoi(argv[1]);
    }

    ConfigParser config_parser;
    if (config_parser.init("../config/config.txt") == false) {
        return -1;
    }

    std::string user = config_parser.getString("rabbitmq_username", "");
    std::string passwd = config_parser.getString("rabbitmq_password", "");
    std::string host_ip = config_parser.getString("rabbitmq_hostip", "");
    int host_port = config_parser.getInt32("rabbitmq_port", -1);


    RabbitMQ rabbitMQ(USER, PASSWD, HOST_IP, HOST_PORT, LOG_CHANNEL_ID);

    rabbitMQ.queue_declare_and_bind(ERR_QUEUE_NAME, true, false, false, LOG_EXCHANGE_NAME, "error");
    rabbitMQ.queue_declare_and_bind(WARN_QUEUE_NAME, true, false, false, LOG_EXCHANGE_NAME, "warn");
    rabbitMQ.queue_declare_and_bind(INFO_QUEUE_NAME, true, false, false, LOG_EXCHANGE_NAME, "info");
    rabbitMQ.close();

    rabbitMQThread.init(user, passwd, host_ip, host_port);
    rabbitMQThread.set_ratelimit(rate_limit);
    //rabbitMQThread.set_queue_consume(queue, true /* ack */, true /* exclusive */);
    rabbitMQThread.set_queue_consume(ERR_QUEUE_NAME, false /* ack */, true /* exclusive */,  LOG_CHANNEL_ID);
    rabbitMQThread.set_queue_consume(WARN_QUEUE_NAME, false /* ack */, true /* exclusive */, LOG_CHANNEL_ID);
    rabbitMQThread.set_queue_consume(INFO_QUEUE_NAME, false /* ack */, true /* exclusive */, LOG_CHANNEL_ID);
    rabbitMQThread.set_workthread(&work_thread);
    rabbitMQThread.run();
    rabbitMQThread.join();

    return 0;
}
