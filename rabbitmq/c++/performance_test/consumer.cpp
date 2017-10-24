#include "common.h"
#include "names.h"
#include "rabbitmq_util.h"
#include "configparser.h"

#include <unistd.h>

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

    RabbitMQ rabbitMQ(user, passwd, host_ip, host_port, CHANNEL_ID);
    rabbitMQ.set_ratelimit(rate_limit);

    rabbitMQ.exchange_declare(EXCHANGE_NAME, EXCHANGE_TYPE, true, false);

    rabbitMQ.queue_declare_and_bind_and_consume(QUEUE_NAME, true, false, false, EXCHANGE_NAME, ROUTER_NAME);

    rabbitMQ.rabbit_consume_loop();

    rabbitMQ.rabbit_close();

    return 0;
}
