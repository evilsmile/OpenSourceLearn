#include "common.h"
#include "names.h"
#include "configparser.h"
#include "rabbitmq_util.h"
#include "mqthread.h"
#include "thread.h"

class MyWorkThread : public Thread 
{
    public:
        MyWorkThread(const std::string& name) : Thread(name) {}

        virtual bool handle(data_ptr_t req) {
            std::cout << "request: " << *req << std::endl;

            return true;
        }

};

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


    RabbitMQ rabbitMQ(user, passwd, host_ip, host_port, LOCAL_CHANNEL_ID);
    rabbitMQ.exchange_declare(LOCAL_EXCHANGE_1, LOCAL_EXCHANGE_TYPE, true, false);
    rabbitMQ.exchange_declare(LOCAL_EXCHANGE_2, LOCAL_EXCHANGE_TYPE, true, false);
    rabbitMQ.exchange_bind(LOCAL_EXCHANGE_1, LOCAL_EXCHANGE_2, "e2e_key");
    rabbitMQ.queue_declare_and_bind(LOCAL_QUEUE, true, false, false, LOCAL_EXCHANGE_2, "");

#if 0
    MyWorkThread myWorker("headers_worker");
    myWorker.start();

    RabbitMQConsumerThread rabbitMQThread;
    rabbitMQThread.init(user, passwd, host_ip, host_port);
    rabbitMQThread.set_ratelimit(rate_limit);
    rabbitMQThread.set_queue_consume(LOCAL_QUEUE, true /* ack */, true /* exclusive */, LOCAL_CHANNEL_ID);
    rabbitMQThread.set_workthread(&myWorker);
    rabbitMQThread.run();
    rabbitMQThread.join();
#endif

    /*
    sleep(10);
    rabbitMQ.exchange_unbind(LOCAL_EXCHANGE_1, LOCAL_EXCHANGE_2, "e2e_key");
    */
    rabbitMQ.close();

    return 0;
}
