#include "common.h"
#include "names.h"
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

    RabbitMQ rabbitMQ(USER, PASSWD, HOST_IP, HOST_PORT, LOCAL_CHANNEL_ID);
    rabbitMQ.exchange_declare(LOCAL_EXCHANGE, LOCAL_EXCHANGE_TYPE, true, false);
    rabbitMQ.queue_declare_and_bind_and_consume(LOCAL_QUEUE_1, true, false, false, LOCAL_EXCHANGE, "");
    rabbitMQ.queue_declare_and_bind_and_consume(LOCAL_QUEUE_2, true, false, false, LOCAL_EXCHANGE, "");
    rabbitMQ.close();

    MyWorkThread myWorker("fanout_worker");
    myWorker.start();

    RabbitMQThread rabbitMQThread(USER, PASSWD, HOST_IP, HOST_PORT, LOCAL_CHANNEL_ID);
    rabbitMQThread.set_queue_consume(LOCAL_QUEUE_1, true);
    rabbitMQThread.set_queue_consume(LOCAL_QUEUE_2, true);
    rabbitMQThread.set_ratelimit(rate_limit);
    rabbitMQThread.set_workthread(&myWorker);
    rabbitMQThread.run();
    rabbitMQThread.join();

    return 0;
}
