#ifndef __RABBITMQ_THREAD_H__
#define __RABBITMQ_THREAD_H__

#include <string>
#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>
#include "thread.h"

#include "common.h"

class RabbitMQThread
{
    public:
        RabbitMQThread(){}

        ~RabbitMQThread();

        RabbitMQThread(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port,
                int channel_id
                );

        bool init(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port,
                int channel_id
                );

        bool run();
        bool join();

        static void* thread_start(void *arg);

        void publish(const std::string& exchange_name,
                const std::string& queue_name,
                const std::string& route_key,
                const std::string& msg,
                int msg_cnt);

        void set_workthread(Thread *ptr_workthread);

        void set_queue_consume(const std::string& queue_name, bool exclusive);

        void consume_loop();

        void close();

        void check_amqp_reply(const std::string& show_tip);

        // Adjust params
        void enable_consume_ack();
        void disable_consume_ack();

        void set_ratelimit(int rate_limit);

        void set_prefetchcnt(uint32_t prefetch_size);

        void stop();
        
    private:
        void _set_default_param();

    private:
        std::string _name;
        pthread_t _tid;
        Thread *_ptr_worker_thread;

        amqp_connection_state_t _conn; 
        int _channel_id;

        bool _running;

        int _rate_limit;
        bool _ack_flag;
        uint32_t _prefetch_cnt;

        uint32_t _fake_job_time_ms;
};

#endif