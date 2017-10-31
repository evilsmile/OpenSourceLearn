#ifndef __RABBITMQ_UTIL_H__
#define __RABBITMQ_UTIL_H__

#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>
#include "thread.h"

class RabbitMQ 
{
    public:
        RabbitMQ(){}

        RabbitMQ(const std::string& username, 
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


        void exchange_declare(const std::string& exchange_name,
                const std::string& exchange_type,
                bool durable, 
                bool auto_delete);

        void queue_declare_and_bind(const std::string& queue_name,
                bool durable, 
                bool exclusive, 
                bool auto_delete,
                const std::string& exchange_name,
                const std::string& route_key);

        void publish(const std::string& exchange_name,
                const std::string& route_key,
                const std::string& msg,
                int msg_cnt);

        void consume_loop(Thread* ptr_work_thread = NULL);

        void close();

        void check_amqp_reply(const std::string& show_tip);

        // Adjust params
        void enable_consume_ack();
        void disable_consume_ack();

        void set_ratelimit(int rate_limit);

        void set_prefetchcnt(uint32_t prefetch_size);

        void set_job_time_ms(uint32_t ms);

        void stop();
        
    private:
        void _set_default_param();

    private:
        amqp_connection_state_t _conn; 
        int _channel_id;

        bool _running;

        int _rate_limit;
        bool _ack_flag;
        uint32_t _prefetch_cnt;

        uint32_t _fake_job_time_ms;
};

#endif
