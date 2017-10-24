#ifndef __RABBITMQ_UTIL_H__
#define __RABBITMQ_UTIL_H__

#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>

class RabbitMQ 
{
    public:
        RabbitMQ(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port,
                int channel_id
                );

        void exchange_declare(const std::string& exchange_name,
                const std::string& exchange_type,
                bool durable, 
                bool auto_delete);

        void queue_declare_and_bind_and_consume(const std::string& queue_name,
                bool durable, 
                bool exclusive, 
                bool auto_delete,
                const std::string& exchange_name,
                const std::string& route_key);

        void rabbit_publish(const std::string& exchange_name,
                const std::string& queue_name,
                const std::string& route_key,
                const std::string& msg,
                int msg_cnt);

        void rabbit_consume_loop();

        void rabbit_close();

        void openConsumeAck();
        void closeConsumeAck();

        void set_ratelimit(int rate_limit);

        void check_amqp_reply(const std::string& show_tip);

    private:
        amqp_connection_state_t _conn; 
        int _channel_id;

        int _rate_limit;
        bool _ack_flag;
};

#endif
