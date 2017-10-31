#ifndef __RABBITMQ_THREAD_H__
#define __RABBITMQ_THREAD_H__

#include <string>
#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>
#include "thread.h"

#include "common.h"

enum Role {
    PUBLIHSER,
    CONSUMER
};

class RabbitMQThreadBase
{
    public:
        RabbitMQThreadBase();

        ~RabbitMQThreadBase();

        RabbitMQThreadBase(const std::string& username, 
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
        void join();

        static void* thread_start(void *arg);

        void close();

        void check_amqp_reply(const std::string& show_tip);

        void set_ratelimit(int rate_limit);

        void set_thread_role(Role role);

        void stop();

        virtual void loop_handler() = 0;
        
    protected:
        virtual void _set_default_param();

    protected:
        std::string _name;
        pthread_t _tid;
        bool _inited;

        amqp_connection_state_t _conn; 
        int _channel_id;
        std::string _str_channel_id;

        bool _running;

        int _rate_limit;

        static Role _role;
};

//////////////////////////////////// Consumer ////////////////////////////
class RabbitMQConsumerThread : public RabbitMQThreadBase
{
    public:
        RabbitMQConsumerThread(){}
        RabbitMQConsumerThread(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port,
                int channel_id
                );

        void set_workthread(Thread *ptr_workthread);
        void set_queue_consume(const std::string& queue_name, bool ack, bool exclusive);

        void cancel_consume(void);

    protected:
        virtual void loop_handler();

        void set_prefetchcnt(uint32_t prefetch_size);

        virtual void _set_default_param();

    private:
        Thread *_ptr_worker_thread;
        bool _ack_flag;

        uint32_t _prefetch_cnt;
};

//////////////////////////////// Publisher ///////////////////////////////
typedef struct _st_publish_args {
    _st_publish_args(const std::string& exchange_name,
                     const std::string& route_key,
                     const std::string& msg,
                     int msg_cnt) 
                  : exchange_name(exchange_name),
                    route_key(route_key),
                    msg(msg),
                    msg_cnt(msg_cnt)
    {
    }

    std::string exchange_name;
    std::string route_key;
    std::string msg;
    int msg_cnt;
} st_publish_args_t;


class RabbitMQPublisherThread : public RabbitMQThreadBase
{
    public:
        RabbitMQPublisherThread(){}
        RabbitMQPublisherThread(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port,
                int channel_id
                );

        void enable_msg_persistent();
        void disable_msg_persistent();

        void set_publish_args(st_publish_args_t* publish_args);
        virtual void loop_handler();

    protected:
        virtual void _set_default_param();

    private:
        st_publish_args_t* _p_publish_args;
        bool _msg_persistent;

};

#endif
