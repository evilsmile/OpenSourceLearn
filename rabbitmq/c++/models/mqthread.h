#ifndef __RABBITMQ_THREAD_H__
#define __RABBITMQ_THREAD_H__

#include <string>
#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>
#include "thread.h"

#include "common.h"
#include <set>

enum Role {
    PUBLIHSER,
    CONSUMER
};

enum MQ_CMD {
    MQ_CMD_PAUSE_CONSUMER = 0,
    MQ_CMD_RESUME_CONSUMER = 1,
};


struct BaseReq {
    BaseReq(MQ_CMD cmd) 
        : cmd(cmd)
    {
    }
    virtual ~BaseReq(){}

    MQ_CMD cmd;
};

struct PauseconsumerReq : public BaseReq {
    PauseconsumerReq(MQ_CMD cmd, int channel_id) 
          : BaseReq(cmd), 
            channel_id(channel_id)
    {
    }

    int channel_id;
};

struct ResumeconsumerReq : public BaseReq {
    ResumeconsumerReq(MQ_CMD cmd, int channel_id) 
          : BaseReq(cmd), 
            channel_id(channel_id)
    {
    }

    int channel_id;
};

typedef boost::shared_ptr<BaseReq> ptr_base_req_t;
typedef std::queue<ptr_base_req_t> ptr_requests_t;

class RabbitMQThreadBase
{
    public:
        RabbitMQThreadBase();

        ~RabbitMQThreadBase();

        RabbitMQThreadBase(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port
                );

        bool init(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port
                );

        bool purge_queue(const std::string& queue, int channel_id);

        bool run();
        void join();

        static void* thread_start(void *arg);

        virtual void close_all();

        bool open_channel(int);
        bool close_channel(int);

        void check_amqp_reply(const std::string& show_tip);

        void set_ratelimit(int rate_limit);

        void set_thread_role(Role role);

        int get_msg_count_from_mq(const std::string& queue);

        void stop();

        void main_loop();

        virtual void req_handler(ptr_base_req_t){}
        virtual bool mq_loop_handler() = 0;

        void add_request(ptr_base_req_t req);
        ptr_base_req_t get_request();
        
    protected:
        virtual void _set_default_param();

    protected:
        ptr_requests_t _requests;

        std::set<int> _channel_ids;
        std::string _name;
        pthread_t _tid;
        bool _inited;

        amqp_connection_state_t _conn; 

        bool _running;

        int _rate_limit;

        int64_t _start_time;

        static Role _role;
};

//////////////////////////////////// Consumer ////////////////////////////
// 在暂停和恢复的时候需要
struct ConsumeInfoItem {
    ConsumeInfoItem(std::string queue_name, bool ack, bool exclusive)
          : queue_name(queue_name), ack(ack), exclusive(exclusive)
    {
    }

    std::string queue_name;
    bool ack;
    bool exclusive;
};

typedef std::map<int, ConsumeInfoItem> consume_info_list_t;

class RabbitMQConsumerThread : public RabbitMQThreadBase
{
    public:
        RabbitMQConsumerThread(){}
        RabbitMQConsumerThread(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port
                );

        void set_workthread(Thread *ptr_workthread);
        bool set_queue_consume(const std::string& queue_name, bool ack, bool exclusive, int channel_id);

        bool pause_consume(ptr_base_req_t);
        bool resume_consume(ptr_base_req_t req);

        virtual void req_handler(ptr_base_req_t);

        bool cancel_consume(int);

        void recover_consume(int, bool requeue);

        bool reject_msg(int, uint64_t delivery_tag, bool requeue);
        bool nack_msg(int, uint64_t delivery_tag, bool multiple, bool requeue);

        std::string get_one_msg(const std::string& queue_name, bool ack);

    protected:
        virtual bool mq_loop_handler();

        void set_prefetchcnt(uint32_t prefetch_size);

        virtual void _set_default_param();

    private:
        Thread *_ptr_worker_thread;
        consume_info_list_t _consume_info_list;

        uint32_t _prefetch_cnt;
};

//////////////////////////////// Publisher ///////////////////////////////
typedef struct _st_publish_args {
    _st_publish_args(const std::string& exchange_name,
                     const std::string& route_key,
                     const std::string& msg,
                     int msg_cnt,
                     int channel_id) 
                  : exchange_name(exchange_name),
                    route_key(route_key),
                    msg(msg),
                    msg_cnt(msg_cnt),
                    channel_id(channel_id)
    {
    }

    std::string exchange_name;
    std::string route_key;
    std::string msg;
    int msg_cnt;
    int channel_id;
} st_publish_args_t;


class RabbitMQPublisherThread : public RabbitMQThreadBase
{
    public:
        RabbitMQPublisherThread(){}
        RabbitMQPublisherThread(const std::string& username, 
                const std::string& password, 
                const std::string& hostip, 
                int port
                );

        void enable_msg_persistent();
        void disable_msg_persistent();

        bool init_publish_args(st_publish_args_t* publish_args);
        virtual bool mq_loop_handler();

    protected:
        virtual void _set_default_param();

    private:
        st_publish_args_t* _p_publish_args;
        amqp_bytes_t _msg_bytes;
        int _sent;

        bool _msg_persistent;

};

#endif
