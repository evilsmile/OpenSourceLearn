#include "mqthread.h"


RabbitMQThread::RabbitMQThread(const std::string& username, 
                 const std::string& password, 
                 const std::string& hostip, 
                 int port,
                 int channel_id
                 )
    : _channel_id(channel_id)
{
    _set_default_param();
    _conn = amqp_new_connection();

    amqp_socket_t* pSocket = amqp_tcp_socket_new(_conn);
    if (pSocket == NULL) {
        ABORT("amqp create socket failed.");
    }

    int state = amqp_socket_open(pSocket, hostip.c_str(), port);
    if (state < 0)  {
        ABORT("amqp open socket failed.");
    }

    amqp_rpc_reply_t reply = amqp_login(_conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, username.c_str(), password.c_str());
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        ABORT("amqp login failed.");
    }

    amqp_channel_open(_conn, _channel_id);
    check_amqp_reply("amqp open channel failed.");
}

RabbitMQThread::~RabbitMQThread()
{
    close();
}

bool RabbitMQThread::init(const std::string& username, 
                 const std::string& password, 
                 const std::string& hostip, 
                 int port,
                 int channel_id
                 )
{
    _set_default_param();

    _channel_id = channel_id;
    _conn = amqp_new_connection();

    amqp_socket_t* pSocket = amqp_tcp_socket_new(_conn);
    if (pSocket == NULL) {
        ABORT("amqp create socket failed.");
    }

    int state = amqp_socket_open(pSocket, hostip.c_str(), port);
    if (state < 0)  {
        ABORT("amqp open socket failed.");
    }

    amqp_rpc_reply_t reply = amqp_login(_conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, username.c_str(), password.c_str());
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        ABORT("amqp login failed.");
    }

    amqp_channel_open(_conn, _channel_id);
    check_amqp_reply("amqp open channel failed.");

    return true;
}

void RabbitMQThread::_set_default_param(void)
{
    _rate_limit = 5000;
    _ack_flag = true;
    _prefetch_cnt = 1;
    _running = true;
    _ptr_worker_thread = NULL;
}

bool RabbitMQThread::run()
{
    if (pthread_create(&_tid, NULL, thread_start, (void*)this) < 0) {
        std::cerr << "create mqthread failed.\n";
        return false;
    }

    return true;
}

void *RabbitMQThread::thread_start(void *arg)
{
    if (arg == NULL) {
        std::cerr << "arg is NULL" << std::endl;
        return NULL;
    }

    std::cout << "mqthread consume start...." << std::endl;
    RabbitMQThread* ptr_mqthread = (RabbitMQThread*)arg;
    ptr_mqthread->consume_loop();

    return NULL;
}

void RabbitMQThread::set_workthread(Thread *ptr_workthread)
{
    this->_ptr_worker_thread = ptr_workthread;
}

void RabbitMQThread::set_queue_consume(const std::string& queue_name, bool exclusive)
{
    amqp_basic_qos(_conn, 
                   _channel_id,
                   0, /* prefetch_size */
                   _prefetch_cnt, /* prefetch_count */
                   0 /* global */);

    amqp_basic_consume(_conn,        /* connection */
                       _channel_id,  /* channel */
                       amqp_cstring_bytes(queue_name.c_str()),  /* queue */
                       amqp_empty_bytes,     /* consumer_tag */
                       0,       /* no_local */
                       (_ack_flag? 0 : 1),       /* no_ack */
                       exclusive?1:0,       /* exclusive */
                       amqp_empty_table    /* argument */
                       );
    check_amqp_reply("amqp basic consume failed.");
    std::cout << "Queue '" << queue_name << "' basic_consume." << std::endl;
}

void RabbitMQThread::stop()
{
    _running = false;
}

void RabbitMQThread::close()
{
    amqp_channel_close(_conn, CHANNEL_ID, AMQP_REPLY_SUCCESS);
    amqp_connection_close(_conn, AMQP_REPLY_SUCCESS);
    amqp_destroy_connection(_conn);
}

void RabbitMQThread::consume_loop()
{
    std::cout << "rate limit: " << _rate_limit << ", consume_ack: " << (_ack_flag?"true":"false") << std::endl;
    amqp_rpc_reply_t reply;
    
    int recv = 0;
    int previous_recv = 0;
    int recv_this_sec = 0;

    uint64_t start_time = now_us();
    uint64_t start_sec = start_time;
    uint64_t next_sec = start_time + SUMMARY_EVERY_US;

    while (_running) {
        uint64_t now = now_us();
        // 限流
        if (recv_this_sec >= _rate_limit) {
            while (now < next_sec) {
                // 2ms
                microsleep(2000);
                now = now_us();
            }
        }

        if (now >= next_sec) {
            /*
            int countOverInterval = recv - previous_recv;
            previous_recv = recv;
            double intervalRate = countOverInterval / ((now - start_sec) / 1000000.0);
            printf("%d ms: recv %d - %d since last report (%d Hz)\n",
                    (int)(now - start_time)/1000, recv, countOverInterval, (int)intervalRate);
                    */
            recv_this_sec = 0;
            start_sec = now;
            next_sec = start_sec + SUMMARY_EVERY_US;
        }



        amqp_envelope_t envelope;
        memset(&envelope, 0, sizeof(envelope));

        amqp_maybe_release_buffers(_conn);

        struct timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = 20 * 1000; // 20s
        reply = amqp_consume_message(_conn, &envelope, &tv, 0);

        switch(reply.reply_type) {
            case AMQP_RESPONSE_NORMAL:
                break;
            case AMQP_RESPONSE_LIBRARY_EXCEPTION:
                if (reply.library_error == AMQP_STATUS_TIMEOUT) {
                    amqp_destroy_envelope(&envelope);
                    continue;
                } else {
                    std::cerr << "amqp consume library exception\n";
                    return;
                }
            default:
                std::cerr << "amqp consume message failed.\n";
                return;
        }

        std::string data((char*)envelope.message.body.bytes, envelope.message.body.len);

        if (_ptr_worker_thread != NULL) {
            _ptr_worker_thread->add_request(data);
        }

        int channelid = envelope.channel;
        int deliverytag = envelope.delivery_tag;

        /*
        std::cout << "Channel: " << channelid << ", "
            << "DeliveryTag: " << deliverytag << ", "
            << "Exchange: " << std::string((char*)envelope.exchange.bytes, envelope.exchange.len) << ", "
            << "RouteKey: " << std::string((char*)envelope.routing_key.bytes, envelope.routing_key.len) << ", "
            << "Data: " << data 
            << std::endl;
            */

        if (_ack_flag) {
            amqp_basic_ack(_conn, channelid, deliverytag, 0 /* multiple */);
        }

        ++recv_this_sec;
        amqp_destroy_envelope(&envelope);
    }
}

void RabbitMQThread::publish(const std::string& exchange_name,
                       const std::string& queue_name,
                       const std::string& route_key,
                       const std::string& msg,
                       int msg_cnt)
{
    amqp_bytes_t message_bytes;

    message_bytes.len = msg.size();
    message_bytes.bytes = (void*)(msg.c_str());

    int sent = 0;
    int previous_sent = 0;
    int sent_this_sec = 0;

    uint64_t start_time = now_us();
    uint64_t start_sec = start_time;
    uint64_t next_sec = start_time + SUMMARY_EVERY_US;

    std::cout << "rate_limit: " << _rate_limit << std::endl;
    for (int i = 0; _running && i < msg_cnt; ++i) {

        uint64_t now = now_us();

        // 限流
        if (sent_this_sec >= _rate_limit) {
            while (now < next_sec) {
                // 2ms
                microsleep(2000);
                now = now_us();
            }
        }

        if (now >= next_sec) {
            /*
            int countOverInterval = sent - previous_sent;
            previous_sent = sent;
            double intervalRate = countOverInterval / ((now - start_sec) / 1000000.0);
            printf("%d ms: sent %d - %d since last report (%d Hz)\n",
                    (int)(now - start_time)/1000, sent, countOverInterval, (int)intervalRate);
                    */
            sent_this_sec = 0;
            start_sec = now;
            next_sec = start_sec + SUMMARY_EVERY_US;
        }

        amqp_basic_publish(_conn,                          /* state */
                           CHANNEL_ID,                    /* channel */
                           amqp_cstring_bytes(exchange_name.c_str()), /* exchange */
                           amqp_cstring_bytes(route_key.c_str()),   /* routekey */
                           0,                /* mandatory */
                           0,                /* immediate */
                           NULL,             /* properties */
                           message_bytes     /* body */
                           );
        check_amqp_reply("amqp basic publish");
        ++sent;
        ++sent_this_sec;
    }

    {
        uint64_t stop_time = now_us();
        uint64_t total_delta = (int)(stop_time - start_time);

        printf("PRODUCER - Message count: %d\n", sent);
        printf("Total time, milliseconds: %lu\n", total_delta/1000);
        printf("Overall messages-per-second: %g\n", (sent/(total_delta / 1000000.0)));
    }
}

bool RabbitMQThread::join()
{
    pthread_join(_tid, NULL);
}

void RabbitMQThread::enable_consume_ack()
{
    _ack_flag = true;
}

void RabbitMQThread::disable_consume_ack()
{
    _ack_flag = false;
}

void RabbitMQThread::set_ratelimit(int rate_limit)
{
    _rate_limit = rate_limit;
}

void RabbitMQThread::set_prefetchcnt(uint32_t prefetch_count)
{
    _prefetch_cnt = prefetch_count;
}

void RabbitMQThread::check_amqp_reply(const std::string& show_tip)
{
    amqp_rpc_reply_t reply = amqp_get_rpc_reply(_conn);

    switch (reply.reply_type) {
        case AMQP_RESPONSE_NORMAL:
            return;

        case AMQP_RESPONSE_NONE:
            fprintf(stderr, "%s: missing RPC reply type!\n", show_tip.c_str());
            break;

        case AMQP_RESPONSE_LIBRARY_EXCEPTION:
            fprintf(stderr, "%s: %s\n", show_tip.c_str(), amqp_error_string2(reply.library_error));
            break;

        case AMQP_RESPONSE_SERVER_EXCEPTION:
            switch (reply.reply.id) {
                case AMQP_CONNECTION_CLOSE_METHOD: 
                    {
                        amqp_connection_close_t *m = (amqp_connection_close_t *) reply.reply.decoded;
                        fprintf(stderr, "%s: server connection error %uh, message: %.*s\n",
                                show_tip.c_str(),
                                m->reply_code,
                                (int) m->reply_text.len, (char *) m->reply_text.bytes);
                        break;
                    }
                case AMQP_CHANNEL_CLOSE_METHOD: 
                    {
                        amqp_channel_close_t *m = (amqp_channel_close_t *) reply.reply.decoded;
                        fprintf(stderr, "%s: server channel error %uh, message: %.*s\n",
                                show_tip.c_str(),
                                m->reply_code,
                                (int) m->reply_text.len, (char *) m->reply_text.bytes);
                        break;
                    }
                default:
                    fprintf(stderr, "%s: unknown server error, method id 0x%08X\n", show_tip.c_str(), reply.reply.id);
                    break;
            }
            break;          
    }
    ABORT(show_tip);
}

