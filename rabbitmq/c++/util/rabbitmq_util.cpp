#include "common.h"
#include "rabbitmq_util.h"

static bool bConsumeAck = true;

void openConsumeAck()
{
    bConsumeAck = true;
}

void closeConsumeAck()
{
    bConsumeAck = false;
}

void rabbit_init(amqp_connection_state_t& conn, 
                 const std::string& username, 
                 const std::string& password, 
                 const std::string& hostip, 
                 int port,
                 int channel_id
                 )
{
    conn = amqp_new_connection();

    amqp_socket_t* pSocket = amqp_tcp_socket_new(conn);
    if (pSocket == NULL) {
        ABORT("amqp create socket failed.");
    }

    int state = amqp_socket_open(pSocket, hostip.c_str(), port);
    if (state < 0)  {
        ABORT("amqp open socket failed.");
    }

    amqp_rpc_reply_t reply = amqp_login(conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, username.c_str(), password.c_str());
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        ABORT("amqp login failed.");
    }

    amqp_channel_open(conn, channel_id);
    check_amqp_reply(conn, "amqp open channel failed.");
}

void exchange_declare(amqp_connection_state_t& conn, 
                      const std::string& exchange_name,
                      const std::string& exchange_type,
                      bool durable, 
                      bool auto_delete,
                      int channel_id
        )
{

    amqp_exchange_declare(conn, /* state */
                          channel_id,    /* channel */
                          amqp_cstring_bytes(exchange_name.c_str()),  /* exchange */
                          amqp_cstring_bytes(exchange_type.c_str()),  /* type */
                          0,          /* passive */
                          durable?1:0,          /* durable */
                          auto_delete?1:0,          /* auto_delete */
                          0,    /* internal */
                          amqp_empty_table);
    check_amqp_reply(conn, "amqp declare exchange failed.");
    std::cout << "Exchange '" << exchange_name << "' declared." << std::endl;
}

void queue_declare_and_bind_and_consume(amqp_connection_state_t& conn, 
                      const std::string& queue_name,
                      bool durable, 
                      bool exclusive, 
                      bool auto_delete,
                      int channel_id,
                      const std::string& exchange_name,
                      const std::string& route_key
        )
{
     amqp_queue_declare(conn,              /* state */
                       channel_id,        /* channel */
                       amqp_cstring_bytes(queue_name.c_str()),  /* queue */
                       0,       /* passive */
                       durable?1:0,       /* durable */
                       exclusive?1:0,       /* exclusive */
                       auto_delete?1:0,       /* auto_delete */
                       amqp_empty_table);
    check_amqp_reply(conn, "Declaring queue");
    std::cout << "Queue '" << queue_name << "' declared." << std::endl;
    amqp_queue_bind(conn,
                    channel_id,
                    amqp_cstring_bytes(queue_name.c_str()),
                    amqp_cstring_bytes(exchange_name.c_str()),
                    amqp_cstring_bytes(route_key.c_str()),
                    amqp_empty_table);
    check_amqp_reply(conn, "amqp bind queue failed.");
    std::cout << "Queue '" << queue_name << "' binded." << std::endl;

    amqp_basic_qos(conn, channel_id, 0, 1, 0);

    amqp_basic_consume(conn,        /* connection */
                       channel_id,  /* channel */
                       amqp_cstring_bytes(queue_name.c_str()),  /* queue */
                       amqp_empty_bytes,     /* consumer_tag */
                       0,       /* no_local */
                       (bConsumeAck? 0 : 1),       /* no_ack */
                       exclusive?1:0,       /* exclusive */
                       amqp_empty_table    /* argument */
                       );
    check_amqp_reply(conn, "amqp basic consume failed.");
    std::cout << "Queue '" << queue_name << "' basic_consume." << std::endl;
}

void rabbit_close(amqp_connection_state_t& conn)
{
    amqp_channel_close(conn, CHANNEL_ID, AMQP_REPLY_SUCCESS);
    amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
    amqp_destroy_connection(conn);
}

void rabbit_consume_loop(amqp_connection_state_t& conn, int rate_limit)
{
    std::cout << "rate limit: " << rate_limit << ", consume_ack: " << (bConsumeAck?"true":"false") << std::endl;
    amqp_rpc_reply_t reply;
    
    int recv = 0;
    int previous_recv = 0;
    int recv_this_sec = 0;

    uint64_t start_time = now_us();
    uint64_t start_sec = start_time;
    uint64_t next_sec = start_time + SUMMARY_EVERY_US;

    while (true) {
        uint64_t now = now_us();
        // 限流
        if (recv_this_sec >= rate_limit) {
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

        amqp_maybe_release_buffers(conn);

        struct timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = 20 * 1000; // 20s
        reply = amqp_consume_message(conn, &envelope, &tv, 0);

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

        int channelid = envelope.channel;
        int deliverytag = envelope.delivery_tag;

//        std::cout << deliverytag << " ";
        /*
        std::cout << "Channel: " << channelid << ", "
            << "DeliveryTag: " << deliverytag << ", "
            << "Exchange: " << std::string((char*)envelope.exchange.bytes, envelope.exchange.len) << ", "
            << "RouteKey: " << std::string((char*)envelope.routing_key.bytes, envelope.routing_key.len) << ", "
            << "Data: " << data 
            << std::endl;
            */

        if (bConsumeAck) {
            amqp_basic_ack(conn, channelid, deliverytag, 0 /* multiple */);
        }
        ++recv_this_sec;
        amqp_destroy_envelope(&envelope);
    }
}

void rabbit_publish(amqp_connection_state_t conn,
                       const std::string& exchange_name,
                       const std::string& queue_name,
                       const std::string& route_key,
                       int channel_id,
                       int rate_limit,
                       int msg_cnt)
{
    char message[1024] = {0};
    amqp_bytes_t message_bytes;

    memset(message, '0', sizeof(message));
    /*
    for (int i = 0; i < (int)sizeof(message); ++i) {
        message[i] = ('0' + i%10)&0xff;
    }
    */

    message_bytes.len = sizeof(message);
    message_bytes.bytes = message;

    int sent = 0;
    int previous_sent = 0;
    int sent_this_sec = 0;

    uint64_t start_time = now_us();
    uint64_t start_sec = start_time;
    uint64_t next_sec = start_time + SUMMARY_EVERY_US;

    std::cout << "rate_limit: " << rate_limit << std::endl;
    for (int i = 0; i < msg_cnt; ++i) {

        uint64_t now = now_us();

        // 限流
        if (sent_this_sec >= rate_limit) {
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

        amqp_basic_publish(conn,                          /* state */
                           CHANNEL_ID,                    /* channel */
                           amqp_cstring_bytes(exchange_name.c_str()), /* exchange */
                           amqp_cstring_bytes(route_key.c_str()),   /* routekey */
                           0,                /* mandatory */
                           0,                /* immediate */
                           NULL,             /* properties */
                           message_bytes     /* body */
                           );
        check_amqp_reply(conn, "amqp basic publish");
        ++sent;
        ++sent_this_sec;

    }

    {
        uint64_t stop_time = now_us();
        uint64_t total_delta = (int)(stop_time - start_time);

        printf("PRODUCER - Message count: %d\n", msg_cnt);
        printf("Total time, milliseconds: %lu\n", total_delta/1000);
        printf("Overall messages-per-second: %g\n", (msg_cnt/(total_delta / 1000000.0)));
    }
}
