#include "common.h"

#include <unistd.h>

#if 1
// 开启消费ACK
bool bConsumeAck = true;
#else
// 关闭消费ACK
bool bConsumeAck = false;
#endif

void rabbit_init(amqp_connection_state_t& conn)
{
    conn = amqp_new_connection();

    amqp_socket_t* pSocket = amqp_tcp_socket_new(conn);
    if (pSocket == NULL) {
        ABORT("amqp create socket failed.");
    }

    int state = amqp_socket_open(pSocket, HOST_IP.c_str(), HOST_PORT);
    if (state < 0)  {
        ABORT("amqp open socket failed.");
    }

    amqp_rpc_reply_t reply = amqp_login(conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, USER.c_str(), PASSWD.c_str());
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        ABORT("amqp login failed.");
    }

    amqp_channel_open(conn, CHANNEL_ID);
    check_amqp_reply(conn, "amqp open channel failed.");
    
    amqp_exchange_declare(conn, /* state */
                          CHANNEL_ID,    /* channel */
                          amqp_cstring_bytes(EXCHANGE_NAME.c_str()),  /* exchange */
                          amqp_cstring_bytes(EXCHANGE_TYPE.c_str()),  /* type */
                          0,          /* passive */
                          1,          /* durable */
                          0,          /* auto_delete */
                          0,    /* internal */
                          amqp_empty_table);
    check_amqp_reply(conn, "amqp declare exchange failed.");
    std::cout << "Exchange '" << EXCHANGE_NAME << "' declared." << std::endl;

     amqp_queue_declare(conn,              /* state */
                       CHANNEL_ID,        /* channel */
                       amqp_cstring_bytes(QUEUE_NAME.c_str()),  /* queue */
                       0,       /* passive */
                       1,       /* durable */
                       0,       /* exclusive */
                       0,       /* auto_delete */
                       amqp_empty_table);
    check_amqp_reply(conn, "Declaring queue");
    std::cout << "Queue '" << QUEUE_NAME << "' declared." << std::endl;
    amqp_queue_bind(conn,
                    CHANNEL_ID,
                    amqp_cstring_bytes(QUEUE_NAME.c_str()),
                    amqp_cstring_bytes(EXCHANGE_NAME.c_str()),
                    amqp_cstring_bytes(ROUTER_NAME.c_str()),
                    amqp_empty_table);
    check_amqp_reply(conn, "amqp bind queue failed.");
    std::cout << "Queue '" << QUEUE_NAME << "' binded." << std::endl;

    amqp_basic_qos(conn, CHANNEL_ID, 0, 1, 0);

    char szChannelId[32] = {0};
    snprintf(szChannelId, sizeof(szChannelId), "%d", CHANNEL_ID);

    amqp_basic_consume(conn,        /* connection */
                       CHANNEL_ID,  /* channel */
                       amqp_cstring_bytes(QUEUE_NAME.c_str()),  /* queue */
                       amqp_cstring_bytes(szChannelId),     /* consumer_tag */
                       0,       /* no_local */
                       (bConsumeAck? 0 : 1),       /* no_ack */
                       0,       /* exclusive */
                       amqp_empty_table    /* argument */
                       );
    check_amqp_reply(conn, "amqp basic consume failed.");
    std::cout << "Queue '" << QUEUE_NAME << "' basic_consume." << std::endl;
}

void rabbit_consume_loop(amqp_connection_state_t& conn, int rate_limit)
{
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

void rabbit_close(amqp_connection_state_t& conn)
{
    amqp_channel_close(conn, CHANNEL_ID, AMQP_REPLY_SUCCESS);
    amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
    amqp_destroy_connection(conn);
}

int main(int argc, char *argv[])
{
    int rate_limit = RATE_LIMIT;
    if (argc > 1) {
        rate_limit = atoi(argv[1]);
    }
    std::cout << "rate limit: " << rate_limit << ", consume_ack: " << (bConsumeAck?"true":"false") << std::endl;

    amqp_connection_state_t conn;

    rabbit_init(conn);

    rabbit_consume_loop(conn, rate_limit);

    rabbit_close(conn);

    return 0;
}
