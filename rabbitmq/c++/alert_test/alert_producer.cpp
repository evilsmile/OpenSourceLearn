#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>

#include "common.h"

void set_msg(char *message, size_t size)
{
    snprintf(message, size, "!!!!!!!!!!!!!!!!!");
}

static void send_batch(amqp_connection_state_t conn,
                       const std::string& router_key,
                       const std::string& msg,
                       int rate_limit,
                       int msg_cnt)
{
    amqp_bytes_t message_bytes;

    message_bytes.len = msg.size();
    message_bytes.bytes = (void*)msg.c_str();

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
                           amqp_cstring_bytes(ALERT_EXCHANGE.c_str()), /* exchange */
                           amqp_cstring_bytes(router_key.c_str()),   /* routekey */
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
    if (argc < 3) {
        std::cout << "Usage: router_key msg" << std::endl;
        return -1;
    }

    std::string router_key = argv[1];
    std::string msg = argv[2];

    amqp_connection_state_t conn;

    rabbit_init(conn);

    send_batch(conn, router_key, msg, rate_limit, 3);

    rabbit_close(conn);
   
    return 0;
}
