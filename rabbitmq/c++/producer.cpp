#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <sys/time.h>
#include <ctime>

#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>

#include "common.h"

const int SUMMARY_EVERY_US = 1000000;

const int SEND_MSG_CNT = 10000;

const int RATE_LIMIT = 10000;

uint64_t now_ms(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);

    return (uint64_t) tv.tv_sec * 1000000 + (uint64_t)tv.tv_usec;
}

void microsleep(int usec)
{
    struct timespec req;
    req.tv_sec = 0;
    req.tv_nsec = 1000 * usec;
    nanosleep(&req, NULL);
}

static void send_batch(amqp_connection_state_t conn,
                       const std::string& queue_name,
                       int rate_limit,
                       int msg_cnt)
{
    uint64_t start_time = now_ms();
    uint64_t previous_report_time = start_time;
    uint64_t next_summary_time = start_time + SUMMARY_EVERY_US;

    char message[256] = {0};
    amqp_bytes_t message_bytes;

    for (int i = 0; i < (int)sizeof(message); ++i) {
        message[i] = ('0' + i%10)&0xff;
    }

    message_bytes.len = sizeof(message);
    message_bytes.bytes = message;

    int sent = 0;
    int previous_sent = 0;
    for (int i = 0; i < msg_cnt; ++i) {
        uint64_t now = now_ms();

        amqp_basic_publish(conn,                          /* state */
                           CHANNEL_ID,                    /* channel */
                           amqp_cstring_bytes(EXCHANGE_NAME.c_str()), /* exchange */
                           amqp_cstring_bytes(ROUTER_NAME.c_str()),   /* routekey */
                           0,                /* mandatory */
                           0,                /* immediate */
                           NULL,             /* properties */
                           message_bytes     /* body */
                           );
        check_amqp_reply(conn, "amqp basic publish");
        ++sent;

        if (now > next_summary_time) {
            int countOverInterval = sent - previous_sent;
            double intervalRate = countOverInterval / ((now - previous_report_time) / 1000000.0);
            printf("%d ms: sent %d - %d since last report (%d Hz)\n",
                    (int)(now - start_time)/1000, sent, countOverInterval, (int)intervalRate);
            previous_sent = sent;
            previous_report_time = now;
            next_summary_time = SUMMARY_EVERY_US;
        }

        while (((i * 1000000.0) / (now - start_time)) > rate_limit) {
            microsleep(2000);
            now = now_ms();
        }
    }

    {
        uint64_t stop_time = now_ms();
        int total_delta = (int)(stop_time - start_time);

        printf("PRODUCER - Message count: %d\n", msg_cnt);
        printf("Total time, milliseconds: %d\n", total_delta/1000);
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

int main()
{
    amqp_connection_state_t conn;

    rabbit_init(conn);

    send_batch(conn, QUEUE_NAME, RATE_LIMIT, SEND_MSG_CNT);

    rabbit_close(conn);
   
    return 0;
}
