#include <string>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>
#include <unistd.h>

amqp_connection_state_t conn = NULL;

static const std::string hostip = "127.0.0.1";
static const int port = 5672;
static const std::string username = "guest";
static const std::string password = "guest";
static const int channel_id = 2;

static const std::string ex_name = "priority_test_ex";
static const std::string ex_type = "direct";

static const std::string qu_name = "priority_test_qu";

static const std::string rt_key = "priority_test_rt";

//                           0  1  2  3  4  5  6  7  8  9 
//static int priorities[10] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
static int priorities[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
//static int priorities[10] = {1, 3, 2, 3, 8, 1, 2, 2, 7, 2};

static const int PER = 4;

#define ABORT(msg) \
    std::cerr << "ABORT: " << msg << std::endl; \
    exit(-1);

std::string int2str(int i)
{
    char sz[32];
    memset(sz, 0, sizeof(sz));
    snprintf(sz, sizeof(sz), "%d", i);

    return sz; 
}

void check_amqp_reply(const std::string& show_err_tip, const std::string& show_succ_tip)
{
    amqp_rpc_reply_t reply = amqp_get_rpc_reply(conn);

    switch (reply.reply_type) {
        case AMQP_RESPONSE_NORMAL:
            if (!show_succ_tip.empty())
            {
                printf("%s\n", show_succ_tip.c_str());
            }
            return;

        case AMQP_RESPONSE_NONE:
            fprintf(stderr, "%s: missing RPC reply type!\n", show_err_tip.c_str());
            break;

        case AMQP_RESPONSE_LIBRARY_EXCEPTION:
            fprintf(stderr, "%s: %s\n", show_err_tip.c_str(), amqp_error_string2(reply.library_error));
            break;

        case AMQP_RESPONSE_SERVER_EXCEPTION:
            switch (reply.reply.id) {
                case AMQP_CONNECTION_CLOSE_METHOD: 
                    {
                        amqp_connection_close_t *m = (amqp_connection_close_t *) reply.reply.decoded;
                        fprintf(stderr, "%s: server connection error %uh, message: %.*s\n",
                                show_err_tip.c_str(),
                                m->reply_code,
                                (int) m->reply_text.len, (char *) m->reply_text.bytes);
                        break;
                    }
                case AMQP_CHANNEL_CLOSE_METHOD: 
                    {
                        amqp_channel_close_t *m = (amqp_channel_close_t *) reply.reply.decoded;
                        fprintf(stderr, "%s: server channel error %uh, message: %.*s\n",
                                show_err_tip.c_str(),
                                m->reply_code,
                                (int) m->reply_text.len, (char *) m->reply_text.bytes);
                        break;
                    }
                default:
                    fprintf(stderr, "%s: unknown server error, method id 0x%08X\n", show_err_tip.c_str(), reply.reply.id);
                    break;
            }
            break;          
    }
    ABORT(show_err_tip);
}

void test()
{
    conn = amqp_new_connection();

    amqp_socket_t* pSocket = amqp_tcp_socket_new(conn);
    if (pSocket == NULL) {
        ABORT("amqp create socket failed.");
    }

    printf("socket create succ.\n");

    int state = amqp_socket_open(pSocket, hostip.c_str(), port);
    if (state < 0)  {
        ABORT("amqp open socket failed.");
    }
    printf("socket open succ.\n");

    amqp_rpc_reply_t reply = amqp_login(conn, "/", 0, 131072, 0, AMQP_SASL_METHOD_PLAIN, username.c_str(), password.c_str());
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        ABORT("amqp login failed.");
    }
    printf("amqp login succ.\n");

    // 初始化
    amqp_channel_open(conn, channel_id);
    check_amqp_reply("amqp open channel failed.", "amqp channel channel success.");


    // 预清理测试
    amqp_exchange_delete(conn,
                         channel_id,
                         amqp_cstring_bytes(ex_name.c_str()),
                         0);
    check_amqp_reply("amqp delete exchange failed.", "amqp delete exchange succ.");

    amqp_queue_delete(conn,
                      channel_id,
                      amqp_cstring_bytes(qu_name.c_str()),
                      0,
                      0);
    check_amqp_reply("amqp delete queue failed.", "amqp delete queue succ.");

    amqp_exchange_declare(conn, /* state */
                          channel_id,    /* channel */
                          amqp_cstring_bytes(ex_name.c_str()),  /* exchange */
                          amqp_cstring_bytes(ex_type.c_str()),  /* type */
                          0,          /* passive */
                          1,          /* durable */
                          0,          /* auto_delete */
                          0,    /* internal */
                          amqp_empty_table);
    check_amqp_reply("amqp declare exchange failed.", "amqp declare exchange succ");

    // 设置队列的优先级属性
    amqp_table_t table;
    table.num_entries = 1;
    table.entries = (amqp_table_entry_t*)calloc(1, sizeof(amqp_table_entry_t));
    table.entries[0].key = amqp_cstring_bytes("x-max-priority");
    table.entries[0].value.kind = AMQP_FIELD_KIND_I16;
    table.entries[0].value.value.i16 = 28;

    amqp_queue_declare(conn,              /* state */
            channel_id,        /* channel */
            amqp_cstring_bytes(qu_name.c_str()),  /* queue */
            0,       /* passive */
            1,       /* durable */
            0,       /* exclusive */
            0,       /* auto_delete */
            //amqp_empty_table);
            table);
    check_amqp_reply("amqp declare queue failed.", "amqp declare queue succ.");

    amqp_queue_bind(conn,
            channel_id,
            amqp_cstring_bytes(qu_name.c_str()),
            amqp_cstring_bytes(ex_name.c_str()),
            amqp_cstring_bytes(rt_key.c_str()),
            amqp_empty_table);
    check_amqp_reply("amqp bind queue failed.", "amqp bind queue succ");

    // 消费
    amqp_basic_qos(conn, 
            channel_id,
            0, /* prefetch_size */
            1, /* prefetch_count */
            0 /* global */);

    amqp_basic_consume(conn,        /* connection */
            channel_id,  /* channel */
            amqp_cstring_bytes(qu_name.c_str()),  /* queue */
            amqp_cstring_bytes(int2str(channel_id).c_str()),     /* consumer_tag */
            0,       /* no_local */
            0,       /* no_ack */
            1,       /* exclusive */
            amqp_empty_table    /* argument */
            );

    while (true)
    {
        amqp_envelope_t envelope;
        memset(&envelope, 0, sizeof(envelope));

        amqp_maybe_release_buffers(conn);

        amqp_consume_message(conn, &envelope, NULL, 0);
        check_amqp_reply("amqp consume msg failed.", "");

        std::string data((char*)envelope.message.body.bytes, envelope.message.body.len);

        int channel_id = envelope.channel;
        int delivery_tag = envelope.delivery_tag;

        std::cout << "channelid: " << channel_id << ", delivery_tag: " << delivery_tag << ", data[" << data << "]" << std::endl;
        amqp_basic_ack(conn, channel_id, delivery_tag, 0 /* multiple */);
        check_amqp_reply("amqp basic ack failed.", "amqp basic ack succ.");

        amqp_destroy_envelope(&envelope);
//        usleep(10000);
    }

    amqp_channel_close(conn, channel_id, AMQP_REPLY_SUCCESS);
    amqp_connection_close(conn, AMQP_REPLY_SUCCESS);
    amqp_destroy_connection(conn);
    printf("amqp close and destroy connection.\n");
}

int main()
{
    test();

    return 0;
}
