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
static const int channel_id = 1;

static const std::string ex_name = "priority_test_ex";
static const std::string ex_type = "direct";

static const std::string qu_name = "priority_test_qu";

static const std::string rt_key = "priority_test_rt";

//                           0  1  2  3  4  5  6  7  8  9 
static int priorities[10] = {1, 2, 1, 1, 1, 1, 1, 1, 1, 1};
//static int priorities[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
//static int priorities[10] = {1, 3, 2, 3, 8, 1, 2, 2, 7, 2};

std::string names[2] = {"Important", "Not-Important"};
static const int PER = 2;
static const int TOTAL = 200;

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


    /*
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

    amqp_exchange_declare(conn, // state 
            channel_id,    // channel 
            amqp_cstring_bytes(ex_name.c_str()),  // exchange
            amqp_cstring_bytes(ex_type.c_str()),  // type
            0,          // passive
            1,          // durable
            0,          // auto_delete
            0,    // internal
            amqp_empty_table);
    check_amqp_reply("amqp declare exchange failed.", "amqp declare exchange succ");

    // 设置队列的优先级属性
    amqp_table_t table;
    table.num_entries = 1;
    table.entries = (amqp_table_entry_t*)calloc(1, sizeof(amqp_table_entry_t));
    table.entries[0].key = amqp_cstring_bytes("x-max-priority");
    table.entries[0].value.kind = AMQP_FIELD_KIND_I16;
    table.entries[0].value.value.i16 = 28;

    amqp_queue_declare(conn,              // state 
            channel_id,        // channel
            amqp_cstring_bytes(qu_name.c_str()),  // queue 
            0,       // passive 
            1,       // durable 
            0,       // exclusive
            0,       // auto_delete 
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
            0, // prefetch_size 
            PER, // prefetch_count 
            0 // global 
            );

    amqp_basic_consume(conn,        // connection 
            channel_id,  // channel 
            amqp_cstring_bytes(qu_name.c_str()),  // queue 
            amqp_cstring_bytes(int2str(channel_id).c_str()),     // consumer_tag 
            0,       // no_local 
            0,       // no_ack 
            1,       // exclusive
            amqp_empty_table    // argument 
            );

                      */
    // 发布
    for (int i = 0; i < TOTAL; )
    {
        for (int j = 0; j < PER; ++j)
        {
            // 发送测试
            int prio = priorities[i%PER];
            printf("publishing [%d] wth priority[%d]..\n", i, prio);
            amqp_basic_properties_t props;
            props._flags = AMQP_BASIC_PRIORITY_FLAG | AMQP_BASIC_DELIVERY_MODE_FLAG;
            props.delivery_mode = 2; /* persistent delivery mode */
            //props.priority = prio;

            std::string msg = "priority test [" + int2str(i) + "] pri:" + int2str(prio);
            amqp_bytes_t message_bytes;
            message_bytes.len = msg.size();
            message_bytes.bytes = (void*)(msg.c_str());

            if (amqp_basic_publish(conn,                              /* state */
                        channel_id,                                   /* channel */
                        amqp_cstring_bytes(ex_name.c_str()),          /* exchange */
                        amqp_cstring_bytes(rt_key.c_str()),           /* routekey */
                        1,                                            /* mandatory */
                        0,                                            /* immediate */
                        &props,                                       /* properties */
                        message_bytes                                 /* body */
                        ) < 0) {
                std::cerr << "basic publish failed." << std::endl;
                break;
            }
            ++i;
        }
        check_amqp_reply("amqp basic publish", "");
        usleep(200);
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
