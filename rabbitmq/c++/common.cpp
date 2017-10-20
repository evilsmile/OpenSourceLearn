#include "common.h"

void check_amqp_reply(amqp_connection_state_t& conn, const std::string& show_tip)
{
    amqp_rpc_reply_t reply = amqp_get_rpc_reply(conn);

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

void microsleep(int usec)
{
    struct timespec req;
    req.tv_sec = 0;
    req.tv_nsec = 1000 * usec;
    nanosleep(&req, NULL);
}
