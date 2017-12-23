#include "mqthread.h"
#include "stringutil.h"

Role RabbitMQThreadBase::_role = CONSUMER;

static const int DEFAULT_HEARTBEAT_TIMEOUT = 0;

#if 0
static const int DEFAULT_FRAME_MAX = 131072;
#else
static const int DEFAULT_FRAME_MAX = 4096;
#endif

RabbitMQThreadBase::RabbitMQThreadBase()
    : _inited(false)
{
}

RabbitMQThreadBase::RabbitMQThreadBase(const std::string& username, 
                 const std::string& password, 
                 const std::string& hostip, 
                 int port
                 ) : _inited(false)
{
    this->init(username, password, hostip, port);
}

RabbitMQThreadBase::~RabbitMQThreadBase()
{
    if (_inited) {
        close();
    }
}

bool RabbitMQThreadBase::init(const std::string& username, 
                 const std::string& password, 
                 const std::string& hostip, 
                 int port
                 )
{
    _username = username;
    _password = password;
    _hostip = hostip;
    _port = port;

    _set_default_param();

    if (connect() == false) {
        return false;
    }

    _inited = true;

    return true;
}

bool RabbitMQThreadBase::reconnect()
{
    close();

    return connect();
}

bool RabbitMQThreadBase::connect()
{
    _conn = amqp_new_connection();

    amqp_socket_t* pSocket = amqp_tcp_socket_new(_conn);
    if (pSocket == NULL) {
        std::cerr << "amqp create socket failed." << std::endl;
        return false;
    }

    int state = amqp_socket_open(pSocket, _hostip.c_str(), _port);
    if (state < 0)  {
        std::cerr << "amqp open socket failed." << std::endl;
        return false;
    }

    amqp_rpc_reply_t reply = amqp_login(_conn, "/", 0, DEFAULT_FRAME_MAX, DEFAULT_HEARTBEAT_TIMEOUT, AMQP_SASL_METHOD_PLAIN, _username.c_str(), _password.c_str());
    if (reply.reply_type != AMQP_RESPONSE_NORMAL) {
        std::cerr << "amqp login failed." << std::endl;
        return false;
    }

    return true;
}

void RabbitMQThreadBase::set_thread_role(Role role)
{
    _role = role;
}

bool RabbitMQThreadBase::purge_queue(const std::string& queue, int channel_id)
{
    open_channel(channel_id);

    amqp_queue_purge(_conn, channel_id, amqp_cstring_bytes(queue.c_str()));
    _check_amqp_reply("purge_queue");

    while (get_msg_count_from_mq(queue) > 0) {
        sleep(1);
    }

    std::cout << "Queue purged success" << std::endl;

    return true;
}

int RabbitMQThreadBase::get_msg_count_from_mq(const std::string& queue)
{
    amqp_queue_declare_ok_t* pResult = amqp_queue_declare(_conn, 1, amqp_cstring_bytes(queue.c_str()), 1, 0, 0, 1,amqp_empty_table);
    _check_amqp_reply("get msg count by queue_declare");

    int msg_cnt = pResult->message_count;
    std::cout << "msg_cnt: " << msg_cnt << std::endl;

    return msg_cnt;
}

void RabbitMQThreadBase::add_request(ptr_base_req_t req)
{
    _requests.push(req);
}

ptr_base_req_t RabbitMQThreadBase::get_request()
{
    ptr_base_req_t req;

    if (!_requests.empty()) {
        req = _requests.front();
        _requests.pop();
    }

    return req;
}

bool RabbitMQThreadBase::run()
{
    if (pthread_create(&_tid, NULL, thread_start, (void*)this) < 0) {
        std::cerr << "create mqthread failed.\n";
        return false;
    }

    return true;
}

void *RabbitMQThreadBase::thread_start(void *arg)
{
    if (arg == NULL) {
        std::cerr << "arg is NULL" << std::endl;
        return NULL;
    }

    std::cout << "mqthread consume start...." << std::endl;
    RabbitMQThreadBase* ptr_mqthread = (RabbitMQThreadBase*)arg;

    ptr_mqthread->main_loop();

    return NULL;
}

void RabbitMQThreadBase::main_loop()
{
    std::cout << "rate limit: " << _rate_limit << std::endl;

    int mq_handled_this_sec = 0;

    _start_time = now_us();
    uint64_t start_sec = _start_time;
    uint64_t next_sec = start_sec + SUMMARY_EVERY_US;

    while (_running) {

        // 先处理外部线程传递的消息
        ptr_base_req_t req = get_request();
        if (req) {
            req_handler(req);
        }

        uint64_t now = now_us();
        // 限流
        if (mq_handled_this_sec >= _rate_limit && now < next_sec) {
            continue;
        }

        if (now >= next_sec) {
            mq_handled_this_sec = 0;
            start_sec = now;
            next_sec = start_sec + SUMMARY_EVERY_US;
        }

        mq_loop_handler();

        ++mq_handled_this_sec;
    }
}

void RabbitMQThreadBase::close()
{
    for (std::set<int>::iterator iter = _channel_ids.begin();
            iter != _channel_ids.end();
            ++iter) {
        amqp_channel_close(_conn, *iter, AMQP_REPLY_SUCCESS);
    }

    amqp_connection_close(_conn, AMQP_REPLY_SUCCESS);
    amqp_destroy_connection(_conn);
}

bool RabbitMQThreadBase::open_channel(int channel_id)
{
    std::set<int>::iterator iter = _channel_ids.find(channel_id);
    if (iter != _channel_ids.end()) {
        std::cout << "channel id " << channel_id << " opened.\n";
        return true;
    }

    amqp_channel_open(_conn, channel_id);

    _check_amqp_reply("amqp open channel failed.");

    _channel_ids.insert(channel_id);

    return true;
}

bool RabbitMQThreadBase::close_channel(int channel_id)
{
    std::set<int>::iterator iter = _channel_ids.find(channel_id);
    if (iter == _channel_ids.end()) {
        std::cout << "channel id " << channel_id << " not found in open list.\n";
        return true;
    }

    amqp_channel_close(_conn, *iter, AMQP_REPLY_SUCCESS);
    _check_amqp_reply("amqp close channel failed.");

    _channel_ids.erase(iter);

    return true;
}

void RabbitMQThreadBase::set_ratelimit(int rate_limit)
{
    _rate_limit = rate_limit;
}

void RabbitMQThreadBase::join()
{
    pthread_join(_tid, NULL);
}

void RabbitMQThreadBase::stop()
{
    _running = false;
}

/////////////////////////////////////// protected memebers ////////////////////////////////
void RabbitMQThreadBase::_set_default_param(void)
{
    _rate_limit = 5000;
    _running = true;
}

void RabbitMQThreadBase::_check_amqp_reply(const std::string& show_tip)
{
    amqp_rpc_reply_t reply = amqp_get_rpc_reply(_conn);

    switch (reply.reply_type) {
        case AMQP_RESPONSE_NORMAL:
            return;

        case AMQP_RESPONSE_NONE:
            fprintf(stderr, "%s: missing RPC reply type!\n", show_tip.c_str());
            break;

        case AMQP_RESPONSE_LIBRARY_EXCEPTION:
            fprintf(stderr, "%s: %s\n. try reconnect.", show_tip.c_str(), amqp_error_string2(reply.library_error));
            reconnect();
            break;

        case AMQP_RESPONSE_SERVER_EXCEPTION:
            switch (reply.reply.id) {
                case AMQP_CONNECTION_CLOSE_METHOD: 
                    {
                        amqp_connection_close_t *m = (amqp_connection_close_t *) reply.reply.decoded;
                        fprintf(stderr, "%s: server connection error [%u], message: %.*s. try reconnect.\n",
                                show_tip.c_str(),
                                m->reply_code,
                                (int) m->reply_text.len, (char *) m->reply_text.bytes);
                        reconnect();
                        break;
                    }
                case AMQP_CHANNEL_CLOSE_METHOD: 
                    {
                        amqp_channel_close_t *m = (amqp_channel_close_t *) reply.reply.decoded;
                        fprintf(stderr, "%s: server channel error [%u], message: %.*s\n",
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

//////////////////////////////////// Consumer ////////////////////////////
RabbitMQConsumerThread::RabbitMQConsumerThread(const std::string& username, 
        const std::string& password, 
        const std::string& hostip, 
        int port
        )
: RabbitMQThreadBase(username, password, hostip, port)
{
}

void RabbitMQConsumerThread::set_workthread(Thread *ptr_workthread)
{
    this->_ptr_worker_thread = ptr_workthread;
}

bool RabbitMQConsumerThread::set_queue_consume(const std::string& queue_name, bool ack, bool exclusive, int channel_id)
{
    if (open_channel(channel_id) == false) {
        return false;
    }
    return _set_queue_consume(queue_name, ack, exclusive, channel_id);
}

bool RabbitMQConsumerThread::_set_queue_consume(const std::string& queue_name, bool ack, bool exclusive, int channel_id)
{
    std::cout << "queue '" << queue_name << "' -> consume_ack: " << (ack?"true":"false") << std::endl;
    amqp_basic_qos(_conn, 
            channel_id,
            0, /* prefetch_size */
            _prefetch_cnt, /* prefetch_count */
            0 /* global */);

    amqp_basic_consume(_conn,        /* connection */
            channel_id,  /* channel */
            amqp_cstring_bytes(queue_name.c_str()),  /* queue */
            amqp_cstring_bytes(int2str(channel_id).c_str()),     /* consumer_tag */
            0,       /* no_local */
            (ack? 0 : 1),       /* no_ack */
            (exclusive? 1 : 0),       /* exclusive */
            amqp_empty_table    /* argument */
            );
    _check_amqp_reply("amqp basic consume failed.");
    std::cout << "Queue '" << queue_name << "' basic_consume." << std::endl;

    ConsumeInfoItem item(queue_name, ack, exclusive);
    _consume_info_list.insert(std::make_pair(channel_id, item));

    return true;
}

#define CLOSE_CHANNEL_WHEN_PAUSE

#define PAUSE_BY_CHANNEL_FLOW
bool RabbitMQConsumerThread::pause_consume(ptr_base_req_t req)
{
    PauseconsumerReq* pauseReq = dynamic_cast<PauseconsumerReq*>(req.get());
    if (pauseReq == NULL) {
        std::cerr << "request cast from base to pauseconcume failed" << std::endl;
        return false;
    }

    int channel_id = pauseReq->channel_id;

    std::cout << "cancel consumer of channel [" << channel_id << "]\n";

#ifdef PAUSE_BY_CHANNEL_FLOW
    if (inactive_channel(channel_id)) {
        return false;
    }

#else
    if (cancel_consume(channel_id) == false) {
        return false;
    }

#ifdef CLOSE_CHANNEL_WHEN_PAUSE
    std::cout << "close consumer of channel [" << channel_id << "]\n";
    if (close_channel(channel_id) == false) {
        return false;
    }
#endif  /* CLOSE_CHANNEL_WHEN_PAUSE */

#endif /* PAUSE_BY_CHANNEL_FLOW */

    return true;
}

bool RabbitMQConsumerThread::resume_consume(ptr_base_req_t req)
{
    ResumeconsumerReq* resumeReq = dynamic_cast<ResumeconsumerReq*>(req.get());
    if (resumeReq == NULL) {
        std::cerr << "request cast from base to pauseconcume failed" << std::endl;
        return false;
    }

    int channel_id = resumeReq->channel_id;

    std::cout << "resume consumer " << channel_id << "\n";

    consume_info_list_t::iterator itemIter = _consume_info_list.find(channel_id);
    if (itemIter == _consume_info_list.end()) {
        std::cerr << "consume record of channel [" << channel_id << "] not found." << std::endl;
        return false;
    }


#ifdef PAUSE_BY_CHANNEL_FLOW
    if (active_channel(channel_id)) {
        return false;
    }

#else

#ifdef CLOSE_CHANNEL_WHEN_PAUSE
    const ConsumeInfoItem& item = itemIter->second;

    if (set_queue_consume(item.queue_name, 
                item.ack,
                item.exclusive,
                channel_id) == false) {
        return false;
    }
#else
    if (_set_queue_consume(item.queue_name, 
                item.ack,
                item.exclusive,
                channel_id) == false) {
        return false;
    }
#endif  /* CLOSE_CHANNEL_WHEN_PAUSE */

#endif  /* PAUSE_BY_CHANNEL_FLOW */

    return true;
}

std::string RabbitMQConsumerThread::get_one_msg(const std::string& queue_name, bool ack)
{
    std::string data;

    amqp_basic_get(_conn,
            3, 
            amqp_cstring_bytes(queue_name.c_str()),
            ack ? 0 : 1);
    _check_amqp_reply("amqp basic get one msg failed.");

    return data;
}

void RabbitMQConsumerThread::set_prefetchcnt(uint32_t prefetch_count)
{
    _prefetch_cnt = prefetch_count;
}

bool RabbitMQConsumerThread::mq_loop_handler()
{
    amqp_envelope_t envelope;
    memset(&envelope, 0, sizeof(envelope));

    amqp_maybe_release_buffers(_conn);

    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 20 * 1000; // 20ms
    amqp_consume_message(_conn, &envelope, &tv, 0);
    _check_amqp_reply("amqp consume msg failed.");

    std::string data((char*)envelope.message.body.bytes, envelope.message.body.len);

    if (_ptr_worker_thread != NULL) {
        _ptr_worker_thread->add_request(data);
    }

    int channel_id = envelope.channel;
    int delivery_tag = envelope.delivery_tag;
    int redelivered = envelope.redelivered;
    //     std::string exchange((char*)envelope.exchange.bytes, envelope.exchange.len);
    //     std::string router((char*)envelope.routing_key.bytes, envelope.routing_key.len);

//    std::cout << "channelid: " << channel_id << ", delivery_tag: " << delivery_tag << " redelivered:" << redelivered << std::endl;

    consume_info_list_t::iterator itemIter = _consume_info_list.find(channel_id);

    if (itemIter != _consume_info_list.end() && itemIter->second.ack) {

        ConsumeInfoItem& consume_info_item = itemIter->second;
    
        consume_info_item.cur_msgcnt_after_fetched++;
        consume_info_item.cur_delivery_tag = delivery_tag;

        if (consume_info_item.cur_msgcnt_after_fetched >= _prefetch_cnt) {
            //nack_msg(channel_id, consume_info_item.cur_delivery_tag, true /* multiple */, true /*requeue */);
            amqp_basic_ack(_conn, channel_id, consume_info_item.cur_delivery_tag, 0 /* multiple */);
            _check_amqp_reply("amqp basic ack failed.");
            consume_info_item.cur_msgcnt_after_fetched = 0;
            consume_info_item.cur_delivery_tag = 0;
        } 
    } else {
//        std::cout << "not found in consume_info cache of channel: " << channel_id << std::endl;
    }

    amqp_destroy_envelope(&envelope);

    return true;
}

bool RabbitMQConsumerThread::reconnect()
{
    if (RabbitMQThreadBase::reconnect() == false) {
        return false;
    }

    consume_info_list_t::iterator itemIter;
    for (itemIter = _consume_info_list.begin();
            itemIter != _consume_info_list.end();
            ++itemIter) {
        
        int channel_id = itemIter->first;
        const ConsumeInfoItem& item = itemIter->second;

        if (set_queue_consume(item.queue_name, 
                    item.ack,
                    item.exclusive,
                    channel_id) == false) {
            return false;
        }
    }

    return true;
}

bool RabbitMQConsumerThread::inactive_channel(int channel_id)
{
    return _active_or_inactive_channel(channel_id, false); 
}

bool RabbitMQConsumerThread::active_channel(int channel_id)
{
    return _active_or_inactive_channel(channel_id, true); 
}

bool RabbitMQConsumerThread::_active_or_inactive_channel(int channel_id, bool active_flag)
{
    amqp_channel_flow(_conn, channel_id, true);
//    std::cout << "reply status: " << reply->active << std::endl;

    return true;
}

bool RabbitMQConsumerThread::cancel_consume(int channel_id)
{
    amqp_basic_cancel(_conn, channel_id, amqp_cstring_bytes(int2str(channel_id).c_str()));
    _check_amqp_reply("amqp basic cancel failed.");

    return true;
}

void RabbitMQConsumerThread::recover_consume(int channel_id, bool requeue)
{
    amqp_basic_recover(_conn, channel_id, requeue?1:0);
}

bool RabbitMQConsumerThread::reject_msg(int channel_id, uint64_t delivery_tag, bool requeue)
{
    amqp_basic_reject(_conn, channel_id, delivery_tag, requeue?1:0);

    return true;
}

bool RabbitMQConsumerThread::nack_msg(int channel_id, uint64_t delivery_tag, bool multiple, bool requeue)
{
    std::cout << "basic.nack delivery_tag: " << delivery_tag << std::endl;
    amqp_basic_nack(_conn, channel_id, delivery_tag, multiple?1:0, requeue?1:0);

    return true;
}

void RabbitMQConsumerThread::_set_default_param(void)
{
    RabbitMQThreadBase::_set_default_param();

    _ptr_worker_thread = NULL;
    _prefetch_cnt = 1;
    _role = CONSUMER;
}

void RabbitMQConsumerThread::req_handler(ptr_base_req_t req)
{
    if (!req) {
        std::cerr << "NULL request. Ignore." << std::endl;
        return;
    }

    switch(req->cmd) {
        case MQ_CMD_PAUSE_CONSUMER:
            pause_consume(req);
            break;
        case MQ_CMD_RESUME_CONSUMER:
            resume_consume(req);
            break;
        default:
            std::cerr << "Unknown cmd:" << (int)req->cmd << std::endl;
            break;
    };
}

//////////////////////////////// Publisher ///////////////////////////////
RabbitMQPublisherThread::RabbitMQPublisherThread(const std::string& username, 
        const std::string& password, 
        const std::string& hostip, 
        int port
        )
: RabbitMQThreadBase(username, password, hostip, port)
{
}
bool RabbitMQPublisherThread::init_publish_args(st_publish_args_t* p_publish_args)
{
    _p_publish_args = p_publish_args;

    _msg_bytes.len = _p_publish_args->msg.size();
    _msg_bytes.bytes = (void*)(_p_publish_args->msg.c_str());

    if (open_channel(_p_publish_args->channel_id) == false) {
        std::cerr << "open channel " << _p_publish_args->channel_id << " failed.\n";
        return false;
    }

    return true;
}

bool RabbitMQPublisherThread::mq_loop_handler()
{
    if (_sent <  _p_publish_args->msg_cnt) {
        ++_sent;

        amqp_bytes_t b_router_key = amqp_empty_bytes;

        if (!_p_publish_args->route_key.empty()) {
            b_router_key = amqp_cstring_bytes(_p_publish_args->route_key.c_str());
        }

        amqp_basic_properties_t props;
        if (_msg_persistent) {
            props._flags = AMQP_BASIC_CONTENT_TYPE_FLAG | AMQP_BASIC_DELIVERY_MODE_FLAG;
            props.content_type = amqp_cstring_bytes("text/plain");
            props.delivery_mode = 2; /* persistent delivery mode */
        }

        if (_publish_confirm_mode == CMODE_TX) {
            amqp_tx_select(_conn, _p_publish_args->channel_id);
        } 

        amqp_basic_publish(_conn,                          /* state */
                _p_publish_args->channel_id,                    /* channel */
                amqp_cstring_bytes(_p_publish_args->exchange_name.c_str()), /* exchange */
                b_router_key,   /* routekey */
                0,                /* mandatory */
                0,                /* immediate */
                (_msg_persistent ? &props : NULL),  /* properties */
                _msg_bytes     /* body */
                );
        _check_amqp_reply("amqp basic publish");

        if (_publish_confirm_mode == CMODE_TX) {
            amqp_tx_commit(_conn, _p_publish_args->channel_id);
        } 

    }  else {
        uint64_t stop_time = now_us();
        uint64_t total_delta = (int)(stop_time - _start_time);

        printf("PRODUCER - Message count: %d\n", _sent);
        printf("Total time, seconds: %.3f\n", total_delta/1000.0/1000.0);
        printf("Overall messages-per-second: %g\n", (_sent/(total_delta / 1000000.0)));

        // stop running
        _running = false;
    }

    return true;
}

void RabbitMQPublisherThread::enable_msg_persistent(void)
{
    _msg_persistent = true;
}

void RabbitMQPublisherThread::disable_msg_persistent(void)
{
    _msg_persistent = false;
}

bool RabbitMQPublisherThread::set_confirm_mode(PUBLISH_CONFIRM_MODE mode)
{
    if ((mode == CMODE_CONFIRM &&_publish_confirm_mode == CMODE_TX)
            || (mode == CMODE_TX &&_publish_confirm_mode == CMODE_CONFIRM)) {
        std::cerr << "already in "
                  << (_publish_confirm_mode==CMODE_CONFIRM?"confirm":"transaction")
                  << " mode. can't switch to "
                  << (mode==CMODE_CONFIRM?"confirm":"transaction")
                  << " mode\n";
        return false;
    }

    _publish_confirm_mode = mode;

    if (_publish_confirm_mode == CMODE_CONFIRM) {
        std::cout << "init confirm mode\n";
        amqp_confirm_select(_conn, _p_publish_args->channel_id);
    }

    return true;
}

void RabbitMQPublisherThread::_set_default_param(void)
{
    RabbitMQThreadBase::_set_default_param();
    _role = PUBLIHSER;
    _msg_persistent = true;
    _sent = 0;
    _publish_confirm_mode = CMODE_DISABLED;
}
