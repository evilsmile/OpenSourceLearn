#ifndef __COMMON_H__
#define __COMMON_H__

#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include <amqp_tcp_socket.h>
#include <amqp.h>
#include <amqp_framing.h>


const std::string HOST_IP = "127.0.0.1";
//const std::string HOST_IP = "119.23.49.157";
const int HOST_PORT = 5672;
const int CHANNEL_ID = 1;

const std::string EXCHANGE_NAME = "flow_ctl_test_exchange";
const std::string EXCHANGE_TYPE = "direct";
const std::string ROUTER_NAME = "flow_ctl_test_router";

const std::string USER = "guest";
const std::string PASSWD = "guest";

const std::string QUEUE_NAME = "flow_ctl_test_queue";

#define ABORT(msg) \
    std::cerr << "ABORT: " << msg << std::endl; \
    exit(-1);

void check_amqp_reply(amqp_connection_state_t& conn, const std::string& show_tip);

void microsleep(int usec);

#endif
