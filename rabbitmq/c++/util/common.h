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

const std::string USER = "guest";
const std::string PASSWD = "guest";

const std::string ALERT_USER = "alert_user";
const std::string ALERT_PASSWD = "alertme";

const int SUMMARY_EVERY_US = 1000000;

const int SEND_MSG_CNT = 1000000;

const int RATE_LIMIT = 5000;


////////////////////////////////////////////////////////////////
//alert_producer/consumer
const std::string ALERT_EXCHANGE = "alert_exchange";
const std::string ALERT_EXCHANGE_TYPE = "topic";
const std::string CRITICAL_QUEUE = "critial_queue";
const std::string RATING_QUEUE = "rating_queue";
const std::string CRITICAL_ROUTER = "critical.*";
const std::string RATING_ROUTER = "*.rate_limit";


#define ABORT(msg) \
    std::cerr << "ABORT: " << msg << std::endl; \
    exit(-1);

void check_amqp_reply(amqp_connection_state_t& conn, const std::string& show_tip);

void microsleep(int usec);

uint64_t now_us(void);

#endif
