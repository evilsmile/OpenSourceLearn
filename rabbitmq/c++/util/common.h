#ifndef __COMMON_H__
#define __COMMON_H__

#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <inttypes.h>

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

#define ABORT(msg) \
    std::cerr << "ABORT: " << msg << std::endl; \
    exit(-1);

void microsleep(int usec);

uint64_t now_us(void);

#endif
