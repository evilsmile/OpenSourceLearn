#include <sys/time.h>
#include <ctime>

#include "common.h"

void microsleep(int usec)
{
    struct timespec req;
    req.tv_sec = 0;
    req.tv_nsec = 1000 * usec;
    nanosleep(&req, NULL);
}

uint64_t now_us(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);

    return (uint64_t) tv.tv_sec * 1000000 + (uint64_t)tv.tv_usec;
}


