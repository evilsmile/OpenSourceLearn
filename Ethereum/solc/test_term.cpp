#include <termios.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <cstdio>

int main(void)
{
    int fd;
    struct termio t;

    int ret = ioctl(0, TCGETS, &t);
    printf("ret:%d\n", ret);

    return 0;
}
