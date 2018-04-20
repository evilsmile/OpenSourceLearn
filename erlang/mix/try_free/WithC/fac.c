#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include <erl_interface.h>
#include <ei.h>

#define BUSIZ 100

int main(int argc, char* argv[])
{
    int fd; 

    int loop = 1;
    int got;

    unsigned char buf[BUSIZ];
    ErlMessage emsg;

    ETERM *fromp, *argp, *resp;
    int res;

    erl_init(NULL, 0);

    if (erl_connect_init(1, "mycookies", 0) == -1)
    {
        erl_err_quit("erl_connect_init");
    }

    if ((fd = erl_connect("bla@STC")) < 0) 
    {
        erl_err_quit("erl_connect");
    }

    while (loop)
    {
        got = erl_receive_msg(fd, buf, BUFSIZ, &emsg);
    }

    if (got == ERL_TICK) 
    {
        /* ignore */
    }
    else if (got == ERL_ERROR) 
    {
        loop = 0;
    }
    else
    {
        if (emsg.type == ERL_REG_SEND)
        {
            /* unpack message fields */
            fromp = erl_element(1, emsg.msg);
            argp = erl_element(2, emsg.msg);

            /* call fac and send result back */
            resp = erl_format("{ok, ~i}", fac(ERL_INT_VALUE(argp)));
            erl_send(fd, fromp, resp);

            erl_free_term(emsg.from);
            erl_free_term(emsg.msg);
            erl_free_term(fromp);
            erl_free_term(resp);
        }
    }
}

int fac(int y)
{
    if (y <= 0)
    {
        return 1;
    }
    else
    {
        return (y*fac(y-1));
    }
}
