#include <zmq.h>
#include <string>
#include <iostream>
#ifndef _WIN32
#include <unistd.h>
#else
#include <windows.h>

#define sleep(n) Sleep(n)

#endif

static const std::string PORT = "8343";
static const std::string LISTEN_URL = "tcp://*:" + PORT;
static const std::string CONN_URL = "tcp://localhost:" + PORT;

void usage()
{
    std::cout << "Usage: s/c # run as server or client " << std::endl
        ;
}

int start_server()
{
    void *context = zmq_ctx_new();
    void *responder = zmq_socket(context, ZMQ_REP);
    int rc = zmq_bind(responder, LISTEN_URL.c_str());

    while (true) {
        char buffer[10];
        zmq_recv(responder, buffer, 10, 0);
        std::cout << "Received Hello" << std::endl;
        sleep(1);
        zmq_send(responder, "World", 5, 0);
    }
}

int start_client()
{
    std::cout << "client connecting to server..." << std::endl;
    void *context = zmq_ctx_new();
    void *request = zmq_socket(context, ZMQ_REQ);
    zmq_connect (request, CONN_URL.c_str());

    int request_nr = 0;
    for (request_nr = 0; request_nr != 10; ++request_nr) {
        char buffer[10];
        std::cout << "Sending hello " << request_nr << "... " << std::endl;
        zmq_send(request, "Hello", 5, 0);
        zmq_recv(request, buffer, 10, 0);
        std::cout << "Recevied World " << request_nr << std::endl;
    }
    zmq_close(request);
    zmq_ctx_destroy(context);
}

int main(int argc, char* argv[])
{
    if (argc != 2) {
        usage();
        return -1;
    }
    std::string cmd = argv[1];
    if (cmd == "s") {
        start_server();
    } else if (cmd == "c") {
        start_client();
    } else {
        usage();
        return -1;
    }

	return 0;
}
