#include "thread.h"

Thread::Thread(const std::string& name)
    : _name(name)
{
   pthread_mutex_init(&_mutex, NULL);
}

Thread::~Thread()
{
}

bool Thread::start()
{
    if (pthread_create(&_tid, NULL, loop_handle, (void*)this) < 0) {
        std::cerr << "create thread failed.\n";
        return false;
    }

    return true;
}

void* Thread::loop_handle(void *arg)
{
    while (true) {

        Thread *p_thread = (Thread*)arg;
        if (!p_thread) {
            std::cerr << "NULL thread arg" << std::endl;
            continue;
        }

        while (p_thread->has_request()) {
            data_ptr_t req = p_thread->get_request();
            p_thread->handle(req);
        }

        usleep(20);
    }

    return NULL;
}

bool Thread::has_request() const
{
    return (!_requests.empty());
}

bool Thread::add_request(const std::string& raw_req)
{
    data_ptr_t data_ptr(new std::string(raw_req));

    pthread_mutex_lock(&_mutex);

    _requests.push(data_ptr);

    pthread_mutex_unlock(&_mutex);

    return true;
}

data_ptr_t Thread::get_request()
{
    pthread_mutex_lock(&_mutex);

    data_ptr_t data_ptr(_requests.back());

    pthread_mutex_unlock(&_mutex);

    return data_ptr;
}

bool Thread::handle(data_ptr_t req)
{
    return true;
}

/////////////////////////////////////////////////////////////////////////////////////////////
WorkThread::WorkThread(const std::string& name)
    : Thread(name)
{
}

bool WorkThread::handle(data_ptr_t req)
{
    return true;
}
