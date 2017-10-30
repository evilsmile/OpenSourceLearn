#include "thread.h"

#define USE_LOCK 

Thread::Thread(const std::string& name)
    : _stop(false),
      _name(name)

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

void Thread::stop()
{
    _stop = true;
}

void* Thread::loop_handle(void *arg)
{
    Thread *p_thread = (Thread*)arg;
    if (!p_thread) {
        std::cerr << "NULL thread arg" << std::endl;
        return NULL;
    }

    while (!p_thread->_stop) {

        for (int i = 0; i < 10 && p_thread->has_request(); ++i) {
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

#ifdef USE_LOCK
    pthread_mutex_lock(&_mutex);
#endif

    _requests.push(data_ptr);

#ifdef USE_LOCK
    pthread_mutex_unlock(&_mutex);
#endif

    return true;
}

data_ptr_t Thread::get_request()
{
#ifdef USE_LOCK
    pthread_mutex_lock(&_mutex);
#endif

    data_ptr_t data_ptr(_requests.front());
    _requests.pop();

#ifdef USE_LOCK
    pthread_mutex_unlock(&_mutex);
#endif

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
