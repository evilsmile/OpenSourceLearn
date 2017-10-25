#ifndef __THREAD_H__
#define __THREAD_H__

#include <string>
#include <pthread.h>
#include <queue>
#include <boost/shared_ptr.hpp>

typedef boost::shared_ptr<std::string> data_ptr_t;
typedef std::queue<data_ptr_t> data_cache_t;

class Thread {
    public:
        Thread(const std::string& name);
        ~Thread();

        bool start();

        std::string get_name() const;

        bool add_request(const std::string& req);
        data_ptr_t get_request();

        bool has_request() const;

    protected:
        static void* loop_handle(void *arg);
        virtual bool handle(data_ptr_t);

    private:
        std::string _name;
        pthread_mutex_t _mutex;
        data_cache_t _requests;

        pthread_t _tid;
};

class WorkThread : public Thread {
    public:
        WorkThread(const std::string& name);

        virtual bool handle(data_ptr_t);

    private:
};


#endif
