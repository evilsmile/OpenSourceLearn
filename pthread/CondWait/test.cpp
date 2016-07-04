#include <iostream>
#include <pthread.h>
#include <unistd.h>
#include <cstdio>
#include <string.h>
#include <cstdlib>
#include <list>
#include <string>

static pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

static void cleanup_handler(void *arg) 
{
    printf("Clean up handler of second thread.\n");
    (void)pthread_mutex_unlock(&mtx);
}

std::list<int> data;

static void *thread_func(void *arg) 
{
    pthread_cleanup_push(cleanup_handler, NULL);

    while (true) {
        pthread_cond_wait(&cond, &mtx);
        if (data.empty()) {
            continue;
        }
        std::cout << "Got " << data.size() << " elements." << std::endl;
        std::list<int>::iterator iter;
        for (iter = data.begin(); 
             iter != data.end(); 
             ++iter) 
        {
            std::cout << *iter << "\t";
        }
        std::cout << std::endl;
    }
    pthread_mutex_unlock(&mtx);
    pthread_cleanup_pop(0);
    return 0;
}

int main(int argc, char* argv[])
{
    pthread_t tid;
    pthread_create(&tid, NULL, thread_func, NULL);
    for (int i = 0; i < 10; i++) {
        data.push_front(i);
        pthread_mutex_lock(&mtx);
        pthread_cond_signal(&cond);
        pthread_mutex_unlock(&mtx);
        sleep(1);
    }
    std::cout << "thread 1 wanna end the cancel thread 2." << std::endl;
    pthread_cancel(tid);
    pthread_join(tid, NULL);
    std::cout << "All done." << std::endl;

	return 0;
}

