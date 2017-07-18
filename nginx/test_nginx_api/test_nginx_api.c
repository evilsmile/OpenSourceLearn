#include <ngx_config.h>
#include <ngx_conf_file.h>
#include <nginx.h>
#include <ngx_core.h>
#include <ngx_string.h>
#include <ngx_palloc.h>

#include <stdio.h>
#include <stdlib.h>

volatile ngx_cycle_t *ngx_cycle;
ngx_log_t ngx_log;

static int ERRRET = -1;

#define EXIT_IF_NULL(ptr, msg)  \
    do {                        \
       if ((ptr) == NULL) {    \
           fprintf(stderr, msg "\n"); \
           return (ERRRET);          \
       }                         \
       ERRRET--;                 \
    } while(0)

#define DOTEST(exp)      \
    do {                 \
        printf("========== TEST " #exp " ===========\n"); \
        typeof (exp) res = exp;  \
        printf("=========" #exp " test result: [%d]. ===========\n", res);  \
    } while(0)


void ngx_log_error_core(ngx_uint_t level, ngx_log_t *log, ngx_err_t err,  const char *fmt, ...)
{
}

////////////////////////////////////////////////

int test_pool()
{
    int i = 0;

    // create pool
    ngx_pool_t *pool = ngx_create_pool(1024, &ngx_log);
    EXIT_IF_NULL(pool, "create pool failed.");
    
    printf("create pool succ. [%p]\n", pool);

    // create array
    ngx_int_t *array = ngx_palloc(pool, 128 * sizeof(ngx_int_t));
    EXIT_IF_NULL(array, "create array failed.");
    printf("create array succ. [%p]\n", array);

    for (i = 10; i < 20; ++i) {
        array[i] = random()%100;
        printf("array[%d] = %d\n", i, array[i]);
    }

    // create str
    ngx_str_t *str = ngx_pcalloc(pool, sizeof(ngx_str_t));
    EXIT_IF_NULL(str, "create str failed.");
    str->data = ngx_palloc(pool, 26 * sizeof(char));
    str->len = 2;
    char *str_sample = "who am i";
    int str_sample_len = strlen(str_sample);
    if (str->len < str_sample_len) {
        str_sample_len = str->len;
    }
    memcpy(str->data, str_sample, str_sample_len);
    printf("str: %s\n", str->data);

    ngx_destroy_pool(pool);

    return 0;
}

int main()
{
    int ret = 0;

    DOTEST(test_pool());

    return 0;
}
