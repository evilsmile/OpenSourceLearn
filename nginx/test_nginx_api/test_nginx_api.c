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

typedef struct {
    ngx_int_t num;
    ngx_str_t str;
    ngx_queue_t q;
} node_t;

ngx_int_t cmp(const ngx_queue_t* l, const ngx_queue_t* r)
{
    node_t* l_n = ngx_queue_data(l, node_t, q);
    node_t* r_n = ngx_queue_data(r, node_t, q);

    return l_n->num > r_n->num;
}

int test_queue()
{
    int i = 0;
    ngx_queue_t test_q;
    ngx_queue_init(&test_q);

    node_t nodes[10];
    for (i = 0; i < 10; ++i) {
        nodes[i].num = rand() % 100;
        ngx_queue_insert_tail(&test_q, &nodes[i].q);
    }

    ngx_queue_t* iter;
    printf("before sort: \n");
    for (i = 0, iter = ngx_queue_head(&test_q);
            iter != ngx_queue_sentinel(&test_q);
            ++i, iter = ngx_queue_next(iter)) {
        node_t *node = ngx_queue_data(iter, node_t, q);
        printf("node[%d]: %d\n", i, node->num);
    }

    ngx_queue_sort(&test_q, cmp);
    printf("after sort: \n");
    for (i = 0, iter = ngx_queue_head(&test_q);
            iter != ngx_queue_sentinel(&test_q);
            ++i, iter = ngx_queue_next(iter)) {
        node_t *node = ngx_queue_data(iter, node_t, q);
        printf("node[%d]: %d\n", i, node->num);
    }

   
    return 0;
}

int test_array()
{
    ngx_pool_t* pool = ngx_create_pool(2048, &ngx_log);
    EXIT_IF_NULL(pool, "create pool failed.");

    ngx_array_t* array = ngx_array_create(pool, 20, sizeof(ngx_int_t));
    EXIT_IF_NULL(array, "create array failed.");

    ngx_int_t i;
    ngx_int_t* elem;
    for (i = 0; i < 20; ++i) {
        elem = ngx_array_push(array);
        *elem = i;
    }
    elem = (ngx_int_t*)array->elts;
    for (i = 0; i < 20; ++i) {
        printf("array[%d] = %d\n", i, elem[i]);
    }

    printf("Add 10 elements to array: \n");
    ngx_int_t n = 10;
    elem = ngx_array_push_n(array, n);
    for (i = 0; i < n; ++i) {
        elem[i] = 20 + i;
    }

    elem = (ngx_int_t*)array->elts;
    for (i = 20; i < 20 + n; ++i) {
        printf("array[%d] =  %d\n", elem[i]);
    }

    ngx_array_destroy(array);

    printf("ngx_array_init: \n");
    array = ngx_pcalloc(pool, sizeof(ngx_array_t));
    ngx_array_init(array, pool, 20, sizeof(ngx_int_t));
    for (i = 0; i < 20; ++i) {
        elem = ngx_array_push(array);
        *elem = rand() % 1000;
    }

    elem = array->elts;
    for (i = 0; i < 20; ++i) {
        printf("array[%d] = %d\n", i, elem[i]);
    }

    ngx_destroy_pool(pool);

    return 0;
}

int main()
{
    int ret = 0;

    DOTEST(test_pool());

    DOTEST(test_queue());

    DOTEST(test_array());

    return 0;
}
