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

void travel_radix_tree(ngx_radix_node_t* root);
int test_radix_tree()
{
    // radix_tree节点实际数据，可以为任意类型
    ngx_int_t data[64];
    ngx_int_t i = 0;
    for (i = 0; i < 64; ++i) {
        data[i] = i;
    }

    ngx_pool_t* pool = ngx_create_pool(2048, &ngx_log);
    EXIT_IF_NULL(pool, "create pool failed.");

    // 全局变量，初始化。radix树操作中会用到
    ngx_pagesize = getpagesize();
    printf("pagesize = %d\n", ngx_pagesize);

    // 创建基数树
    ngx_radix_tree_t* tree = ngx_radix_tree_create(pool, -1);
    EXIT_IF_NULL(tree, "create radix tree failed.");

    // 注意key与mask
    // 不能有重复的key
    int32_t mask = 0xFF000000;
    int key = 0;
    // 插入数据
    for (i = 0; i < 64; ++i) {
        if (ngx_radix32tree_insert(tree, (i&0xFF)<<24, mask, &data[i]) != NGX_OK) {
            fprintf(stderr, "insert radix tree failed.\n");
            return -1;
        }
    }

    travel_radix_tree(tree->root);
    printf("\n");

    ngx_uint_t tkey = (0x30 <<24);
    ngx_int_t* value = ngx_radix32tree_find(tree, tkey);
    if (value != NGX_RADIX_NO_VALUE) {
        printf("find the value: [%d] by key [%#x]\n", *value, tkey);
    }

    if (NGX_OK == ngx_radix32tree_delete(tree, tkey, mask)) {
        printf("delete key[%#x] succ\n", tkey);
    } else {
        fprintf(stderr, "delete key[%#x] failed\n", tkey);
    }

    value = ngx_radix32tree_find(tree, tkey);
    if (value != NGX_RADIX_NO_VALUE) {
        printf("find the value: %d with the key = %#x\n", *value, tkey);
    } else {
        printf("not find value of key[%#x]\n", tkey);
    }

    printf("after delete key:0x30000000: \n");
    travel_radix_tree(tree->root);
    printf("\n");

    return 0;
}

void travel_radix_tree(ngx_radix_node_t* root) 
{
    if (root->left != NULL) {
        travel_radix_tree(root->left);
    }
    if (root->value != NGX_RADIX_NO_VALUE) {
        ngx_int_t* value = root->value;
        printf("%d ", *value);
    }
    if (root->right != NULL) {
        travel_radix_tree(root->right);
    }
}

int main()
{
    int ret = 0;

    DOTEST(test_pool());

    DOTEST(test_queue());

    DOTEST(test_array());
    
    DOTEST(test_radix_tree());

    return 0;
}
