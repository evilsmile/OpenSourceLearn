#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

static void* ngx_http_myconfig_create_loc_conf(ngx_conf_t *cf);
static char* ngx_http_myconfig_merge_loc_conf(ngx_conf_t *cf, void *parent, void *child);
static ngx_int_t ngx_http_myconfig_handler(ngx_http_request_t *r);

/*
static ngx_conf_enum_t test_enums[] = {
    {ngx_string("good"), 1},
    {ngx_string("better"), 2},
    {ngx_string("best"), 3},
    {ngx_null_string, 0},
};
*/

typedef struct {
    ngx_str_t arg_str;
    ngx_int_t arg_counter;
    ngx_flag_t arg_flag;
    ngx_uint_t arg_enum_seq;
    ngx_bufs_t arg_bufs;
} ngx_http_myconfig_loc_conf_t;

static char* ngx_http_mystring(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);

static ngx_command_t ngx_http_myconfig_commands[] = {
    {
        ngx_string("mystring"),                                      // 指令名称
        NGX_HTTP_LOC_CONF | NGX_CONF_TAKE1,                          // 配置参数属性
        ngx_http_mystring,                                           // set 函数
        NGX_HTTP_LOC_CONF_OFFSET,                                    // 该指令配置在http的location中
        offsetof(ngx_http_myconfig_loc_conf_t, arg_str),             // 
        NULL
    },
    ngx_null_command
};

static ngx_http_module_t ngx_http_myconfig_module_ctx = {
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ngx_http_myconfig_create_loc_conf,                                // 创建myconig_loc_conf_t结构体并初始化
    ngx_http_myconfig_merge_loc_conf                                  // 合并可能出现的重复项
};

ngx_module_t ngx_http_myconfig_module = {
    NGX_MODULE_V1,
    &ngx_http_myconfig_module_ctx,                                     // 设置为自己的模块上下文
    ngx_http_myconfig_commands,
    NGX_HTTP_MODULE,                                                  // 表明是HTTP模块
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NGX_MODULE_V1_PADDING
};

static char* ngx_http_mystring(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_conf_log_error(NGX_LOG_INFO, cf, 0, "mystring config begin");
    ngx_http_myconfig_loc_conf_t *mlcf;
    mlcf = conf;
    char *rt = ngx_conf_set_str_slot(cf, cmd, conf);                                    // 调用ngx_conf_set_str_slot 处理ngx_str_t类型的变量  
    ngx_conf_log_error(NGX_LOG_INFO, cf, 0, "mystring = [%s]", mlcf->arg_str.data);

    ngx_http_core_loc_conf_t *clcf;
    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_http_myconfig_handler;                                          // 按需求挂载了handler函数

    return rt;
}

static void* ngx_http_myconfig_create_loc_conf(ngx_conf_t *cf)
{
    ngx_http_myconfig_loc_conf_t *mlcf;

    mlcf = ngx_pcalloc(cf->pool, sizeof(ngx_http_myconfig_loc_conf_t));
    if (mlcf == NULL) {
        return NGX_CONF_ERROR;
    }

    // 一定要初始话结构体的成员变量，不然会出现一些错误
    ngx_str_null(&mlcf->arg_str);
    mlcf->arg_counter = NGX_CONF_UNSET;
    mlcf->arg_flag = NGX_CONF_UNSET;
    mlcf->arg_enum_seq = NGX_CONF_UNSET_UINT;

    return mlcf;
}

static char* ngx_http_myconfig_merge_loc_conf(ngx_conf_t *cf, void *parent, void *child)
{
    return NGX_CONF_OK;
}

static ngx_int_t ngx_http_myconfig_handler(ngx_http_request_t *r)
{
    ngx_log_error(NGX_LOG_INFO, r->connection->log, 0, "ngx_http_myconfig_handler is called");

    if (!(r->method & (NGX_HTTP_HEAD | NGX_HTTP_GET))) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, "method is failed!");
        return NGX_HTTP_NOT_ALLOWED;
    }

    ngx_int_t rc = ngx_http_discard_request_body(r);
    if (rc != NGX_OK) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, "discard_request_body failed!");
        return rc;
    }

    ngx_str_t type = ngx_string("text/html");
    r->headers_out.content_type = type;
    r->headers_out.status = NGX_HTTP_OK;
    if (r->method == NGX_HTTP_HEAD) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, "only header!");
        r->headers_out.content_length_n = type.len;
        return ngx_http_send_header(r);
    }

    ngx_log_error(NGX_LOG_INFO, r->connection->log, 0, "handling is begining");
    ngx_http_myconfig_loc_conf_t *mlcf;
    mlcf = ngx_http_get_module_loc_conf(r, ngx_http_myconfig_module);
    if (mlcf == NULL) {
        ngx_log_error(NGX_LOG_ERR, r->connection->log, 0, "mlcf is empty!");
        return NGX_ERROR;
    }

    ngx_str_t mystring = mlcf->arg_str;
    ngx_str_t myflag;
    if (mlcf->arg_flag == 1) {
        ngx_str_set(&myflag, "on");
    } else {
        ngx_str_set(&myflag, "off");
    }

    ngx_str_t grade;
    if (mlcf->arg_enum_seq == 1) {
        ngx_str_set(&grade, "good");
    } else {
        ngx_str_set(&grade, "best");
    }

    ngx_str_t format = ngx_string("mystring=%s, myflag=%s, grade=%s");
    ngx_int_t content_length = format.len + mystring.len + myflag.len + grade.len;
    r->headers_out.content_length_n = content_length;
    u_char* content_buf = (u_char*)ngx_pcalloc(r->pool, content_length);
    if (content_buf == NULL) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }

    ngx_sprintf(content_buf, (char*)format.data, mystring.data, myflag.data, grade.data);
    ngx_buf_t *buf = ngx_pcalloc(r->pool, sizeof(ngx_buf_t));
    if (buf == NULL) {
        return NGX_HTTP_INTERNAL_SERVER_ERROR;
    }
    buf->pos = content_buf;
    buf->last = buf->pos + content_length;
    buf->last_buf = 1;
    buf->memory = 1;

    ngx_chain_t out;
    out.buf = buf;
    out.next = NULL;
    rc = ngx_http_send_header(r);
    if (rc == NGX_ERROR || rc > NGX_OK || r->header_only) {
        return rc;
    }

    return ngx_http_output_filter(r, &out);
}


