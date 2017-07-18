CFLAGS=-g \
	   -I../src/core/ 				\
	   -I../objs/				    \
	   -I ../src/os/unix/ 			

LDFLAGS+= ../objs/src/core/ngx_palloc.o 	\
		  ../objs/src/os/unix/ngx_alloc.o   \
		  ../objs/src/core/ngx_buf.o        \
		  ../objs/src/core/ngx_list.o        \
		  ../objs/src/core/ngx_string.o        \
		  ../objs/src/core/ngx_array.o          \
		  ../objs/src/core/ngx_queue.o

all:test_nginx_api


clean:
	rm -f test_nginx_api
