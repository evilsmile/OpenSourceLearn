#ifndef __RABBITMQ_UTIL_H__
#define __RABBITMQ_UTIL_H__
void rabbit_init(amqp_connection_state_t& conn, 
                 const std::string& username, 
                 const std::string& password, 
                 const std::string& hostip, 
                 int port,
                 int channel_id
                 );

void exchange_declare(amqp_connection_state_t& conn, 
                      const std::string& exchange_name,
                      const std::string& exchange_type,
                      bool durable, 
                      bool auto_delete,
                      int channel_id);

void queue_declare_and_bind_and_consume(amqp_connection_state_t& conn, 
                      const std::string& queue_name,
                      bool durable, 
                      bool exclusive, 
                      bool auto_delete,
                      int channel_id,
                      const std::string& exchange_name,
                      const std::string& route_key);

void rabbit_publish(amqp_connection_state_t conn,
                       const std::string& exchange_name,
                       const std::string& queue_name,
                       const std::string& route_key,
                       int channel_id,
                       int rate_limit,
                       int msg_cnt);

void rabbit_consume_loop(amqp_connection_state_t& conn, int rate_limit);

void rabbit_close(amqp_connection_state_t& conn);

void openConsumeAck();
void closeConsumeAck();

#endif
