#ifndef __ACTIVEMQ_TOPIC_H__
#define __ACTIVEMQ_TOPIC_H__

#include <cms/Connection.h>
#include <cms/ConnectionFactory.h>
#include <cms/ExceptionListener.h>
#include <activemq/library/ActiveMQCPP.h>

#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>

#include <decaf/lang/exceptions/NullPointerException.h>
#include <decaf/lang/Thread.h>  
#include <decaf/lang/Runnable.h>

#include <memory>


using namespace cms;
using namespace decaf::lang;
using namespace decaf::lang::exceptions;

using namespace boost;

namespace ActiveMQTopic {

/*
 * 程序初始化时调用
 */
void ActiveMQInit();
/*
 * 程序退出时调用
 */
void ActiveMQClose();

// ------------------------------- Message -------------------------------------------

enum  MSGTYPE{ 
    MSGTYPE_SUBSCRIBE,
    MSGTYPE_PUBLISH 
};

/*
 * 订阅和发布时使用的消息类的基类
 */
template <MSGTYPE Type>
class TopicMessage {
public:

    TopicMessage(cms::TextMessage* msg) : _msgType(Type), _msg(msg) { }

    ~TopicMessage(); 

public:
    /*
     * 以下为设置不同类型参数的功能函数
     */
    void SetIntProperty(const std::string& name, int value);
    int GetIntProperty(const std::string& name) const;

    void SetShortProperty(const std::string& name, short value);
    short GetShortProperty(const std::string& name) const;

    void SetLongProperty(const std::string& name, long long value);
    long long GetLongProperty(const std::string& name) const;

    void SetStringProperty(const std::string& name, const std::string& value);
    std::string GetStringProperty(const std::string& name) const;

    /*
     * 消息是否合法可用
     */
    bool IsValid();

    /*
     * 获取原始消息指针
     */
    cms::TextMessage* GetMessage() const;

private:
    TopicMessage(TopicMessage& op) : _msgType(op._msgType), _msg(op._msg) { }

    TopicMessage& operator=(const TopicMessage& op);

protected:
    MSGTYPE _msgType;
    cms::TextMessage* _msg;
};

template <MSGTYPE Type>
TopicMessage<Type>& TopicMessage<Type>::operator=(const TopicMessage<Type>& op) 
{
    _msgType = op._msgType;
    _msg = op._msg;
    return *this;
}

template <MSGTYPE Type>
TopicMessage<Type>::~TopicMessage() {

    // MSGTYPE_SUBSCRIBE 由触发onMessage的外围activemq系统释放
    if (_msg && _msgType == MSGTYPE_PUBLISH) {
        delete _msg;
    }
}

template <MSGTYPE Type>
void TopicMessage<Type>::SetIntProperty(const std::string& name, int value)
{
    _msg->setIntProperty(name, value);
}

template <MSGTYPE Type>
int TopicMessage<Type>::GetIntProperty(const std::string& name) const
{
    return _msg->getIntProperty(name);
}

template <MSGTYPE Type>
void TopicMessage<Type>::SetLongProperty(const std::string& name, long long value)
{
    _msg->setLongProperty(name, value);
}

template <MSGTYPE Type>
long long TopicMessage<Type>::GetLongProperty(const std::string& name) const
{
    return _msg->getLongProperty(name);
}

template <MSGTYPE Type>
void TopicMessage<Type>::SetStringProperty(const std::string& name, const std::string& value)
{
    _msg->setStringProperty(name, value);
}

template <MSGTYPE Type>
std::string TopicMessage<Type>::GetStringProperty(const std::string& name) const 
{
    return _msg->getStringProperty(name);
}

template <MSGTYPE Type>
void TopicMessage<Type>::SetShortProperty(const std::string& name, short value)
{
    _msg->setShortProperty(name, value);
}

template <MSGTYPE Type>
short TopicMessage<Type>::GetShortProperty(const std::string& name) const
{
    return _msg->getShortProperty(name);
}

template <MSGTYPE Type>
bool TopicMessage<Type>::IsValid()
{
    return (_msg != NULL);
}

template <MSGTYPE Type>
cms::TextMessage* TopicMessage<Type>::GetMessage() const
{
    return _msg;
}

/*
 * 订阅者使用的消息类。与发布者使用的消息类的区别是析构时不需要释放_msg
 */
class SubscribeMessage : public TopicMessage<MSGTYPE_SUBSCRIBE> { 
public:
    SubscribeMessage(cms::TextMessage* msg) : TopicMessage<MSGTYPE_SUBSCRIBE>(msg){}
    virtual ~SubscribeMessage(){};
};

/*
 * 发布者使用的消息类
 */
class PublishMessage : public TopicMessage<MSGTYPE_PUBLISH> { 
public:
    PublishMessage(cms::TextMessage* msg) : TopicMessage<MSGTYPE_PUBLISH>(msg){}
    virtual ~PublishMessage(){};
};

// ------------------------------- Subscriber -------------------------------------------

class SubscriberThread;

/* 
 * 订阅者的封装类。在onMessage()调用时会调用所属的订阅线程中指定的处理函数。 
 *
 */

class Subscriber : public Runnable, 
                 public MessageListener,
                 public ExceptionListener {
public:
    Subscriber(const std::string& brokerUrl, const std::string& topicName, SubscriberThread* thread);
    ~Subscriber();
    void setTopicName(const std::string& topicName);

public:
    virtual void onMessage(const cms::Message* message);
    virtual void run();
    virtual void onException(const CMSException& e);

private:
    std::string _topicName;
    std::string _brokerUrl;

    scoped_ptr<cms::Connection> _connection;
    scoped_ptr<cms::Session> _session;
    scoped_ptr<cms::MessageConsumer> _consumer;

    SubscriberThread* _thread;
};

/* 
 * 用于接收话题订阅的线程. 使用时需继承SubscriberThread并实现onMessage()
 *
 */

class SubscriberThread {
public:
    SubscriberThread(const std::string& brokerUrl, const std::string& topicName);
    void begin();

public:
    virtual void onMessage(const SubscribeMessage& pm){}

private:
    boost::scoped_ptr<Subscriber> _subscriber;
};

// ------------------------------- Publiser -------------------------------------------

/*
 * 发布话题者封装。
 * 使用前需要Init().
 */
class Publisher  {
public:
    Publisher(const std::string& brokerUrl, const std::string& topicName);
    ~Publisher();

    int Init();
    // 发布话题函数，发布PublishMessage中的内容
    int Send(std::auto_ptr<PublishMessage>& pm);

    void SetTopicName(const std::string& topicName);

    std::auto_ptr<PublishMessage> CreatePublishMessage(const std::string& text);

private:
    std::string _topicName;
    std::string _brokerUrl;

    scoped_ptr<cms::Connection> _connection;
    scoped_ptr<cms::Session> _session;
    scoped_ptr<cms::MessageProducer> _producer;
    scoped_ptr<cms::Destination> _topic;
};

}

#endif
