#include <iostream>
#include <cstdlib>

#include "logger.h"
#include "activemq_topic.h"

using namespace activemq;

namespace ActiveMQTopic {

void ActiveMQInit()
{
    activemq::library::ActiveMQCPP::initializeLibrary();
}

void ActiveMQClose()
{
    activemq::library::ActiveMQCPP::shutdownLibrary();
}

// ------------------------------- Subscriber -------------------------------------------

Subscriber::Subscriber(const std::string& brokerUrl, 
                       const std::string& topicName,
                       SubscriberThread* thread)
    : _brokerUrl(brokerUrl), 
      _topicName(topicName),
      _thread(thread)
{

    cms::ConnectionFactory* connectionFactory = cms::ConnectionFactory::createCMSConnectionFactory(_brokerUrl);
    _connection.reset(connectionFactory->createConnection());
    _session.reset(_connection->createSession());
}

Subscriber::~Subscriber()
{
    if (_connection) {
        _connection->stop();
    }
}

void Subscriber::setTopicName(const std::string& topicName) 
{
    this->_topicName = topicName; 
}

void Subscriber::run()
{
    if (_session == NULL) {
        throw NullPointerException(__FILE__, __LINE__, "session object is null");
    }
   
    scoped_ptr<cms::Topic> msg(_session->createTopic(_topicName));
    _consumer.reset(_session->createConsumer(msg.get()));
    _consumer->setMessageListener(this);

    _connection->start();

}

void Subscriber::onMessage(const cms::Message* message) 
{
    cms::TextMessage* textMessage = const_cast<cms::TextMessage*>(dynamic_cast<const cms::TextMessage*>(message));

    SubscribeMessage msg(textMessage);

    _thread->onMessage(msg);
}

void Subscriber::onException(const CMSException& ex AMQCPP_UNUSED) 
{
    std::cerr << "CMS Exception occurred.  Shutting down client." << std::endl;
}

/////////////////////////////////////////////////////////////////////////////////////////////////

SubscriberThread::SubscriberThread(const std::string& brokerUrl, const std::string& topicName)
{ 
    _subscriber.reset(new Subscriber(brokerUrl, topicName, this));
}

void SubscriberThread::begin()
{
    Thread subscribeThread(_subscriber.get());
    subscribeThread.start();
    subscribeThread.join();
}

// ------------------------------- Publiser -------------------------------------------

Publisher::Publisher(const std::string& brokerUrl, const std::string& topicName)
    : _brokerUrl(brokerUrl), _topicName(topicName)
{
}

Publisher::~Publisher()
{
}

int Publisher::Init()
{
    scoped_ptr<cms::ConnectionFactory> connectionFactory;
    connectionFactory.reset(cms::ConnectionFactory::createCMSConnectionFactory(_brokerUrl));

    _connection.reset(connectionFactory->createConnection());

    _session.reset(_connection->createSession());

    if (_session == NULL) {
        log_err("session object is null");
        return -1;
    }

    _topic.reset(_session->createTopic(_topicName));
    _producer.reset(_session->createProducer(_topic.get()));
    _producer->setDeliveryMode(DeliveryMode::NON_PERSISTENT);
}

int Publisher::Send(std::auto_ptr<PublishMessage>& pm)
{
    _producer->send(pm->GetMessage());
}

void Publisher::SetTopicName(const std::string& topicName) 
{ 
    this->_topicName = topicName; 
}

std::auto_ptr<PublishMessage> Publisher::CreatePublishMessage(const std::string& text)
{
    if (_session == NULL) {
        log_err("session is null");
        return std::auto_ptr<PublishMessage>(new PublishMessage(NULL));
    }
    return std::auto_ptr<PublishMessage>(new PublishMessage(_session->createTextMessage(text)));
}

};
