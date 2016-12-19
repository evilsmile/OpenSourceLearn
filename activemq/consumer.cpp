#include <iostream>
#include <cstdlib>

#include <cms/Connection.h>
#include <cms/ConnectionFactory.h>
#include <cms/ExceptionListener.h>

#include <boost/scoped_ptr.hpp>

#include <activemq/library/ActiveMQCPP.h>

#include <decaf/lang/exceptions/NullPointerException.h>

using namespace cms;
using namespace activemq;
using namespace decaf::lang;
using namespace decaf::lang::exceptions;

using namespace boost;

class Consumer : public cms::MessageListener,
                 public cms::ExceptionListener {
public:
    Consumer();
    ~Consumer();
    void setTopicName(const std::string& topicName) { this->_topicName = topicName; }
    void run();

public:
    void onMessage(const cms::Message* message);
    virtual void onException(const cms::CMSException& e);
    virtual void onException(const decaf::lang::Exception& ex) ;

private:
    std::string _topicName;
    static std::string _brokerUrl;

    scoped_ptr<cms::Connection> _connection;
    scoped_ptr<cms::Session> _session;
    scoped_ptr<cms::MessageConsumer> _consumer;

};

std::string Consumer::_brokerUrl = "failover:(tcp://192.168.0.40:61616)";

Consumer::Consumer()
{
    cms::ConnectionFactory* connectionFactory = cms::ConnectionFactory::createCMSConnectionFactory(_brokerUrl);
    _connection.reset(connectionFactory->createConnection());
    _session.reset(_connection->createSession());
}

Consumer::~Consumer()
{
    if (_connection) {
        _connection->stop();
    }

}

void Consumer::run()
{
    if (_session == NULL) {
        throw NullPointerException(__FILE__, __LINE__, "session object is null");
    }
   
    scoped_ptr<cms::Topic> msg(_session->createTopic(_topicName));
    _consumer.reset(_session->createConsumer(msg.get()));
    _consumer->setMessageListener(this);

    _connection->start();

}

void Consumer::onMessage(const cms::Message* message) 
{
    std::string cmsType = message->getCMSType();

    std::cout << "CMS type: " << cmsType << std::endl;


}

void Consumer::onException(const CMSException& ex AMQCPP_UNUSED) 
{
    std::cerr << "CMS Exception occurred.  Shutting down client." << std::endl;
    exit(1);
}

void Consumer::onException(const decaf::lang::Exception& ex) 
{
    std::cerr << "Transport Exception occurred: " << ex.getMessage() << std::endl;
}

int main(int argc, char* argv[])
{

    activemq::library::ActiveMQCPP::initializeLibrary();

    Consumer c;
    c.setTopicName("ian_topic");
    c.run();

    while (std::cin.get() != 'q') { }

    activemq::library::ActiveMQCPP::shutdownLibrary();

	return 0;
}

