#include <iostream>

#include "activemq_topic.h"
#include "logger.h"

using namespace ActiveMQTopic;

class TradeSuccSubscribe : public SubscriberThread {

public:
    TradeSuccSubscribe(const std::string& brokerUrl, const std::string& topic) : SubscriberThread(brokerUrl, topic)
    {
    }

    virtual void onMessage(const SubscribeMessage& pm)
    {
        std::string trade_time = pm.GetStringProperty("trade_time");
        int amount = pm.GetIntProperty("amount");
        std::cout << "trade_time: " << trade_time  << " "
            << "amount: " << amount
            << std::endl;
    }
};

int main(int argc, char* argv[])
{
    ActiveMQInit();

    std::string topic = "ian_topic";
    std::string brokerUrl = "failover:(tcp://192.168.0.40:61616)";

    TradeSuccSubscribe sc(brokerUrl, topic);
    sc.begin();

    Publisher p(brokerUrl, topic);
    p.Init();

    std::auto_ptr<PublishMessage> pm(p.CreatePublishMessage(""));
    if (!pm->IsValid()) {
        log_err("create pm message failed.");
        return -1;
    }
    pm->SetStringProperty("trade_time", "2016-12-21 17:55");
    pm->SetIntProperty("amount", 234);
    p.Send(pm);

    std::cout << "...wait" << std::endl;

    while (std::cin.get() != 'q') { }

    ActiveMQClose();

	return 0;
}

