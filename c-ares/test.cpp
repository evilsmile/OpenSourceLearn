#include <iostream>

#include <string>
#include <netdb.h>
#include <arpa/inet.h>
#include <ares.h>

#include <boost/thread/thread.hpp>

static ares_channel channel; 


class QueryAcceptor {
    public:

};

// 根据IP地址查询域名
void dns_ipcb(void *arg, int status, int timeout, struct hostent* host)
{
    if (status == ARES_SUCCESS) {
        std::cout << host->h_name << "\n";
    } else {
        std::cout << "IP lookup failed: " << status << ". Reason: " << ares_strerror(status) << "\n";
    }

}

class DnsResolver {
    public:
        void operator()() {
            int nfds, count;
            fd_set readers, writers;
            timeval tv, *tvp;
            while (true) {
                FD_ZERO(&readers);
                FD_ZERO(&writers);
                nfds = ares_fds(channel, &readers, &writers);         // 获取ares channel使用的FD
                if (nfds == 0) {
                    continue;
                }

                tvp = ares_timeout(channel, NULL, &tv);
                count = select(nfds, &readers, &writers, NULL, tvp);
                ares_process(channel, &readers, &writers);
            }

        }
};

// 根据域名查询IP
void dns_namecb(void *arg, int status, int timeout, struct hostent* host)
{
    if (status == ARES_SUCCESS) {
        std::cout << inet_ntoa(*((struct in_addr*)host->h_addr)) << "\n";
    } else {
        std::cout << "Domainname lookup failed: " << status << ". Reason: " << ares_strerror(status) << "\n";
    }
}

int main(int argc, char **argv)
{
    int res = ARES_SUCCESS;

    // 创建一个ares_channel
    // 可能根据环境变量找到DNS服务IP，或者找/etc/resolved.conf
    if ((res = ares_init(&channel)) != ARES_SUCCESS) {
        std::cout << "ares failed: " << res << "\n";
        return -2;
    }

    DnsResolver dnsResolver;
    boost::thread query_thread(dnsResolver);

    while (true) {
        struct in_addr ip;

#if 0
        // 查询给定IP的域名
        std::string query_ip;
        std::cin >> query_ip;
        inet_aton(query_ip.c_str(), &ip);
        ares_gethostbyaddr(channel, &ip, sizeof ip, AF_INET, dns_ipcb, NULL);
#else
        // 查询给定域名的IP
        std::string query_domain;
        std::cin >> query_domain;
        ares_gethostbyname(channel, query_domain.c_str(), AF_INET, dns_namecb, NULL);
#endif
    }

    query_thread.join();

    return 0;
}
