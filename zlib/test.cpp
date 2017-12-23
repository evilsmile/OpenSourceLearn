#include <iostream>
#include <zlib.h>
#include <string>
#include <fstream>

int main()
{
    std::ifstream in("phone.list");
    if (!in) 
    {
        std::cerr << "Error open file.\n";
        return -1;
    }

    std::string data;
    while(in >> data)
    { }

    std::string compressedData;
    compressedData.resize(data.size());
    unsigned long c_size = compressedData.size();
    compress((unsigned char*)compressedData.c_str(), &c_size, (unsigned char*)data.c_str(), data.size());
    std::cout << "Compress before: " << data.size() << ". After compress: " << c_size << std::endl;

    return 0;
}
