#include <fstream>
#include <iostream>

#include "configparser.h"
#include "stringutil.h"

bool ConfigParser::init(const std::string& config_file_path)
{
    std::ifstream in;
    in.open(config_file_path.c_str(), std::ios::in);

    if (!in.is_open()) {
        std::cerr << "open file " << config_file_path << " failed.\n";        
        return false;
    }

    std::string line;
    while (getline(in, line)) {
        if (line.empty()) {
            continue;
        }

        trim(line);
        if (line[0] == '#') {
            continue;
        }

        std::size_t pos = line.find("#");
        if (pos != std::string::npos) {
            line = line.substr(0, pos);
        }
        
        pos = line.find("=");
        if (pos == std::string::npos) {
            std::cerr << "invalid config: " << line << std::endl;
            return false;
        }

        std::string key = line.substr(0, pos);
        std::string value = line.substr(pos+1);
        
        std::cout << key << " : " << value << std::endl;

        _key_values.insert(std::make_pair(key, value));
    }

    return true;
}

std::string ConfigParser::getString(const std::string& key, const std::string& default_value)
{
    key_value_t::iterator iter = _key_values.find(key);
    if (iter == _key_values.end()) {
        return default_value;
    }

    return iter->second;
}

int32_t ConfigParser::getInt32(const std::string& key, int32_t default_value)
{
    key_value_t::iterator iter = _key_values.find(key);
    if (iter == _key_values.end()) {
        return default_value;
    }

    return atoi(iter->second.c_str());
}
