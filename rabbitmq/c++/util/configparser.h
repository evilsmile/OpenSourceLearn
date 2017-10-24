#ifndef __CONFIG_PARSER_H__
#define __CONFIG_PARSER_H__

#include "common.h"

class ConfigParser
{
    public:
       bool init(const std::string& config_file_path);  
       std::string getString(const std::string& key, const std::string& default_value);
       int32_t getInt32(const std::string& key, int32_t default_value);

    private:
      key_value_t _key_values;
};

#endif
