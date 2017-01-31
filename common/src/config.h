#pragma once

#include <rapidjson\Document.h>
#include <memory>
#include <string>

namespace home_system
{

class config;

typedef std::unique_ptr<config> config_t;

class config
{
public:
    static config_t create(const std::string& file)
    {
        return config_t(new config(file));
    }

    config(const std::string& file);
    ~config();

    rapidjson::Document& get();
    void write();

private:
    std::string file_;
    rapidjson::Document doc_;
    void prepare_empty_file();
};

}

// for default global configuration object
extern home_system::config_t _config;

#define CONFIG (*::_config)
