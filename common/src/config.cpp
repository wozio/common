#include "config.h"
#include "rapidjson/filereadstream.h"
#include "rapidjson/filewritestream.h"
#include "rapidjson/prettywriter.h"
#include "rapidjson/error/en.h"
#include "logger.h"
#include <cstdio>

using namespace rapidjson;

namespace home_system
{
    config::config(const std::string& file)
    : file_(file)
    {
        FILE* f = fopen(file_.c_str(), "r");
        if (f != nullptr)
        {
            char buf[65536];
            FileReadStream s(f, buf, sizeof(buf));
            doc_.ParseStream(s);
            fclose(f);
            if (doc_.HasParseError())
            {
                LOG(ERROR) << "Error parsing configuration file: " << GetParseError_En(doc_.GetParseError()) <<
                    " " << doc_.GetErrorOffset();
                throw std::runtime_error("Error parsing configuration file");
            }
        }
    }

    config::~config()
    {
    }

    rapidjson::Document& config::get()
    {
        return doc_;
    }

    void config::write()
    {
        FILE* f = fopen(file_.c_str(), "w");
        char buf[65536];
        FileWriteStream s(f, buf, sizeof(buf));
        PrettyWriter<FileWriteStream> writer(s);
        doc_.Accept(writer);
        fclose(f);
    }
}