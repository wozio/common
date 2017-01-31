#include "config.h"
#include "rapidjson/filereadstream.h"
#include "rapidjson/filewritestream.h"
#include "rapidjson/writer.h"
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
            if (!doc_.IsObject())
            {
                prepare_empty_file();
            }
        }
        else
        {
            // file not exists
            prepare_empty_file();
        }
    }

    config::~config()
    {
        write();
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
        Writer<FileWriteStream> writer(s);
        doc_.Accept(writer);
        fclose(f);
    }

    void config::prepare_empty_file()
    {
        doc_.SetObject();
        write();
    }
}