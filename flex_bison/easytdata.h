#ifndef __EASYTDATA_H__
#define __EASYTDATA_H__

#include <string>
#include <vector>
#include <map>

class JValue;
#include "j.y.hpp"

typedef std::map<std::string, JValue*> name_value_t;
typedef std::pair<JValue, JValue> name_value_item_t;
typedef std::vector<const JValue*> arrays_t;

class JValue {
    public:
        enum JValueType {
            NUM = 1,
            STRING = 2,
            TRUE = 3,
            FALSE = 4,
            OBJECT = 5,
            ARRAY = 6,
            NIL = 7
        }; public:
        JValue();
        JValue(int v);
        JValue(const std::string& v);
        JValue(char *);
        JValue(bool v);
        std::string ToJson();

        std::string get_string() const ;
        void set_type(JValueType);
        JValueType get_type() const; 

        // ---- Array Op ---
        int push_to_array(const JValue* j_value);

        int add_to_object(const std::string& name, JValue* j_value);

    private:
        JValueType _type;       

        std::string _s_v;
        int _i_v;
        name_value_t _o_v;
        arrays_t _a_v;
};

#endif
