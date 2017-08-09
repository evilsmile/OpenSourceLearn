#ifndef __EASYTDATA_H__
#define __EASYTDATA_H__

#include <string>
#include <vector>
#include <map>

class JValue;
#include "j.y.hpp"

typedef std::map<std::string, JValue*> name_value_t;
typedef std::vector<JValue*> arrays_t;

#define INNER_LEADING_DIFF_SPACES 8

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
        };
    public:
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
        int push_to_array(JValue* j_value);

        int add_to_object(const std::string& name, JValue* j_value);

        void traverse_print();
        void set_ouput_leading_spaces(int n);
        void incr_ouput_leading_spaces();
    private:
        JValueType _type;       
        int _output_leading_spaces;
        int _inner_output_leading_spaces;

        std::string _s_v;
        int _i_v;
        name_value_t _o_v;
        arrays_t _a_v;
};

#endif
