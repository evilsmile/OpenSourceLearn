#include "easytdata.h"
#include <stdio.h>

std::string JValue::ToJson()
{
    return ">>>>>"; 
}


JValue::JValue() 
    : _type(NIL), _output_leading_spaces(0),_inner_output_leading_spaces(INNER_LEADING_DIFF_SPACES)
{
    
}

JValue::JValue(int v)
    : _type(NUM), _output_leading_spaces(0),_inner_output_leading_spaces(INNER_LEADING_DIFF_SPACES)
{
    this->_i_v = v;
}

JValue::JValue(const std::string& v)
    : _type(STRING), _output_leading_spaces(0),_inner_output_leading_spaces(INNER_LEADING_DIFF_SPACES)
{
    this->_s_v = v;
}
JValue::JValue(char *v)
    : _type(STRING), _output_leading_spaces(0),_inner_output_leading_spaces(INNER_LEADING_DIFF_SPACES)
{
    this->_s_v = std::string(v);
}
JValue::JValue(bool v)
    : _type( v ? TRUE : FALSE ), _output_leading_spaces(0),_inner_output_leading_spaces(INNER_LEADING_DIFF_SPACES)
{
}

void JValue::set_type(JValueType type)
{
    this->_type = type;
}

JValue::JValueType JValue::get_type() const
{
    return this->_type;
}

std::string JValue::get_string() const 
{
    if (_type != STRING) {
        return "";
    }
    return _s_v;
}

void JValue::set_ouput_leading_spaces(int n)
{
    this->_output_leading_spaces = n;
    this->_inner_output_leading_spaces = n + INNER_LEADING_DIFF_SPACES;
}

void JValue::incr_ouput_leading_spaces()
{
    this->_output_leading_spaces += INNER_LEADING_DIFF_SPACES;
}

void JValue::traverse_print()
{
    if (_type == JValue::NUM) {
        printf("%d", _i_v);
    } else if (_type == JValue::STRING) {
        printf("%s", _s_v.c_str());
    } else if (_type == JValue::TRUE) {
        printf("true");
    } else if (_type == JValue::FALSE) {
        printf("false");
    } else if (_type == JValue::NIL) {
        printf("nil");
    } else if (_type == JValue::OBJECT) {
        printf("%*c \n", _output_leading_spaces, '{');
        name_value_t::iterator iter;
        for (iter = _o_v.begin();
                iter != _o_v.end();
                ++iter) {
            printf("%*s : ", _inner_output_leading_spaces, iter->first.c_str());
            iter->second->traverse_print();
            printf(", \n");
        }
        printf("%*c ,\n", _output_leading_spaces, '}');
    } else if (_type == JValue::ARRAY) {
        printf("%*c\n", _output_leading_spaces, '[');
        arrays_t::iterator iter;
        for (iter = _a_v.begin();
                iter != _a_v.end();
                ++iter) {
            (*iter)->incr_ouput_leading_spaces();
            (*iter)->traverse_print();
        }
        printf("%*c\n", _output_leading_spaces, ']');
    }
}

// ---- Array Op ---
int JValue::push_to_array(JValue* j_value)
{
    _a_v.push_back(j_value);

    return 0;
}

// ---- Object Op ---
int JValue::add_to_object(const std::string& name, JValue* j_value)
{
    _o_v[name] = j_value;
    
    return 0;
}
