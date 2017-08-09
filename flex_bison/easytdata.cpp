#include "easytdata.h"

std::string JValue::ToJson()
{
    return ">>>>>"; 
}


JValue::JValue() 
    : _type(NIL)
{
    
}

JValue::JValue(int v)
    : _type(NUM)
{
    this->_i_v = v;
}

JValue::JValue(const std::string& v)
    : _type(STRING)
{
    this->_s_v = v;
}
JValue::JValue(char *v)
    : _type(STRING)
{
    this->_s_v = std::string(v);
}
JValue::JValue(bool v)
    : _type( v ? TRUE : FALSE )
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

// ---- Array Op ---
int JValue::push_to_array(const JValue* j_value)
{
    _a_v.push_back(j_value);
}

// ---- Object Op ---
int JValue::add_to_object(const std::string& name, JValue* j_value)
{
    _o_v[name] = j_value;
}
