#ifndef __EASYTDATA_H__
#define __EASYTDATA_H__

#include <string>
#include <vector>
#include <map>
#include <boost/shared_ptr.hpp>

class JValue;
typedef boost::shared_ptr<JValue> JValuePtr;
typedef std::map<std::string, JValuePtr> name_value_t;
typedef std::vector<JValuePtr> arrays_t;

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
        ~JValue();

        std::string to_json();

        std::string get_string() const ;
        void set_type(JValueType);
        JValueType get_type() const; 

        // ---- Array Op ---
        int push_to_array(JValuePtr j_value);

        int add_to_object(const std::string& name, JValuePtr j_value);

        void traverse_print();
        void set_ouput_leading_spaces(int n);
        void incr_ouput_leading_spaces();

    private:
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
