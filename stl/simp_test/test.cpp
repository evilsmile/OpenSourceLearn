/* 正如以下两个例子所示，我们可以勾勒出STL的概观了。STL包含各种泛型算法(algorithm)，
 * 如sort、find、lexicographical_compare；泛型指针(iterator)，如ostream_iterator；泛型容器(container)如vector，以及function objects如less和greater。
 * 所谓使用STL，就是去扩充它。下面的例子中，我们各自完成了自己的STL兼容组件：
 * 第一个是完成新品种的iterator，第二个是完成两个新的function objects。
 * STL的算法和容器是独立分离的，任何新增的算法并不要求你改写任何容器。我们可以随心所欲
 * 地将任何算法与任何容器匹配滥用。
 * 而且STL无须继承，便可扩充、可定制。新的iterator(如line_iterator)和新的function objects(如梦strtab_cmp)无须继承自任何特殊基类。它们只需要满足某些抽象条件。
 * 抽象化并不意味着效率低。泛型算法如sort和copy，以及泛型容器vector，其效率如手工精制一般。
 * STL提供了一种新的程序设置思维方式，其中算法与抽象条件组居于中心地位。抽象条件是STL的基础所在。
 *
 */
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <iterator>

using namespace std;

// 一个能够[从输入流一次返回一行]的iterator
class line_iterator {
    istream* in;
    string line;
    bool is_valid;
    void read() {
        if (*in) {
            getline(*in, line);
        }
        // 如果in中failbit或badbit置位，则(*in)为false
        is_valid = (*in) ? true : false;
    }

public:
    typedef input_iterator_tag iterator_category;
    typedef string value_type;
    typedef ptrdiff_t difference_type;
    typedef const string* pointer;
    typedef const string& reference;

    line_iterator() : in(&cin), is_valid(false) {}
    line_iterator(istream& s) : in(&s) { read(); }
    reference operator*() const { return line; }
    pointer operator->() const { return &line; }
    line_iterator operator++() {
        read();
        return *this;
    }
    line_iterator operator++(int) {
        line_iterator tmp = *this;
        read();
        return tmp;
    }

    bool operator==(const line_iterator& i) const {
        return (in == i.in && is_valid == i.is_valid) ||
            (is_valid == false && i.is_valid == false);
    }

    bool operator!=(const line_iterator& i) const {
        return !(*this == i);
    }
};

// 
struct strtab_cmp {
    typedef vector<char>::iterator strtab_iterator;
    bool operator() (const pair<strtab_iterator, strtab_iterator>& x,
            const pair<strtab_iterator, strtab_iterator>& y) const {
        return lexicographical_compare(x.first, x.second, y.first, y.second);
    }
};

struct strtab_print {
    ostream& out;
    strtab_print(ostream& os) : out(os) { }

    typedef vector<char>::iterator strtab_iterator;
    void operator() (const pair<strtab_iterator, strtab_iterator>& s) const {
        copy(s.first, s.second, ostream_iterator<char>(out));
    }
};

int main()
{
#if 0
    line_iterator iter(cin);
    line_iterator end_of_file;
    vector<string> v(iter, end_of_file);
    // 降序
    sort(v.begin(), v.end(), greater<string>());
    copy(v.begin(), v.end(), ostream_iterator<string>(cout, "\n"));

#else
    //在上面的对象排序中，会引发大量的赋值动作，string是个大而复杂的数据结构，
    //将某个string的值赋值给另一个string可能速度会很慢。我们真正想要的应该是对
    //文本行的指针做排序，而非文本行自身。
    //比较有效率的做法是将所有的字符串放到一个字符串表格中，当你需要用到特定
    //某字符串时，则以[指向该字符串表格内]的一个指针来代替。
    //
    vector<char> strtab;  // string table
    char c;
    while (cin.get(c)) {
        strtab.push_back(c);
    }
    typedef vector<char>::iterator strtab_iterator;
    // 一个完整行的首发指针
    vector<pair<strtab_iterator, strtab_iterator> > lines;
    strtab_iterator start = strtab.begin();
    while (start != strtab.end()) {
        strtab_iterator next = find(start, strtab.end(), '\n');
        if (next != strtab.end()) {
            ++next;
        }
        lines.push_back(make_pair(start, next));
        start = next;
    }

    sort(lines.begin(), lines.end(), strtab_cmp());
    for_each(lines.begin(), lines.end(), strtab_print(cout));
#endif

    return 0;
}
