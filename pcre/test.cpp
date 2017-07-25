#include <stdio.h>
#include <string.h>
#include <pcre.h>

#include <string>

#define OVECCOUNT 30
#define EBUFLEN 128
#define BUFLEN  1024

int main()
{
    pcre *re;

    // 匹配字符串
    std::string src = "111 <title>Hello</title> 222 <body><p>kkk</p></body>";
    // 模式字符串
    std::string pattern = "<[^>]+>([^<>]+)</[^>]+> (\\d+)";

    printf("String: %s\n", src.c_str());
    printf("pattern: %s\n", pattern.c_str());

    int erroffset;
    const char* error;
    // 返回被编译好的正则表达式的pcre内部表示结构
    re = pcre_compile(pattern.c_str(),   // pattern IN，将要被编译的模式
            0,                           // options IN
            &error,                      // errptr OUT, 输出错误信息
            &erroffset,                  // erroffset OUT，pattern中错误位置的偏移量
            NULL                         // tableptr IN，指定字符表
            );

    if (re == NULL) {
        fprintf(stderr, "pcre_compile failed at offset %d: %s\n",
                erroffset, error);
        return -1;
    }

    int ovector[OVECCOUNT];
    int rc = pcre_exec(re,                  // code IN, 用pcre_compile编译好的正则表达式
            NULL,                           // extra IN, 用来向pcre_exec传一些额外的数据信息的结构指针
            src.c_str(),                    // subject IN, 要被用来匹配的字符串
            (int)src.size(),                // length IN, 长度
            0,                              // startoffset IN, 用来指定subject从什么位置开始被匹配的偏移量
            0,                              // options IN用来指定匹配过程中的一些选项
            ovector,                        // vector OUT, 用来返回匹配位置偏移偏移量的数组
            OVECCOUNT                       // ovecsize IN, 用来返回匹配位置偏移量的数组的最大大小
            );

    // 匹配成功返回非负数
    if (rc < 0) {
        if (rc == PCRE_ERROR_NOMATCH) {
            printf("No match found!\n");
        } else {
            printf("match error %d\n", rc);
            pcre_free(re);
            return -2;
        }
    }

    printf("Find match -->\n\n");

    // 取出捕获的分组，$0是整个正则式, $1是第一个()，依此
    // ovector数组每2个代表捕获分组的始、末位置 
    for (int i = 0; i < rc; ++i) {
        char *substr_start = (char*)src.c_str() + ovector[2*i];
        int substr_len = ovector[2*i+1] - ovector[2*i];
        // %.*s 中的*指定字符串长度
        printf("$%02d: %.*s\n", i, substr_len, substr_start);
    }

    pcre_free(re);
        
    return 0;
}
