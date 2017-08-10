#include <iostream>
#include <string>

#include <stdio.h>

#include "j.y.hpp"
#include "easytdata.h"

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern YY_BUFFER_STATE yy_scan_string(const char*);
extern void yy_switch_to_buffer  (YY_BUFFER_STATE  new_buffer );
extern FILE* yyin;

extern JValuePtr g_oJsonData;

#define TEST_MEMLEAK 0
int main(int argc, char *argv[])
{
#if TEST_MEMLEAK == 1
    for (int i = 0; i < 2; ++i) {
#endif
        if (argc == 1) {
            // default test string
            const char *test = "{}";
            yy_switch_to_buffer(yy_scan_string(test));
        } else if (argc > 1) {
            FILE *fin = NULL;
            fin = fopen(argv[1], "r");
            if (!fin) {
                fprintf(stderr, "open file '%s' failed.\n", argv[1]);
                return -1;
            }
            yyin = fin;
        }
        if (yyparse()) {
            return -1;
        }

        if (yyin) {
            fclose(yyin);
        }

//        g_oJsonData->traverse_print();
        printf("Parsed: %s\n", g_oJsonData->to_json().c_str());

        g_oJsonData.reset();
#if TEST_MEMLEAK == 1
    }
#endif

    return 0;
}
