#include <iostream>
#include <string>

#include <stdio.h>

#include "j.y.hpp"
#include "easytdata.h"

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern YY_BUFFER_STATE yy_scan_string(const char*);
extern void yy_switch_to_buffer  (YY_BUFFER_STATE  new_buffer );
extern FILE* yyin;

extern JValue* g_oJsonData;

int main(int argc, char *argv[])
{
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
    yyparse();

    g_oJsonData->traverse_print();

    return 0;
}
