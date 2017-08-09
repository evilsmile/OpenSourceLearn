%{
#include <iostream>
#include <stdio.h>
#define YYSTYPE JValue*
#include "easytdata.h"

#ifdef __cplusplus
extern "C" {

int yywrap()
{
    return 1;
}

}
#endif

%}

int [+-]*[0-9]+
num [+-]*([0-9]|\.)+
string \"(\\.|[^\\"])*\"
ignore_char [ \t\r\n]
identifier [a-zA-Z_][a-zA-Z0-9_]*


%%
{identifier} {
    if (strncmp(yytext, "true", 4) == 0) {
        yylval = new JValue(true);
        return TRUE;
    } else if (strncmp(yytext, "false", 4) == 0) {
        yylval = new JValue(false);
        return FALSE;
    } else if (strncmp(yytext, "null", 4) == 0) {
        yylval = new JValue();
        return NIL;
    } else {
        yylval = new JValue(yytext);
        return IDENTIFIER;
    }
}

{num} {
    int i = atoi(yytext);
    yylval = new JValue(i);
    return NUM;
}

{string} {
    yylval = new JValue(yytext);
    return STRING;
}

"{" { return L_BRACE; }
"}" { return R_BRACE; }
"[" { return L_BRACKET; }
"]" { return R_BRACKET; }
":" { return COLON; }
";" { return SEMICOLON; }
"," { return COMMA; }
{ignore_char}
%%
