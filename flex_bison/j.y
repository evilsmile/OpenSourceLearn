%{
#include "easytdata.h"
#define YYSTYPE JValuePtr
#define YYDEBUG 1
#include "stdio.h"

extern int yylex();
void yyerror(const char* msg) {
    printf("yyparse error: %s\n", msg);
}

JValuePtr g_oJsonData;

int push_to_array(JValuePtr j_array, JValuePtr j_value)
{
    j_array->push_to_array(j_value);

    return 0;
}

int add_to_object(JValuePtr j_obj, JValuePtr j_name, JValuePtr j_value)
{
    const std::string& name = j_name->get_string();
    j_obj->add_to_object(name, j_value);

    return 0;
}

%}

%token INT NUM STRING IGNORE_CHAR L_BRACE R_BRACE L_BRACKET R_BRACKET COLON SEMICOLON COMMA IDENTIFIER TRUE FALSE NIL

%%
Json : Value { 
     g_oJsonData = $1;  
     }

Object : L_BRACE KeyValues R_BRACE { 
            $$ = $2;
        }
       | L_BRACE R_BRACE { 
        $$.reset(new JValue());
        $$->set_type(JValue::OBJECT);
        }

Array : L_BRACKET Elements R_BRACKET { 
        $$ = $2;
        }
      | L_BRACKET R_BRACKET { 
      $$.reset(new JValue());
      $$->set_type(JValue::ARRAY);
        }

ID : IDENTIFIER { $$ = $1; }
   | STRING { $$ = $1; }

KeyValues : KeyValues COMMA ID COLON Value{
      add_to_object($$, $3, $5);
      $$ = $1;
      }
      | ID COLON Value {
      $$.reset(new JValue());
      $$->set_type(JValue::OBJECT);
      add_to_object($$, $1, $3);
      }

Value : NUM  
      | STRING
      | Object
      | Array 
      | TRUE 
      | FALSE
      | NIL {
      $$ = $1;
      }
      
Elements : Elements COMMA Value {
        $$ = $1;
        push_to_array($1, $3);
        $$->set_type(JValue::ARRAY);
     }
         | Value {
         $$.reset(new JValue());
         $$->set_type(JValue::ARRAY);
         push_to_array($$, $1);
     }

%%
