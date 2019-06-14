/*	Definition section */
%{
extern int yylineno;
extern int yylex();
extern char* yytext;   // Get current token from lex
extern char buf[256];  // Get current code line from lex
#define TABLE_SIZE 30
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef enum{function, variable, parameter}Entrytype;
/*struct Number{
    enum{INTEGER, FLOAT}type;
    union{
        int i_val;
        float f_val;
    };
};*/
typedef struct symbol{
    char name[50];
    Entrytype entrytype;
    char datatype[10];
    char attr[50];    
}symbol;
typedef struct symtable{
    symbol cur[30];
    int lev;
    int item_exist;
    int index;
    struct symtable *parent;
}symtable;
typedef struct par{
    char type[10];
    char id[20];
}par;
symtable *curtable = NULL;  //pointer to current table
int curlevel = 0;  //current table's index and current level
char parlist[50];
par parameters[20];
int cnt = 0;

/* Symbol table function - you can add new function if needed. */
int lookup_symbol(int,char *);
void create_symbol(int);
void insert_symbol(symtable *,char *, Entrytype, char *, char *);
void dump_symbol();
void yyerror(char *);

%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    char* string;
    int boolean;
    int i_val;
    double f_val;
}

/* Token without return */
%token INC DEC MTE LTE EQ NE 
%token ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%token PRINT 
%token IF ELSE WHILE RET
%token SEMICOLON
%token <string>VOID INT FLOAT STRING BOOL

/* Token with return, which need to sepcify type */
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> STR_CONST ID
%token <boolean> TRUE
%token <boolean> FALSE

/* Nonterminal with return, which need to sepcify type */
%type <f_val> binary_expr term boolean_expr postfix factor constant
%type <string>type

/* Token associativity */
%left '*' '/' '%'
%left '+' '-' 
%left LT MT LTE MTE EQ NE
%right '=' ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%nonassoc IFX
%nonassoc ELSE

/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    : stats {dump_symbol();printf("\nTotal lines: %d \n",yylineno);} 
;
stats  
    :stats stat
    |stat
;
stat 
    : expression_stat     
    | selection_stat
    | iteration_stat
    | compound_stat
    | func_decl     
;

expression_stat
    : expression SEMICOLON
    | statement SEMICOLON
    | SEMICOLON
;
expression
    : boolean_expr      //conditional    
    | function_call
    | assignment
;
statement
    : declaration 
    | return_expr
;
function_call
    :ID '(' expr_list ')'{int i=lookup_symbol(curlevel,$1); if(i==0){char tmp[50]="Undeclared function "; strcat(tmp, $1); yyerror(tmp);}}
    |PRINT '(' ID ')' {int i=lookup_symbol(curlevel,$3); if(i==0){char tmp[50]="Undeclared variable "; strcat(tmp, $3); yyerror(tmp);}}
    |PRINT '(' STR_CONST ')'
;
expr_list
    :expr_list ',' expression
    |expression
    |
;
return_expr
    : RET expression    
;
boolean_expr
    :boolean_expr EQ binary_expr { $$ = $1 == $3;}
    |boolean_expr NE binary_expr { $$ = $1 != $3;}
    |boolean_expr LT binary_expr { $$ = $1 < $3;}
    |boolean_expr MT binary_expr { $$ = $1 > $3;}
    |boolean_expr LTE binary_expr { $$ = $1 <= $3;}
    |boolean_expr MTE binary_expr { $$ = $1 >= $3;}
    |binary_expr
    |boolean_const
;
binary_expr
    :binary_expr '+' term { $$ = $1 + $3;}
    |binary_expr '-' term { $$ = $1 - $3;}
    |term
;
term
    :term '*' factor { $$ = $1 * $3;}
    |term '/' factor { $$ = $1 / $3;}
    |term '%' factor //{ $$ = $1 % $3;}
    |factor
;
factor
    :'(' expression ')'
    |'-' factor {$$ = -$2;}
    |ID {int i=lookup_symbol(curlevel,$1); if(i==0){char tmp[50]="Undeclared variable "; strcat(tmp, $1); yyerror(tmp);}}
    |constant
    |postfix    
;
constant
    :I_CONST
    |F_CONST
    |STR_CONST
;
postfix
    :ID INC {int i=lookup_symbol(curlevel,$1); if(i==0){char tmp[50]="Undeclared variable "; strcat(tmp, $1); yyerror(tmp);}}
    |ID DEC {int i=lookup_symbol(curlevel,$1); if(i==0){char tmp[50]="Undeclared variable "; strcat(tmp, $1); yyerror(tmp);}}
;
boolean_const
    :TRUE
    |FALSE
;

arg_list
    :arg_list ',' type ID { strcat(parlist, ", ");
                            strcat(parlist, $3);
                            int i= lookup_symbol(curlevel,$4);
                            if(i != 2){
                                strcpy(parameters[cnt].type,$3);
                                strcpy(parameters[cnt++].id,$4);  //insert_symbol(curtable,$4, parameter, $3, "");
                            }
                            else {
                                char tmp[50]="Redeclared variable "; 
                                strcat(tmp, $4); 
                                yyerror(tmp);
                                }} 
    |type ID { strcat(parlist, $1);
                int i= lookup_symbol(curlevel,$2);
                if(i != 2){
                    strcpy(parameters[cnt].type,$1);
                    strcpy(parameters[cnt++].id,$2);  //insert_symbol(curtable,$4, parameter, $3, "");
                }
                else {
                    char tmp[50]="Redeclared variable "; strcat(tmp, $2); 
                    yyerror(tmp);
                    }}
    |
;

declaration
    : type ID '=' expression { int i= lookup_symbol(curlevel,$2);
                                if(i != 2)
                                    insert_symbol(curtable,$2, variable, $1, "");
                               else {
                                   char tmp[50]="Redeclared variable "; 
                                   strcat(tmp, $2); 
                                   yyerror(tmp);
                                }     
                             }
    | type ID { int i= lookup_symbol(curlevel,$2);
                if(i != 2)
                    insert_symbol(curtable,$2, variable, $1, "");
                else {
                    char tmp[50]="Redeclared variable "; 
                    strcat(tmp, $2); 
                    yyerror(tmp);
                }      
             }
;

func_decl
    : type ID '(' arg_list ')' SEMICOLON { int i= lookup_symbol(curlevel,$2);
                if(i != 2){
                    insert_symbol(curtable,$2, function, $1, parlist);
                    memset(parlist,0,50);
                }
                else {
                    char tmp[50]="Redeclared function "; 
                    strcat(tmp, $2); 
                    yyerror(tmp);
                }  
                cnt = 0;   
               }
    | type ID '(' arg_list ')' '{' {int i= lookup_symbol(curlevel,$2);
                                    if(i != 2)
                                        insert_symbol(curtable,$2, function, $1, parlist); 
                                    memset(parlist,0,50);                                                                          
                                    curlevel++; 
                                    create_symbol(curlevel);
                                    for(int i=0;i<cnt; ++i){
                                        insert_symbol(curtable,parameters[i].id, parameter, parameters[i].type, "");
                                    }
                                    cnt = 0;
                                   }block_item_list '}'{dump_symbol();curlevel--;} 
;

/* actions can be taken when meet the token or rule */
type
    : INT { $$ = $1; }
    | FLOAT { $$ = $1; }
    | BOOL  { $$ = $1; }
    | STRING { $$ = $1; }
    | VOID { $$ = $1; }
;

compound_stat
    :'{' {curlevel++; create_symbol(curlevel);} block_item_list '}' {dump_symbol(); curlevel--;}
;

block_item_list
    :block_item_list stat
    |
;

iteration_stat
    :WHILE '(' boolean_expr ')' compound_stat  
;
selection_stat
    :IF '(' boolean_expr ')' compound_stat %prec IFX
    |IF '(' boolean_expr ')' compound_stat ELSE compound_stat
;
assignment
    :ID assignment_operator expression {int i=lookup_symbol(curlevel,$1); if(i==0){char tmp[50]="Undeclared variable "; strcat(tmp, $1); yyerror(tmp);}}
;

assignment_operator
    :'='
    |ADDASGN 
    |SUBASGN 
    |MULASGN 
    |DIVASGN 
    |MODASGN 
;
%%

/* C code section */
int main(int argc, char** argv)
{
    yylineno = 0;    
    create_symbol(curlevel);
    yyparse();

    return 0;
}

void yyerror(char *s)
{
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno+1, buf);
    printf("| %s", s);
    printf("\n|-----------------------------------------------|\n\n");
}

void create_symbol(int level) {
    symtable *newtable = (symtable *)malloc(sizeof(symtable));
    newtable->parent = curtable;
    newtable->lev = level;
    newtable->item_exist = 0;
    curtable = newtable;
}
void insert_symbol(symtable *now,char *name, Entrytype ent, char *dat, char *par) {
    now->item_exist = 1;   
    strcpy(now->cur[now->index].name, name);
    now->cur[now->index].entrytype = ent;   
    strcpy(now->cur[now->index].datatype, dat);
    strcpy(now->cur[now->index].attr, par);
    now->index += 1;
}
int lookup_symbol(int scope, char *name) {
    for(symtable *now = curtable; now != NULL; now = now->parent){
        for(int i = 0; i<now->index;++i){ 
            if(strcmp(now->cur[i].name, name) == 0) {
                if(now->lev == scope){
                    return 2; //Redeclared
                }
                else{
                    return 1; //OK
                }               
            }
        }
    }
    return 0;   //undefine
}
char *getType(Entrytype e){
    switch(e){
        case function:
            return "function";
        case parameter:
            return "parameter";
        case variable:
            return "variable";

    }
}
void dump_symbol() {
    if(curtable->item_exist == 0){
        symtable *tmp;
        tmp = curtable;
        curtable = curtable->parent;
        free(tmp);
        return;
    }
    printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n",
           "Index", "Name", "Kind", "Type", "Scope", "Attribute");
    
    for(int i=0;i<curtable->index;++i){
        printf("%-10d%-10s%-12s%-10s%-10d%-10s",
        i, curtable->cur[i].name, getType(curtable->cur[i].entrytype), curtable->cur[i].datatype, curtable->lev, curtable->cur[i].attr);
        printf("\n"); 
    }
    symtable *tmp;
    tmp = curtable;
    curtable = curtable->parent;
    free(tmp);
}
