%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lex.yy.c"

typedef struct node
{
 char *token;
 struct node *left;
 struct node *right;
} node;
node *mknode(char *token, node *left, node *right);
/*void printTree(node *tree, int tab);
void printTabs(int a);
void freeTree(node *tree);*/
%}


%union
{
	char *string;
	struct node *node;
}

%token <string> VALTYPE STR IF ELSE WHILE DO FOR VAR RETURN VOID NULL 
%token <string> PLUS MINUS DIV MUL
%token <string> OR AND EQ L GR LE GRE NOT NOTEQ
%token <string> ADDR ASS 
%token <string> BOOLVAL CHARVAL DECVAL HEXVAL REALVAL STRVAL ID

%type <node> code program
%type <node> functions 
%type <node> procedure function main params param

%nonassoc IF
%nonassoc ELSE


%left ASS
%left AND
%left OR
%left EQ NOTEQ
%left G GE L LE
%left PLUS MINUS
%left MULT DIV
%right ADDR


%%
/*---------------------------------------Start program--------------------------------------------------------------*/
program: code							                    {printTree($1,0); freeTree($1);};
code: functions main                         {$$=mknode("CODE\n",$1,$2);};
/*----------------------------------------Functions---------------------------------------------------*/
functions: functions function                               
| functions procedure
| epsilon;
function:  VALTYPE ID '(' params ')' '{' body '}'     {$$=mknode("FUNCTION\n",$1,$2);};
procedure: VOID ID '(' params ')' '{' body '}'     ;
main: VOID 'main' '(' ')' '{' body '}';
body: body function 
| body var_decs
| statments
| epsilon;
params: params ';' param  
| param;
param: VALTYPE IDs                                  
| epsilon                                                   ; 

/*---------------------------------------Variable Declarations-----------------------------------------------------------*/	
                                                     
IDs: IDs ',' ID                                             {$$=mknode($1,NULL,NULL);}
| ID                                                        {$$=mknode($1,NULL,NULL);};
var_decs: primitive_decs
| array_decs
| pointer_decs;
primitive_decs: VAR VALTYPE ID ASS expression "," primitive_multiple_dec ';'
| VAR VALTYPE ID "," primitive_multiple_dec ';'
| primitive_dec;
primitive_multiple_decs: ID "," primitive_multiple_dec
| ID ASS expression "," primitive_multiple_dec
| ID
| ID ASS expression;
primitive_dec: VAR VALTYPE ID ASS expression ';'
|VAR VALTYPE ID ';';
array_decs: array_decs "," array_multiple_dec ';'
| array_dec;
array_multiple_dec: ID '[' expression ']' ASS STRVAL ',' array_multiple_dec
| ID '[' expression ']' ',' array_multiple_dec
| ID '[' expression ']'
| ID '[' expression ']' ASS STRVAL ;
array_dec: STR ID '[' expression ']' ASS STRVAL ';' // |N| <-TODO
| STR ID '[' expression ']' ';' ;

 
/*-------------------------------------------Statments--------------------------------------------------------------------*/
statments: statments statment
| statment;
statment: assignment 
| call
| conditions
| loops
| block
| return;
/*---------------------------------------Assignment----------------------------------------------------------------------*/
assignment: primitive_assignment
| index_assigment
| pointer_assigment ;
primitive_assignment: ID ASS expression ';' ;
index_assigment: ID '[' expression ']' ASS STRVAL ';' ;
pointer_assigment: MULT ID ASS expression ';';
/*----------------------------------------Code Block--------------------------------------------------------------------*/
block: '{' body '}';
/*-----------------------------------------procedure/function calls-----------------------------------------------------*/
call: ID ASS ID '(' func_expressions ')' ';'
| ID '(' func_expressions ')' ';'
| ID ASS ID '(' ')' ';'
| ID '(' ')' ';';
func_expressions: expression ',' func_expressions
| expression;
/*----------------------------------------Conditions--------------------------------------------------------------------*/
if_condition: IF '(' expression ')' '{' statments '}'
|IF '(' expression ')' statment ;
if_else_condition: IF '(' expression ')' '{' statments '}' else_condition |
IF '(' expression ')' statment else_condition;
else_condition: ELSE '{' statments '}'
| ELSE statment;
/*-----------------------------------------Loops------------------------------------------------------------------------*/
loops: do_while
| for
| while ;
while: WHILE '(' expression ')' '{' statments '}'
| WHILE '(' expression ')' statment ;
do_while: DO '{' statments '}' WHILE '(' expression ')' ';' ;
for: FOR '(' init ';' expression ';' update ')' '{' statments '}'
| FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' statment ;

/*-----------------------------------------Return-----------------------------------------------------------------------*/
return: RETURN expression ';';
/*-----------------------------------------Expression--------------------------------------------------------------------*/
expression: expression PLUS expression              				
| expression MINUS expression               												
| expression MULT expression	
| expression DIV expression
| expression OR expression								
| expression AND expression															
| expression EQ expression							
| expression NOTEQ expression
| expression L expression								
| expression LE expression							
| expression GR expression									
| expression GRE expression																						
| primitive_val
| ID
| ID '[' expression ']'	
| ADDR ID											
| ADDR ID '[' expression ']'																															
| call									
| '|' ID '|'											
| '(' expression ')'
| unary_expression;
unary_expression: PLUS expression
| MINUS expression
| MULT expression
| ADDR expression
| NOT expression;



/*--------------------------------------------Literal Values-------------------------------------------------------------------*/
literal_val:
	BOOLVAL										
	|CHARVAL									
	|DECVAL								
	|HEXVAL									
	|REALVAL
    |STRVAL									
	|NULL										
	;

epsilon: ;
%%


void main(){
    yyparse();
}
int yyerror(char *err){
    printf("Error: %s at line %d\n",err, yylineno);
	printf("does not accept '%s'\n",yytext);
	return 0;
}
int yywrap(){
	return 1;
}
node *mknode(char *token,node *left,node *right)
{
 node *newnode = (node*)malloc(sizeof(node));
 char *newstr = (char*)malloc(sizeof(token) + 1);
 strcpy(newstr,token);
 newnode->left = left;
 newnode->right = right;
 newnode->token = newstr;
 return newnode;
}