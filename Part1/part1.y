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
void printTree(node *tree);
int yyerror();
int yywrap();
/*void printTabs(int a);*/
/*void freeTree(node *tree);*/
%}


%union
{
	char *string;
	struct node *node;
}

%token <string> VALTYPE STR IF ELSE WHILE DO FOR VAR RETURN VOID PNULL 
%token <string> PLUS MINUS DIV MUL
%token <string> OR AND EQ L GR LE GRE NOT NOTEQ
%token <string> ADDRS ASS 
%token <string> BOOLVAL CHARVAL DECVAL HEXVAL REALVAL STRVAL ID

%type <node> code program
%type <node> functions procedure function params param body
%type <node> IDs var_decs primitive_decs primitive_dec array_decs primitive_multiple_decs array_multiple_dec array_dec
%type <node> assignment call conditions block
%type <node> primitive_assignment index_assigment pointer_assigment func_expressions
%type <node> loops while do_while for
%type <node> return expression literal_val
%type <node> condition_statment condition_statments body_condition_statement
%type <string> unary_operator

%nonassoc IF
%nonassoc ELSE
%nonassoc '{'
%right '}'
%right UNARY
%right ADDRS
%left ASS
%left AND
%left OR
%left EQ NOTEQ
%left GR GRE L LE
%left PLUS MINUS
%left MULT DIV



%%
/*---------------------------------------Start program--------------------------------------------------------------*/
program: code							                    {printTree($1);};
code: functions                          					{$$=mknode("CODE",$1,NULL);};
/*----------------------------------------Functions---------------------------------------------------*/
functions: function functions    					        {$$=mknode("",$1,$2);}                     
| procedure	functions									    {$$=mknode("",$1,$2);}  	
| function													{$$=$1;} 
| procedure													{$$=$1;};																										
function:  VALTYPE ID '(' params ')' block       	        {$$=mknode("FUNCTION",mknode($2,mknode("ARGS",$4,mknode($1,NULL,mknode("BODY",$6,NULL))),NULL),NULL);};
procedure: VOID ID '(' params ')' block          	        {$$=mknode("PROCEDURE",mknode($2,mknode("ARGS",$4,mknode("VOID",NULL,mknode("BODY",$6,NULL))),NULL),NULL);};
body: function body 										{$$=mknode("",$1,$2);} 
| var_decs body 											{$$=mknode("",$1,$2);}
| block											            {$$=$1;} 
| body_condition_statement '}'                              {$$=$1;};
| epsilon													{$$=NULL;};
params: param ';' params  									{$$=mknode("",$1,$3);}
| param														{$$=$1;};
param: VALTYPE IDs											{$$=mknode($1,$2,NULL);}
| epsilon                                                   {$$=NULL;}; 

/*---------------------------------------Variable Declarations-----------------------------------------------------------*/	
                                                     
IDs: ID ','  IDs 										   {$$=mknode($1,$3,NULL);}                                          
| ID                                                       {$$=mknode($1,NULL,NULL);};
var_decs: primitive_decs								   {$$=$1;}
| array_decs                                               {$$=$1;};
/* --------------------pointer_decs TODO---------------------*/
primitive_decs: VAR VALTYPE ID ASS expression "," primitive_multiple_decs ';' {$$=mknode($2,mknode("=",mknode($3,NULL,NULL),$5),$7);}
| VAR VALTYPE ID "," primitive_multiple_decs ';'                              {$$=mknode($2,mknode($3,NULL,NULL),$5);}        
| primitive_dec                                                               {$$=$1;};
primitive_multiple_decs: ID "," primitive_multiple_decs						  {$$=mknode($1,NULL,$3);} 
| ID ASS expression "," primitive_multiple_decs                               {$$=mknode("",mknode("=",mknode($1,NULL,NULL),$3),$5);}
| ID                                                                          {$$=mknode($1,NULL,NULL);}
| ID ASS expression														      {$$=mknode("=",mknode($1,NULL,NULL),$3);};
primitive_dec: VAR VALTYPE ID ASS expression ';'                              {$$=mknode($2,mknode("=",mknode($3,NULL,NULL),$5),NULL);}
|VAR VALTYPE ID ';'															  {$$=mknode($2,mknode($3,NULL,NULL),NULL);}  ;
array_decs: array_decs "," array_multiple_dec ';'                             {$$=mknode("",$1,$3);}
| array_dec                                                                   {$$=$1;};																
array_multiple_dec: ID '[' expression ']' ASS STRVAL ',' array_multiple_dec   {$$=NULL;}  
| ID '[' expression ']' ',' array_multiple_dec {$$=NULL;}
| ID '[' expression ']'    {$$=NULL;}
| ID '[' expression ']' ASS STRVAL {$$=NULL;};
array_dec: STR ID '[' expression ']' ASS STRVAL ';' {$$=NULL;}
| STR ID '[' expression ']' ';' {$$=NULL;};

 
/*-------------------------------------------Statments--------------------------------------------------------------------*/
condition_statments: condition_statment condition_statments{$$=NULL;}
| condition_statment {$$=NULL;} ;
condition_statment: assignment {$$=NULL;}
| call {$$=NULL;} 
| conditions {$$=NULL;}
| loops {$$=NULL;}
| return {$$=NULL;}
| '{' condition_statments '}' {$$=NULL;};
body_condition_statement: '{' condition_statments {$$=NULL;};
/*---------------------------------------Assignment----------------------------------------------------------------------*/
assignment: primitive_assignment {$$=NULL;}
| index_assigment {$$=NULL;}
| pointer_assigment {$$=NULL;};
primitive_assignment: ID ASS expression ';' {$$=NULL;};
index_assigment: ID '[' expression ']' ASS STRVAL ';' {$$=NULL;};
pointer_assigment: MULT ID ASS expression ';' {$$=NULL;};
/*----------------------------------------Code Block--------------------------------------------------------------------*/
block: '{' body '}' {$$=NULL;};
/*-----------------------------------------Procedure/Function Calls-----------------------------------------------------*/
call: ID '(' func_expressions ')' ';' {$$=NULL;};
func_expressions: expression ',' func_expressions {$$=NULL;}
| expression {$$=NULL;}
| epsilon     {$$=NULL;};
/*----------------------------------------Conditions--------------------------------------------------------------------*/
conditions: IF '(' expression ')' condition_statment %prec IF {$$=NULL;}
|IF '(' expression ')' condition_statment ELSE condition_statment {$$=NULL;};
/*-----------------------------------------Loops------------------------------------------------------------------------*/
loops: do_while {$$=NULL;}
| for {$$=NULL;}
| while {$$=NULL;};
while: WHILE '(' expression ')' condition_statment {$$=NULL;};
do_while: DO '{' condition_statments '}' WHILE '(' expression ')' ';' {$$=NULL;};
for: FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' condition_statment {$$=NULL;};
/*-----------------------------------------Return-----------------------------------------------------------------------*/
return: RETURN expression ';' {$$=NULL;};
/*-----------------------------------------Expression--------------------------------------------------------------------*/
expression: expression PLUS expression   {$$=NULL;}           				
| expression MINUS expression         {$$=NULL;}      												
| expression MULT expression	{$$=NULL;}
| expression DIV expression    {$$=NULL;}
| expression OR expression		{$$=NULL;}						
| expression AND expression			{$$=NULL;}												
| expression EQ expression			{$$=NULL;}				
| expression NOTEQ expression {$$=NULL;}
| expression L expression		{$$=NULL;}						
| expression LE expression	{$$=NULL;}						
| expression GR expression		{$$=NULL;}							
| expression GRE expression		{$$=NULL;}
| unary_operator expression %prec UNARY		{$$=NULL;}																	
| literal_val                   {$$=NULL;}
| ID			{$$=NULL;}	
| call			{$$=NULL;}		
| '|' ID '|'			{$$=NULL;}								
| '(' expression ')'      {$$=NULL;}																																						
| ID '[' expression ']'	{$$=NULL;};

unary_operator: PLUS {$$=NULL;} 
| MINUS{$$=NULL;}
| MULT{$$=NULL;}
| ADDRS {$$=NULL;}
| NOT {$$=NULL;};



/*--------------------------------------------Literal Values-------------------------------------------------------------------*/
literal_val: BOOLVAL	{$$=NULL;}									
	|CHARVAL	{$$=NULL;}								
	|DECVAL			{$$=NULL;}					
	|HEXVAL				{$$=NULL;}					
	|REALVAL {$$=NULL;}
    |STRVAL		{$$=NULL;}							
	|PNULL	{$$=NULL;}									;

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
void printTree(node *tree){
	printf("%s\n",tree->token);
	if(tree->left)printTree(tree->left);
	if(tree->right)printTree(tree->right);
}