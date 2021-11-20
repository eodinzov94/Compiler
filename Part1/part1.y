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
int counter = 1;
/*void printTabs(int a);*/
/*void freeTree(node *tree);*/
%}


%union
{
	char *string;
	struct node *node;
}

%token <string> VALTYPE STR IF ELSE WHILE DO FOR VAR RETURN VOID PNULL 
%token <string> PLUS MINUS DIV MULT
%token <string> OR AND EQ L GR LE GRE NOT NOTEQ
%token <string> ADDRS ASS 
%token <string> BOOLVAL CHARVAL DECVAL HEXVAL REALVAL STRVAL ID

%type <node> code program
%type <node> functions procedure function parameter_list param body
%type <node> id_list var_decs primitive_decs primitive_dec string_dec string_decs primitive_multiple_decs string_multiple_dec
%type <node> assignment call conditions block
%type <node> primitive_assignment index_assigment pointer_assigment func_expressions
%type <node> loops while do_while for
%type <node> return expression literal_val
%type <node> condition_statment condition_statments
%type <string> unary_operator

%nonassoc IF
%nonassoc ELSE
%nonassoc '{'

%right '}'
%right ADDRS
%left ASS
%left AND
%left OR
%left EQ NOTEQ
%left GR GRE L LE
%left PLUS MINUS
%left MULT DIV
%right UNARY




%%
/*---------------------------------------Start program--------------------------------------------------------------*/
program: code							                    {printf("program:%d\n",counter);counter++;printTree($1);};
code: functions                          					{printf("code:%d\n",counter);counter++;$$=mknode("\nCODE",$1,NULL);};
/*----------------------------------------Functions---------------------------------------------------*/
functions: function functions    					        {printf("functions:%d\n",counter);counter++;$$=mknode("",$1,$2);}                     
| procedure	functions									    {printf("functions:%d\n",counter);counter++;$$=mknode("",$1,$2);}  	
| function													{printf("functions:%d\n",counter);counter++;$$=$1;} 
| procedure													{printf("functions:%d\n",counter);counter++;$$=$1;};																										
function:  VALTYPE ID '(' parameter_list ')' block       	        {printf("function:%d\n",counter);counter++;$$=mknode("FUNCTION",mknode($2,mknode("ARGS",$4,mknode($1,NULL,mknode("BODY",$6,NULL))),NULL),NULL);};
procedure: VOID ID '(' parameter_list ')' block          	        {printf("proc:%d\n",counter);;counter++;$$=mknode("PROCEDURE",mknode($2,mknode("ARGS",$4,mknode("VOID",NULL,mknode("BODY",$6,NULL))),NULL),NULL);};


/* BOOOOOOOOOOOOOOOOOOOOODY */
body: function body 										{printf("func body:%d\n",counter);counter++;$$=mknode("",$1,$2);} 
| procedure body                                            {printf("proc body:%d\n",counter);counter++;$$=mknode("",$1,$2);}
| var_decs body 											{printf("var body:%d\n",counter);counter++;$$=mknode("",$1,$2);}
| block	condition_statments										            {printf("block:%d\n",counter );counter++;$$=$1;} 
| block                                                        {printf("block:%d\n",counter );counter++;$$=$1;} 
| condition_statments %prec '}'									{$$= NULL;}
| epsilon													{printf("epsi_body:%d\n",counter );counter++;$$=NULL;};
parameter_list: param ';' parameter_list  									{printf("param_list:%d\n",counter );counter++;$$=mknode("",$1,$3);}
| param	                                                    {printf("param_from_list:%d\n",counter );counter++;$$=$1;}
| epsilon													{printf("epsilon_from_list:%d\n", counter);counter++;$$=mknode("NO_ARGS",NULL,NULL);};
param: VALTYPE id_list											{printf("param:%d\n", counter);counter++;$$=mknode($1,$2,NULL);};            
id_list: ID ','  id_list 										   {printf("id_list:%d\n",counter );counter++;$$=mknode($1,$3,NULL);}                                          
| ID                                                       {printf("ID_from-id_list:%d\n", counter);counter++;$$=mknode($1,NULL,NULL);};
/*---------------------------------------Variable Declarations-----------------------------------------------------------*/	
                                                     
var_decs: primitive_decs								   {printf("var_decs_prim:%d\n", counter);counter++;$$=$1;}
| string_decs                                               {printf("var_decs_arr:%d\n",counter );counter++;$$=$1;};
/* --------------------pointer_decs TODO---------------------*/
primitive_decs: primitive_dec primitive_multiple_decs                    {printf("primitive_decs-mult_ass:%d\n",counter );counter++;$$=mknode("",$1,$2);}
| primitive_dec ';'                                                              {printf("primitive_decs-dec:%d\n",counter );counter++;$$=$1;};
primitive_multiple_decs: ',' ID  primitive_multiple_decs						  {printf("prim_mult_decs-mult:%d\n", counter);counter++;$$=mknode($2,NULL,$3);} 
| ',' ID ASS expression primitive_multiple_decs                               {printf("prim_mult_decs-mult-ass:%d\n", counter);counter++;$$=mknode("",mknode("=",mknode($2,NULL,NULL),$4),$5);}
| ',' ID  ';'                                                                        {printf("prim_mult_decs-ID:%d\n", counter);counter++;$$=mknode($2,NULL,NULL);}
| ',' ID ASS expression	';'													      {printf("prim_mult_decs-mult-ASS:%d\n",counter );counter++;$$=mknode("=",mknode($2,NULL,NULL),$4);};
primitive_dec: VAR VALTYPE ID ASS expression                              {printf("primitive_dec:%d\n", counter);counter++;$$=mknode($2,mknode("=",mknode($3,NULL,NULL),$5),NULL);}
|VAR VALTYPE ID 															  {printf("primitive_dec:%d\n",counter );counter++;$$=mknode($2,mknode($3,NULL,NULL),NULL);}  ;
string_decs: string_dec string_multiple_dec ';'                             {printf("array_decs:%d\n",counter );counter++;$$=mknode("",$1,$2);}
| string_dec ';'                                                            {printf("array_decs:%d\n",counter );counter++;$$=$1;};																
string_multiple_dec: ',' ID '[' expression ']' ASS STRVAL string_multiple_dec   {counter++;$$=NULL;}  
|',' ID '[' expression ']'  string_multiple_dec {counter++;$$=NULL;}
|',' ID '[' expression ']'    { counter++;$$=NULL;;}
|',' ID '[' expression ']' ASS STRVAL { counter++;$$=NULL;;};
string_dec: STR ID '[' expression ']' ASS STRVAL  { counter++;$$=NULL;;}
| STR ID '[' expression ']'  { counter++;$$=NULL;;};

 
/*-------------------------------------------Statments--------------------------------------------------------------------*/
condition_statments: condition_statment condition_statments{ counter++;$$=NULL;;}
| condition_statment { counter++;$$=NULL;;} ;
condition_statment: assignment ';'{ counter++;$$=NULL;;}
| call ';' { counter++;$$=NULL;;} 
| conditions { counter++;$$=NULL;;}
| loops { counter++;$$=NULL;;}
| return { counter++;$$=NULL;;}
| '{' condition_statments '}' { counter++;$$=NULL;;};
/*---------------------------------------Assignment----------------------------------------------------------------------*/
assignment: primitive_assignment { counter++;$$=NULL;;}
| index_assigment { counter++;$$=NULL;;}
| pointer_assigment { counter++;$$=NULL;;};
primitive_assignment: ID ASS expression  { counter++;$$=NULL;;};
index_assigment: ID '[' expression ']' ASS expression  { counter++;$$=NULL;;};
pointer_assigment: MULT ID ASS expression  { counter++;$$=NULL;;};
/*----------------------------------------Code Block--------------------------------------------------------------------*/
block: '{' body '}' { printf("block:%d\n",counter );counter++;;$$=$2;;};
/*-----------------------------------------Procedure/Function Calls-----------------------------------------------------*/
call: ID '(' func_expressions ')' { counter++;$$=NULL;;}
|ID '(' ')' { counter++;$$=NULL;;};
func_expressions: expression ',' func_expressions { counter++;$$=NULL;;}
| expression { counter++;$$=NULL;;}
/*----------------------------------------Conditions--------------------------------------------------------------------*/
conditions: IF '(' expression ')' condition_statment %prec IF { counter++;$$=NULL;;}
|IF '(' expression ')' condition_statment ELSE condition_statment { counter++;$$=NULL;;};
/*-----------------------------------------Loops------------------------------------------------------------------------*/
loops: do_while { counter++;$$=NULL;;}
| for { counter++;$$=NULL;;}
| while { counter++;$$=NULL;;};
while: WHILE '(' expression ')' condition_statment { counter++;$$=NULL;;};
do_while: DO '{' condition_statments '}' WHILE '(' expression ')' ';' { counter++;$$=NULL;;};
for: FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' condition_statment { counter++;$$=NULL;;};
/*-----------------------------------------Return-----------------------------------------------------------------------*/
return: RETURN expression ';' { counter++;$$=NULL;;};
/*-----------------------------------------Expression--------------------------------------------------------------------*/
expression: expression PLUS expression   { counter++;$$=NULL;;}           				
| expression MINUS expression         { counter++;$$=NULL;;}      												
| expression MULT expression	{ counter++;$$=NULL;;}
| expression DIV expression    { counter++;$$=NULL;;}
| expression OR expression		{ counter++;$$=NULL;;}						
| expression AND expression			{ counter++;$$=NULL;;}												
| expression EQ expression			{ counter++;$$=NULL;;}				
| expression NOTEQ expression { counter++;$$=NULL;;}
| expression L expression		{ counter++;$$=NULL;;}						
| expression LE expression	{ counter++;$$=NULL;;}						
| expression GR expression		{ counter++;$$=NULL;;}							
| expression GRE expression		{ counter++;$$=NULL;;}
| unary_operator expression %prec UNARY		{ counter++;$$=NULL;;}									
| literal_val                   { counter++;$$=$1;;}
| ID			{ counter++;$$=NULL;;}	
| call			{ counter++;$$=NULL;;}	
| ADDRS ID											{ counter++;$$=NULL;;}
| ADDRS ID '[' expression ']'							{ counter++;$$=NULL;;}		
| '|' ID '|'			{ counter++;$$=NULL;;}								
| '(' expression ')'      { counter++;$$=NULL;;}																																						
| ID '[' expression ']'	{ counter++;$$=NULL;;};

unary_operator: PLUS { counter++;$$=NULL;;} 
| MINUS { counter++;$$=NULL;;}
| MULT { printf("UNARY *");counter++;$$=NULL;;}
| NOT { counter++;$$=NULL;;};



/*--------------------------------------------Literal Values-------------------------------------------------------------------*/
literal_val: BOOLVAL	{ counter++;$$=mknode($1,NULL,NULL);}									
	|CHARVAL	{ counter++;$$=mknode($1,NULL,NULL);}								
	|DECVAL			{ counter++;$$=mknode($1,NULL,NULL);}					
	|HEXVAL				{ counter++;$$=mknode($1,NULL,NULL);}					
	|REALVAL { counter++;$$=mknode($1,NULL,NULL);}
    |STRVAL		{ counter++;$$=mknode($1,NULL,NULL);}							
	|PNULL	{ counter++;$$=mknode($1,NULL,NULL);}									;

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