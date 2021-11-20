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
void printTree(node *tree,int tabsNum);
int yyerror();
int yywrap();
void printToken(node *tree,int tabsNum);
void printTabs(int tabsNum);
node* mkrp();
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
<<<<<<< HEAD
%type <node> return expression 
%type <node> condition_statment condition_statments
%type <string> unary_operator literal_val
=======
%type <node> return expression literal_val
%type <node> condition_statment condition_statments
%type <string> unary_operator
>>>>>>> 23f217b02d5c82b70967deeab412c3219dbc449b

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
program: code							                    {printTree($1,0);};
code: functions                          					{$$=mknode("CODE",$1,mkrp());};
/*----------------------------------------Functions---------------------------------------------------*/
<<<<<<< HEAD
functions: function functions    					        {$$=mknode("",$1,$2);}                     
| procedure	functions									    {$$=mknode("",$1,$2);}  	
| function													{$$=$1;} 
| procedure													{$$=$1;};																										
function:  VALTYPE ID '(' parameter_list ')' block       	        {$$=mknode("FUNCTION",mknode($2,mknode("ARGS",$4,mknode(")",NULL,mknode($1,NULL,$6))),NULL),mkrp());};
procedure: VOID ID '(' parameter_list ')' block          	        {$$=mknode("PROCEDURE",mknode($2,mknode("ARGS",$4,mknode(")",NULL,mknode("VOID",NULL,$6))),NULL),mkrp());};


/* BOOOOOOOOOOOOOOOOOOOOODY */
body: function body 										{$$=mknode("",$1,$2);} 
| procedure body                                            {$$=mknode("",$1,$2);}
| var_decs body 											{$$=mknode("",$1,$2);}
| block	condition_statments										            {$$=mknode("", $2,$1);} 
| block                                                        {$$=$1;} 
| condition_statments %prec '}'									{$$= $1;}
| epsilon													{$$=NULL;};
parameter_list: param ';' parameter_list  									{$$=mknode("",$1,$3);}
| param	                                                    {$$=$1;}
| epsilon													{$$=mknode("(NO_ARGS)",NULL,NULL);};
param: VALTYPE id_list											{$$=mknode($1,$2,NULL);};            
id_list: ID ','  id_list 										   {$$=mknode($1,$3,NULL);}                                          
| ID                                                       {$$=mknode($1,NULL,NULL);};
/*---------------------------------------Variable Declarations-----------------------------------------------------------*/	
                                                     
var_decs: primitive_decs								   {$$=$1;}
| string_decs                                               {$$=$1;};
/* --------------------pointer_decs TODO---------------------*/
primitive_decs: primitive_dec primitive_multiple_decs                    {$$=mknode("",$1,$2);}
| primitive_dec ';'                                                              {$$=$1;};
primitive_multiple_decs: ',' ID  primitive_multiple_decs						  {$$=mknode($2,NULL,$3);} 
| ',' ID ASS expression primitive_multiple_decs                               {$$=mknode("",mknode("=",mknode($2,NULL,NULL),$4),$5);}
| ',' ID  ';'                                                                        {$$=mknode($2,NULL,NULL);}
| ',' ID ASS expression	';'													      {$$=mknode("=",mknode($2,NULL,NULL),$4);};
primitive_dec: VAR VALTYPE ID ASS expression                              {$$=mknode($2,mknode("=",mknode($3,NULL,NULL),$5),NULL);}
|VAR VALTYPE ID 															  {$$=mknode($2,mknode($3,NULL,NULL),NULL);}  ;
string_decs: string_dec string_multiple_dec ';'                             {$$=mknode("STRING",$1,$2);}
| string_dec ';'                                                            {$$=$1;};		

string_multiple_dec: ',' ID '[' expression ']' ASS STRVAL string_multiple_dec   {$$=mknode("",mknode("=",mknode($2,$4,NULL),mknode($7,NULL,NULL)),$8);}  
|',' ID '[' expression ']'  string_multiple_dec  {$$=mknode("",mknode($2,$4,NULL),$6);} 
|',' ID '[' expression ']'    {$$=mknode("",mknode($2,$4,NULL),NULL);}
|',' ID '[' expression ']' ASS STRVAL {$$=mknode("",mknode("=",mknode($2,$4,NULL),mknode($7,NULL,NULL)),NULL);};
string_dec: STR ID '[' expression ']' ASS STRVAL  {$$=mknode($1,mknode("=",mknode($2,$4,NULL),mknode($7,NULL,NULL)),NULL);}
| STR ID '[' expression ']'  {$$=mknode($1,mknode($2,$4,NULL),NULL);};

 
/*-------------------------------------------Statments--------------------------------------------------------------------*/
condition_statments: condition_statment condition_statments {$$=mknode("",$1,$2);}
| condition_statment { $$=$1;};
condition_statment: assignment ';'{ $$=$1;}
| call ';' { $$=mknode("FUNC_CALL",$1,NULL);} 
| conditions { $$=$1;}
| loops { $$=$1;}
| return { $$=$1;}
| '{' condition_statments '}' { $$=mknode("COND_BLOCK",$2,NULL);} ;
/*---------------------------------------Assignment----------------------------------------------------------------------*/
assignment: primitive_assignment { $$=$1;}
| index_assigment { $$=$1;}
| pointer_assigment { $$=$1;};
primitive_assignment: ID ASS expression  { $$=mknode($2,mknode($1,NULL,NULL),$3);};
index_assigment: ID '[' expression ']' ASS expression  { $$=mknode($5,mknode($1,$3,NULL),$6);};
pointer_assigment: MULT ID ASS expression  { $$=mknode($3,mknode("PTR",mknode($2,NULL,NULL),NULL),$4);};
=======
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
<<<<<<< Updated upstream
=======
>>>>>>> 23f217b02d5c82b70967deeab412c3219dbc449b
>>>>>>> Stashed changes
/*----------------------------------------Code Block--------------------------------------------------------------------*/
block: '{' body '}' { $$=mknode("BODY",$2,NULL);};
/*-----------------------------------------Procedure/Function Calls-----------------------------------------------------*/
<<<<<<< Updated upstream
=======
<<<<<<< HEAD
call: ID '(' func_expressions ')' {$$=mknode($1,NULL,$3);}
|ID '(' ')' { $$=mknode($1,NULL,NULL);};
func_expressions: expression ',' func_expressions { $$=mknode("",$1,$3);}
| expression { $$=$1;};
=======
>>>>>>> Stashed changes
call: ID '(' func_expressions ')' { counter++;$$=NULL;;}
|ID '(' ')' { counter++;$$=NULL;;};
func_expressions: expression ',' func_expressions { counter++;$$=NULL;;}
| expression { counter++;$$=NULL;;}
<<<<<<< Updated upstream
=======
>>>>>>> 23f217b02d5c82b70967deeab412c3219dbc449b
>>>>>>> Stashed changes
/*----------------------------------------Conditions--------------------------------------------------------------------*/
conditions: IF '(' expression ')' condition_statment %prec IF { $$=mknode("IF",$5,$3);}
|IF '(' expression ')' condition_statment ELSE condition_statment { $$=mknode("",mknode("IF",$5,$3),mknode("ELSE",$7,NULL));};
/*-----------------------------------------Loops------------------------------------------------------------------------*/
loops: do_while { $$=$1;}
| for { $$=$1;}
| while { $$=$1;};
while: WHILE '(' expression ')' condition_statment {$$=mknode("WHILE",$5,$3);};
do_while: DO '{' condition_statments '}' WHILE '(' expression ')' ';' { $$=mknode("DO",$3,mknode("WHILE",NULL,$7));};
for: FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' condition_statment 
{ $$=mknode("FOR",mknode("INIT",$3,mknode("CONDITION",$5,mknode("UPDATE",$7,$9))),NULL);};
/*-----------------------------------------Return-----------------------------------------------------------------------*/
return: RETURN expression ';' { mknode("RET",NULL,$2);};
/*-----------------------------------------Expression--------------------------------------------------------------------*/
<<<<<<< HEAD
expression: expression PLUS expression   {$$=mknode($2,$1,$3);}           				
| expression MINUS expression        {$$=mknode($2,$1,$3);}       												
| expression MULT expression	{$$=mknode($2,$1,$3);}  
| expression DIV expression    {$$=mknode($2,$1,$3);}    
| expression OR expression		{$$=mknode($2,$1,$3);}  						
| expression AND expression			{$$=mknode($2,$1,$3);}    											
| expression EQ expression			{$$=mknode($2,$1,$3);} 		
| expression NOTEQ expression {$$=mknode($2,$1,$3);}  
| expression L expression		{$$=mknode($2,$1,$3);} 					
| expression LE expression	{$$=mknode($2,$1,$3);}   					
| expression GR expression		{$$=mknode($2,$1,$3);} 							
| expression GRE expression		{$$=mknode($2,$1,$3);} 
| unary_operator expression %prec UNARY		{ $$=mknode($1,$2,NULL);}									
| literal_val                   { $$=mknode($1,NULL,NULL);}
| ID			{ $$=mknode($1,NULL,NULL);}	
| call			{ $$=mknode("FUNC_CALL",$1,NULL);} 
| ADDRS ID											{ $$=mknode("ADDRESS",mknode($2,NULL,NULL),NULL);}
| ADDRS ID '[' expression ']'							{ $$=mknode("ADDRESS",mknode($2,$4,NULL),NULL);}		
| '|' ID '|'			{ $$=mknode("LEN",NULL,mknode($2,NULL,NULL));}								
| '(' expression ')'      { $$=$2;}																																						
| ID '[' expression ']'	{ $$=mknode($1,$3,NULL);};
unary_operator: PLUS { $$=$1;}
| MINUS { $$=$1;}
| MULT { $$=$1;}
| NOT { $$=$1;};
=======
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
>>>>>>> 23f217b02d5c82b70967deeab412c3219dbc449b



/*--------------------------------------------Literal Values-------------------------------------------------------------------*/
literal_val: BOOLVAL	{$$=$1;}									
	|CHARVAL	{$$=$1;}							
	|DECVAL			{$$=$1;}				
	|HEXVAL				{$$=$1;}				
	|REALVAL {$$=$1;}
    |STRVAL		{$$=$1;}							
	|PNULL	{$$=$1;}									;

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
void printTree(node *tree,int tabs){
	printToken(tree,tabs);
	if(tree->left){
		if(!strcmp(tree->token,""))
			printTree(tree->left,tabs);
		else	 
			printTree(tree->left,tabs+1);
	}
	if(tree->right){
	printTree(tree->right,tabs);
	}
}
void printToken(node *tree,int tabs){
  if(!strcmp(tree->token, "CODE")){
	  printTabs(tabs);
	  printf("(CODE\n");
  }
  else if(!strcmp(tree->token, "")){}
  else if(!strcmp(tree->token, "FUNCTION")){
	  printTabs(tabs);
	  printf("(FUNCTION\n");
  }
  else if(!strcmp(tree->token, "PROCEDURE")){
	  printTabs(tabs);
	  printf("(PROCEDURE\n");	
  }else if(!strcmp(tree->token, "ARGS")){
	  printTabs(tabs);
	  printf("(ARGS\n");
  }
  else{
	  printTabs(tabs);
	  printf("%s\n",tree->token);
  }

}
node* mkrp(){
	return mknode(")",NULL,NULL);
}
void printTabs( int tabs){
	for( int i=tabs;i>0;i--)
	 printf("\t");
}
