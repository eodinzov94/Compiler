%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lex.yy.c"
#include <regex.h>

typedef struct node
{
 char *value;
 char *token;
 struct node *left;
 struct node *right;
} node;
node *mknode(char* value,char *token, node *left, node *right);
void printTree(node *tree,int tabsNum);
int yyerror();
int yywrap();
void printToken(node *tree,int tabsNum);
void printTabs(int tabsNum);
node* mkleaf(char* value,char* token);
char* mkcat(char* s1, char* s2);
void freeTree(node *tree);
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
%type <node> return expression
%type <node> statement statements condition_statement
%type <string> unary_operator literal_val

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



/*#################################################################RULES###########################################################################*/
%%
/*======================================================Start program======================================================*/
program: code							                    {printTree($1,0);freeTree($1);};
code: functions                          					{$$=mknode("CODE","",$1,mkleaf(")",""));};
/*======================================================Functions======================================================*/
functions: function functions    					        {$$=mknode("","",$1,$2);}                     
| procedure	functions									    {$$=mknode("","",$1,$2);}  	
| function													{$$=$1;} 
| procedure													{$$=$1;};																										
function:  VALTYPE ID '(' parameter_list ')' '{' body '}'       	        {$$=mknode("FUNCTION","",mknode($2,"",mknode("ARGS","",$4,mknode(")","",NULL,mknode(mkcat("RET_TYPE ",$1),"",NULL,mknode("BODY","",$7,mkleaf(")",""))))),NULL),mkleaf(")",""));};
procedure: VOID ID '(' parameter_list ')' '{' body '}'          	        {$$=mknode("PROCEDURE","",mknode($2,"",mknode("ARGS","",$4,mknode(")","",NULL,mknode(mkcat("RET_TYPE ","VOID"),"",NULL,mknode("BODY","",$7,mkleaf(")",""))))),NULL),mkleaf(")",""));};

body: function body 										{$$=mknode("","",$1,$2);} 
| procedure body                                            {$$=mknode("","",$1,$2);}
| var_decs body 											{$$=mknode("","",$1,$2);}
| statements %prec '}'									{$$= $1;}
| epsilon													{$$=NULL;};
parameter_list: param ';' parameter_list  									{$$=mknode("","",$1,$3);}
| param	                                                    {$$=$1;}
| epsilon													{$$=mkleaf("(NO_ARGS)","");};
param: VALTYPE id_list											{$$=mknode(mkcat("(",$1),"VALTYPE",$2,mkleaf(")",""));};            
id_list: ID ','  id_list 										   {$$=mknode($1,"ID",NULL,$3);}                                          
| ID                                                       {$$=mkleaf($1,"ID");};
/*======================================================Variable Declarations======================================================*/	
                                                     
var_decs: primitive_decs								   {$$=$1;}
| string_decs                                               {$$=$1;};
primitive_decs: primitive_dec primitive_multiple_decs                    {$$=mknode("","D-RIGHT",$1,$2);}
| primitive_dec ';'                                                              {$$=$1;};
primitive_multiple_decs: ',' ID  primitive_multiple_decs						  {$$=mknode($2,"ID",NULL,$3);} 
| ',' ID ASS expression primitive_multiple_decs                               {$$=mknode("","",mknode("","",mknode("=","ASS",mkleaf($2,"ID"),$4),mkleaf(")","")),$5);}
| ',' ID  ';'                                                                        {$$=mkleaf($2,"ID");}
| ',' ID ASS expression	';'													      {$$=mknode("","",mknode("=","ASS",mkleaf($2,"ID"),$4),mkleaf(")",""));};
primitive_dec: VAR VALTYPE ID ASS expression                              {$$=mknode("","",mknode($2,"VALTYPE",mknode("=","ASS",mkleaf($3,"ID"),$5),NULL),mkleaf(")","CLOSE_RIGHT"));}
|VAR VALTYPE ID 															  {$$=mknode($2,"VALTYPE",mkleaf($3,"ID"),NULL);}  ;
string_decs: string_dec string_multiple_dec ';'                             {$$=mknode("","D-RIGHT",$1,$2);}
| string_dec ';'                                                            {$$=$1;};		

string_multiple_dec: ',' ID '[' expression ']' ASS STRVAL string_multiple_dec   {$$=mknode("","",mknode("","",mknode("=","ASS",mknode($2,"ID",$4,NULL),mkleaf($7,"")),mkleaf(")","")),$8);}  
|',' ID '[' expression ']'  string_multiple_dec  {$$=mknode("","",mknode($2,"ID",$4,NULL),$6);} 
|',' ID '[' expression ']'    {$$=mknode("","",mknode($2,"ID",$4,NULL),NULL);}
|',' ID '[' expression ']' ASS STRVAL {$$=mknode("","",mknode("=","ASS",mknode($2,"ID",$4,NULL),mkleaf($7,"")),mkleaf(")",""));};
string_dec: STR ID '[' expression ']' ASS STRVAL  {$$=mknode($1,"",mknode("=","ASS",mknode($2,"ID",$4,NULL),mkleaf($7,"")),mkleaf(")","CLOSE_RIGHT"));}
| STR ID '[' expression ']'  {$$=mknode($1,"",mknode($2,"ID",$4,NULL),NULL);};

 
/*======================================================Statments======================================================*/
statements: statement statements {$$=mknode("","",$1,$2);}
| statement { $$=$1;};
statement: block { $$=$1;}
| condition_statement { $$=$1;};
condition_statement:call ';' { $$=mknode("FUNC_CALL","",$1,mkleaf(")",""));} 
| conditions { $$=mknode("","",$1,NULL);}
| loops { $$=mknode("","",$1,NULL);}
| return { $$=mknode("","",$1,NULL);}
| assignment ';'{ $$=mknode("","RIGHT",$1,NULL);}
| '{' statements '}' { $$=mknode("BLOCK","",$2,mkleaf(")",""));} 
| ';' {$$=NULL;};
/*======================================================Assignment======================================================*/
assignment: primitive_assignment { $$=$1;}
| index_assigment { $$=$1;}
| pointer_assigment { $$=$1;}; 
primitive_assignment: ID ASS expression  { $$=mknode("","LEFT",mknode($2,"ASS",mkleaf($1,"ID"),$3),mkleaf(")","CLOSE_ASS"));};
index_assigment: ID '[' expression ']' ASS expression  {$$=mknode("","LEFT",mknode($5,"ASS",mknode($1,"",$3,NULL),$6),mkleaf(")","CLOSE_ASS"));};
pointer_assigment: MULT ID ASS expression  { $$=mknode("","LEFT",mknode($3,"ASS",mknode("PTR","",mkleaf($2,"ID"),NULL),$4),mkleaf(")","CLOSE_ASS"));};
/*----------------------------------------Code Block--------------------------------------------------------------------*/
block: '{' body '}' { $$=mknode("BLOCK","",$2,mkleaf(")",""));};
/*======================================================Procedure/Function Calls======================================================*/
call: ID '(' func_expressions ')' {$$=mknode($1,"ID",$3,NULL);}
|ID '(' ')' { $$=mkleaf($1,"ID");};
func_expressions: expression ',' func_expressions { $$=mknode("","",$1,$3);}
| expression { $$=$1;};
/*======================================================Conditions======================================================*/
conditions: IF '(' expression ')' condition_statement %prec IF { $$=mknode("IF","",$3,mknode("","RIGHT",$5,mkleaf(")","")));}
| IF '(' expression ')' '{' '}' %prec IF { $$=mknode("IF","",$3,mkleaf(")",""));}
|IF '(' expression ')' '{' '}' ELSE condition_statement { $$=mknode("","",mknode("IF","",$3,NULL),mknode("ELSE","",mknode("","RIGHT",$8,NULL),mkleaf(")","")));}
|IF '(' expression ')' condition_statement ELSE '{' '}' { $$=mknode("","",mknode("IF","",$3,mknode("","RIGHT",$5,NULL)),mknode("ELSE","",NULL,mkleaf(")","")));}
|IF '(' expression ')' condition_statement ELSE condition_statement { $$=mknode("","",mknode("IF","",$3,mknode("","RIGHT",$5,NULL)),mknode("ELSE","",mknode("","",$7,NULL),mkleaf(")","")));}
|IF '(' expression ')' '{' '}' ELSE '{' '}' { $$=mknode("","",mknode("IF","",$3,NULL),mknode("ELSE","RIGHT",NULL,mkleaf(")","")));};
/*======================================================Loops======================================================*/
loops: do_while { $$=$1;}
| for { $$=$1;}
| while { $$=$1;};
while: WHILE '(' expression ')' condition_statement {$$=mknode("WHILE","",$3,mknode("","RIGHT",$5,mkleaf(")","")));}
| WHILE '(' expression ')' '{' '}' {$$=mknode("WHILE","",$3,mkleaf(")",""));};
do_while: DO '{' statements '}' WHILE '(' expression ')' ';' { $$=mknode("DO","",$3,mknode("WHILE-COND","",$7,mkleaf(")","")));};
for: FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' condition_statement 
{ $$=mknode("FOR","",mknode("INIT","",$3,mknode("CONDITION","",$5,mknode("UPDATE","",$7,$9))),mkleaf(")",""));}
|FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' '{' '}'
{ $$=mknode("FOR","",mknode("INIT","",$3,mknode("CONDITION","",$5,mknode("UPDATE","",$7,NULL))),NULL);};
/*======================================================Return======================================================*/
return: RETURN expression ';' { $$ = mknode("RETURN","",$2,mkleaf(")",""));};
/*======================================================Expression======================================================*/
expression: expression PLUS expression   {$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}           				
| expression MINUS expression			 {$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}       												
| expression MULT expression			{$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}
| expression DIV expression  			 {$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}
| expression OR expression				{$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}					
| expression AND expression				{$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}   											
| expression EQ expression				{$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}		
| expression NOTEQ expression			 {$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}
| expression L expression				{$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}					
| expression LE expression				{$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}				
| expression GR expression				{$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}							
| expression GRE expression				{$$=mknode("","",mknode($2,"OP",$1,$3),mkleaf(")",""));}
| unary_operator expression %prec UNARY		{ $$=mknode($1,"UN_OP",$2,NULL);}									
| literal_val                   { $$=mkleaf($1,"LIT");}
| ID			{ $$=mkleaf($1,"ID");}	
| call			{ $$=mknode("FUNC_CALL","",$1,mkleaf(")",""));} 
| ADDRS ID											{ $$=mknode("ADDRESS","",mkleaf($2,"ID"),NULL);}
| ADDRS ID '[' expression ']'							{ $$=mknode("ADDRESS","",mknode($2,"",$4,NULL),NULL);}		
| '|' ID '|'			{ $$=mknode("LEN","ID",mkleaf($2,"ID"),NULL);}								
| '(' expression ')'      { $$=$2;}																																						
| ID '[' expression ']'	{ $$=mknode($1,"ID",$3,NULL);};
unary_operator: PLUS { $$=$1;}
| MINUS { $$=$1;}
| MULT { $$=$1;}
| NOT { $$=$1;};



/*======================================================Literal Values======================================================*/
literal_val: BOOLVAL	{$$=$1;}									
	|CHARVAL	{$$=$1;}							
	|DECVAL			{$$=$1;}				
	|HEXVAL				{$$=$1;}				
	|REALVAL {$$=$1;}
    |STRVAL		{$$=$1;}							
	|PNULL	{$$=strdup("NULL");};

epsilon: ;
%%
/*####################################################END################################################################################*/


void main(){
    yyparse();
}
int yyerror(char *err){
    printf("%s at line %d\n",err, yylineno);
	printf("Unexpected token:'%s'\n",yytext);
	return 0;
}
int yywrap(){
	return 1;
}
node *mknode(char* value,char *token,node *left,node *right)
{
 node *newnode = (node*)malloc(sizeof(node));
 newnode->left = left;
 newnode->right = right;
 newnode->token = strdup(token);
 newnode->value = strdup(value);
 return newnode;
}
void printTree(node *tree,int tabs){
	if(!tree)
		return;
	printToken(tree,tabs);
	if(tree->left){
		if(!strcmp(tree->token,"RIGHT")){
			printTree(tree->left,tabs+1);
		}else if( !strcmp(tree->token,"LEFT")){
			printTree(tree->left,tabs-1);
		}
		else if(!strcmp(tree->value,"")){
			printTree(tree->left,tabs);
		}
		else{
			printTree(tree->left,tabs+1);
		}	
	}
	if (tree->right){
		if(!strcmp(tree->token,"ASS") || !strcmp(tree->token,"OP")){
			printTree(tree->right,tabs+1);
		}
		else if(!strcmp(tree->token,"D-RIGHT")){
			printTree(tree->right,tabs+1);
		}
		else {
			printTree(tree->right,tabs);
		}
	}
}
void printToken(node *tree,int tabs){
  if(!strcmp(tree->token, "OP") || !strcmp(tree->token, "ASS")){
	  printTabs(tabs);
	  printf("(%s\n", tree->value);
  }
  else if(!strcmp(tree->token, "CLOSE_ASS")){
	  printTabs(tabs-1);
	  printf("%s\n", tree->value);
  }
  else if(!strcmp(tree->token, "CLOSE_RIGHT")){
	  printTabs(tabs+1);
	  printf("%s\n", tree->value);
  }
  else if(!strcmp(tree->value, "CODE")){
	  printTabs(tabs);
	  printf("(CODE\n");
  }
  else if(!strcmp(tree->value, "")){}
  else if(!strcmp(tree->value, "FUNCTION")){
	  printTabs(tabs);
	  printf("(FUNCTION\n");
  }
  else if(!strcmp(tree->value, "PROCEDURE")){
	  printTabs(tabs);
	  printf("(PROCEDURE\n");	
  }else if(!strcmp(tree->value, "ARGS")){
	  printTabs(tabs);
	  printf("(ARGS\n");
  }else if(!strcmp(tree->value, "BODY")){
	  printTabs(tabs);
	  printf("(BODY\n");
  }else if(!strcmp(tree->value, "BLOCK")){
	  printTabs(tabs);
	  printf("(BLOCK\n");
  }else if(!strcmp(tree->value, "WHILE")){
	  printTabs(tabs);
	  printf("(WHILE\n");
  }
  else if(!strcmp(tree->value, "FOR")){
	  printTabs(tabs);
	  printf("(FOR\n");
  }
  else if(!strcmp(tree->value, "RETURN")){
	  printTabs(tabs);
	  printf("(RETURN\n");
  }
  else if(!strcmp(tree->value, "DO")){
	  printTabs(tabs);
	  printf("(DO\n");
  }else if(!strcmp(tree->value, "FUNC_CALL")){
	  printTabs(tabs);
	  printf("(FUNC_CALL\n");
  }else if(!strcmp(tree->value, "IF")){
	  printTabs(tabs);
	  printf("(IF\n");
  }
  else{
	  printTabs(tabs);
	  printf("%s\n",tree->value);
  }

}
node* mkleaf(char* value,char* token){
	return mknode(value,token,NULL,NULL);
}
void printTabs( int tabs){
	for( int i=tabs;i>0;i--)
	 printf("\t");
}
void freeTree(node *tree){
	if(!tree)
		return;
	if(tree->left){
		freeTree(tree->left);
	}
	if(tree->right){
	 freeTree(tree->right);
	}
    free(tree->token);
	free(tree->value);
	free(tree);
}
char* mkcat(char* s1,char*s2){
	int len1= strlen(s1);
	int len2= strlen(s2);
	int newlen = len1+len2+1;
	char *newstr = (char*)malloc(newlen);
	for(int i=0; i<len1; i++){
		newstr[i] = s1[i];
	}
	for(int i=len1; i<newlen-1; i++){
		newstr[i] = s2[i-len1];
	}
	newstr[newlen] = '\0';
	return newstr;
}