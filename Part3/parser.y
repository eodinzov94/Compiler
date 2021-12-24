%{
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lex.yy.c"
#define BOOL   0
#define REAL   1
#define INT    2
#define CHAR   3
#define INTP   4
#define CHARP  5
#define REALP  6
#define STRING 7
#define VOIDF  8
#define NULLPTR 9
#define GLOBAL -1
#define BLOCK 10
typedef enum { FALSE, TRUE } bool;

typedef struct node
{
 char *value;
 char *desc;
 char *code;
 char *var;
 char *label;
 char *truelabel;
 char *falselabel;
 struct node *left;
 struct node *right;
} node;

typedef struct func {
	char *name;
	int *argsType;
	int returnType;
	int numOfArgs;
	struct func* next;
}func;

typedef struct var {
	char *name;
	int type;
	struct var* next;
}var;

typedef struct env {
	struct func *funcs;
	struct var *vars;
	int returnType;
	struct env *next;
}env;



/*PART 1*/
node *mknode(char* value,char *desc, node *left, node *right);
node* mkleaf(char* value,char* desc);
void freeTree(node *tree);
char* mkcat(char* s1,char*s2);
int yyerror();
int yywrap();
/*PART 2*/
func* mkFunc(char* name,int* argsType,int returnType,int numOfArgs);
func* addFunc(func* head,func* node);
var* mkVar(char* name, int type);
var* addVar(var* head, var* node);
env* mkEnv(int returnType);
env* addFuncToEnv(env* env, func* func);
env* addVarToEnv(env* env, var* var);
env* pushEnv(env* head, env* node);
env* popEnv(env* head);
int evaluateFuncCall(node*,func*,env*);
int evalExpType(node* root, env* enviroment);
var* findVar(var* node,char* name);
var* findVarForAssigment (env*,char* name);
bool findFunc(func* node,char* name);
void freeEnvNode(env* node);
void freeFuncs( func* head);
void freeVars( var* head);
void startSemanticalAnalysis(node* root);
int getType(char* type);
void freeENVs(env* head);
void checkStrDeclarations(node * root);
char* getTypeByNumber(int);
int getTypeForPrimDec(node *root);
void checkPrimDeclarations(node* root,int type);
int getConcreteType(int pType);
func* getFuncByID(env*, char*);
void checkAssigment(node* root);
void checkLoops(node* root);
void checkConds(node* root);
void checkConditionStatement(node* root);
void checkVarDecs(node* root);
bool assignmentCheck(int varType, int expType);
int getIDVarType(env*, char*);
void exitWithError();
void hasReturn(node* root);
int* getArgsTypeFromRoot(node* root,int* len);
env* addArgsToEnv(env* env,node* root,int numOfArgs);
void findCheckMain();
int numOfmains = 0;
int varLabel = 0;
int codeLabel = 1;
bool isPointer(int type);
/*PART3*/
char* newCodeLabel();
char* newVarLabel();
void print (node* root);
void To3AC(node* root);
void condsTo3AC(node* root);
void loopsTo3AC(node* root);
void assigmentTo3AC(node *root);
void conditionStatementTo3AC(node* root);
void strDeclarationsTo3AC(node * root);
void varDecsTo3AC(node* root);
void FuncCallTo3AC(node* root,func*);
void primDeclarationsTo3AC(node* root);
void expTo3AC(node* root);
int calcBytesForFunc(func*);
int getBytesByType(int);
func* findFuncInGlobal(char* name);
env* global = NULL;
env* pointerToGlobalOnly = NULL;
node* root = NULL;
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
%type <node> return expression decs_with_stats 
%type <node> statement statements condition_statement literal_val integer

%type <string> unary_operator 

%nonassoc IF
%nonassoc ELSE


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
/*======================================================Start Program======================================================*/
program: code							                    {	root=$1;
																startSemanticalAnalysis($1);
																To3AC($1);
																/*freeTree($1);
																freeENVs(global);
																getchar();*/
															}
;
code: functions                          					{$$=mknode("CODE","",$1,mkleaf(")",""));}
;
/*======================================================Functions======================================================*/
functions: function functions    					        {$$=mknode("","FUNCTIONS",$1,$2);}                     
| procedure	functions									    {$$=mknode("","FUNCTIONS",$1,$2);}  	
| function													{$$=mknode("","FUNCTIONS",$1,NULL);} 
| procedure													{$$=mknode("","FUNCTIONS",$1,NULL);}
;																										
function:  VALTYPE ID '(' parameter_list ')' '{' body '}'   {$$=mknode("FUNCTION","",mknode($2,"",mknode("ARGS","",$4,mknode(")","",NULL,mknode($1,"",NULL,mknode("BODY","",$7,mkleaf(")",""))))),NULL),mkleaf(")",""));};
procedure: VOID ID '(' parameter_list ')' '{' body '}'      {$$=mknode("PROCEDURE","",mknode($2,"",mknode("ARGS","",$4,mknode(")","",NULL,mknode("VOID","",NULL,mknode("BODY","",$7,mkleaf(")",""))))),NULL),mkleaf(")",""));}
;
body: function body 										{$$=mknode("FUNCFROMBODY","FUNCTIONS",$1,$2);} 
| procedure body                                            {$$=mknode("FUNCFROMBODY","FUNCTIONS",$1,$2);}
| var_decs body												{$$=mknode("","VARDECS",$1,$2);}
| statements 												{$$=$1;}
;
parameter_list: param ';' parameter_list  					{$$=mknode("","",$1,$3);}
| param	                                                    {$$=mknode("","",$1,NULL);}
| epsilon													{$$=mkleaf("(NO_ARGS)","");};
param: VALTYPE id_list										{$$=mknode(mkcat("(",$1),"VALTYPE",$2,mkleaf(")",""));};            
id_list: ID ','  id_list 									{$$=mknode($1,"ID",NULL,$3);}                                          
| ID                                                        {$$=mkleaf($1,"ID");}
;
decs_with_stats: var_decs decs_with_stats 					{$$=mknode("","VARDECS",$1,$2);}
| statements												{$$=$1;}
;

/*======================================================Variable Declarations======================================================*/	                                         
var_decs: primitive_decs								    {$$=mknode("","PRIM_DECS",NULL,$1);}
| string_decs                                               {$$=mknode("","STR_DECS",NULL,$1);};
primitive_decs: primitive_dec primitive_multiple_decs       {$$=mknode("","D-RIGHT",$1,$2);}
| primitive_dec ';'                                         {$$=mknode("","",NULL,$1);};
primitive_multiple_decs: ',' ID  primitive_multiple_decs	{$$=mknode($2,"ID",NULL,$3);} 
| ',' ID ASS expression primitive_multiple_decs             {$$=mknode("","DEC_ASS_PRIM_NODE",mknode("","",mknode("=","ASS",mkleaf($2,"ID"),$4),mkleaf(")","")),$5);}
| ',' ID  ';'                                               {$$=mkleaf($2,"ID");}
| ',' ID ASS expression	';'									{$$=mknode("","DEC_ASS_PRIM_LEAF",mknode("=","ASS",mkleaf($2,"ID"),$4),mkleaf(")",""));};
primitive_dec: VAR VALTYPE ID ASS expression                {$$=mknode("","DEC_ASS_PRIM",mknode($2,"",mknode("=","ASS",mkleaf($3,"ID"),$5),NULL),mkleaf(")","CLOSE_RIGHT"));}
|VAR VALTYPE ID 											{$$=mknode($2,"DEC_PRIM",mkleaf($3,"ID"),NULL);}  ;
string_decs: string_dec string_multiple_dec ';'             {$$=mknode("","D-RIGHT",$1,$2);}
| string_dec ';'                                            {$$=$1;}
;		
string_multiple_dec: ',' ID '[' integer ']' ASS STRVAL string_multiple_dec   
															{$$=mknode("","STR_3L",mknode("","",mknode("=","ASS",mknode($2,"ID",$4,NULL),mkleaf($7,"")),mkleaf(")","")),$8);}  
|',' ID '[' integer ']'  string_multiple_dec  			{$$=mknode("","STR_1L",mknode($2,"ID",$4,NULL),$6);} 
|',' ID '[' integer ']'    								{$$=mknode("","STR_1L",mknode($2,"ID",$4,NULL),NULL);}
|',' ID '[' integer ']' ASS STRVAL 						{$$=mknode("","STR_2L",mknode("=","ASS",mknode($2,"ID",$4,NULL),mkleaf($7,"")),mkleaf(")",""));}
;
string_dec: STR ID '[' integer ']' ASS STRVAL  			{$$=mknode($1,"STR_2L",mknode("=","ASS",mknode($2,"ID",$4,NULL),mkleaf($7,"")),mkleaf(")","CLOSE_RIGHT"));}
| STR ID '[' integer ']' 								{$$=mknode($1,"STR_1L",mknode($2,"ID",$4,NULL),NULL);}
;
/*======================================================Statements======================================================*/
statements: statement statements 							{$$=mknode("","STATEMENTS",$1,$2);}
| epsilon													{$$=NULL;}
;
statement: block 											{$$=$1;}
| condition_statement 										{$$=mknode("","COND_STAT",NULL,$1);}
;
condition_statement:call ';' 								{$$=mknode("FUNC_CALL","FUNC_CALL",$1,mkleaf(")",""));} 
| conditions 												{$$=mknode("","CONDS",$1,NULL);}
| loops 													{$$=mknode("","LOOPS",$1,NULL);}
| return 													{$$=mknode("","RET",$1,NULL);}
| assignment ';'											{$$=mknode("","RIGHT",$1,NULL);}
;
assignment: primitive_assignment 							{$$=mknode("","PRIMASS",NULL,$1);}
| index_assigment 											{$$=mknode("","INDEXASS",NULL,$1);}
| pointer_assigment 										{$$=mknode("","PTRASS",NULL,$1);}
; 
primitive_assignment: ID ASS expression  					{$$=mknode("","LEFT",mknode($2,"ASS",mkleaf($1,"ID"),$3),mkleaf(")","CLOSE_ASS"));}
;
index_assigment: ID '[' expression ']' ASS expression  		{$$=mknode("","LEFT",mknode($5,"ASS",mknode($1,"ID",$3,NULL),$6),mkleaf(")","CLOSE_ASS"));}
;
pointer_assigment: MULT ID ASS expression  					{$$=mknode("","LEFT",mknode($3,"ASS",mknode("PTR","",mkleaf($2,"ID"),NULL),$4),mkleaf(")","CLOSE_ASS"));}
;
/*======================================================Code Block======================================================*/
block: '{' body '}' 										{$$=mknode("BLOCK","ENV_BLOCK",$2,mkleaf(")",""));}
;
/*======================================================Procedure/Function Calls======================================================*/
call: ID '(' func_expressions ')' 							{$$=mknode($1,"ID",$3,NULL);}
|ID '(' ')' 												{$$=mkleaf($1,"ID");}
;
func_expressions: expression ',' func_expressions 			{$$=mknode("","",$1,$3);}
| expression 												{$$=mknode("","LEFT",$1,NULL);}
;
/*======================================================Conditions======================================================*/
conditions: IF '(' expression ')' condition_statement %prec IF              		{$$=mknode("IF","IF",$3,mknode("","RIGHT",mknode("","COND_STAT",NULL,$5),mkleaf(")","")));}
| IF '(' expression ')' '{' decs_with_stats '}' %prec IF 							{$$=mknode("IF","IF",$3,mknode("","RIGHT",mknode("","INNER_ENV",NULL,$6),mkleaf(")","")));}
| IF '(' expression ')' '{' decs_with_stats '}' ELSE condition_statement 			{$$=mknode("","IFELSE",mknode("IF","IF",$3,mknode("","RIGHT",mknode("","INNER_ENV",NULL,$6),NULL)),mknode("ELSE","ELSE",mknode("","",mknode("","COND_STAT",NULL,$9),NULL),mkleaf(")","")));}
| IF '(' expression ')' condition_statement ELSE '{' decs_with_stats '}' 			{$$=mknode("","IFELSE",mknode("IF","IF",$3,mknode("","RIGHT",mknode("","COND_STAT",NULL,$5),NULL)),mknode("ELSE","ELSE",mknode("","",mknode("","INNER_ENV",NULL,$8),NULL),mkleaf(")","")));}
| IF '(' expression ')' condition_statement ELSE condition_statement        		{$$=mknode("","IFELSE",mknode("IF","IF",$3,mknode("","RIGHT",mknode("","COND_STAT",NULL,$5),NULL)),mknode("ELSE","ELSE",mknode("","",mknode("","COND_STAT",NULL,$7),NULL),mkleaf(")","")));}
| IF '(' expression ')' '{' decs_with_stats '}' ELSE '{' decs_with_stats '}' 		{$$=mknode("","IFELSE",mknode("IF","IF",$3,mknode("","RIGHT",mknode("","INNER_ENV",NULL,$6),NULL)),mknode("ELSE","ELSE",mknode("","",mknode("","INNER_ENV",NULL,$10),NULL),mkleaf(")","")));}
;
/*======================================================Loops======================================================*/
loops: do_while 											{$$=$1;}
| for 														{$$=$1;}
| while 													{$$=$1;}
;
while: WHILE '(' expression ')' condition_statement 		{$$=mknode("WHILE","",$3,mknode("","RIGHT",mknode("","COND_STAT",NULL,$5),mkleaf(")","")));}
| WHILE '(' expression ')' '{' decs_with_stats '}' 							{$$=mknode("WHILE","",$3,mknode("","RIGHT",mknode("","INNER_ENV",NULL,$6),mkleaf(")","")));}
;
do_while: DO '{' decs_with_stats '}' WHILE '(' expression ')' ';'{$$=mknode("DO","",mknode("","INNER_ENV",NULL,$3),mknode("WHILE-COND","",$7,mkleaf(")","")));};
for: FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' condition_statement 
															{$$=mknode("FOR","",mknode("INIT","",$3,mknode("CONDITION","",$5,mknode("UPDATE","",$7,mknode("","COND_STAT",NULL,$9)))),mkleaf(")",""));}
| FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' '{' decs_with_stats '}'
															{$$=mknode("FOR","",mknode("INIT","",$3,mknode("CONDITION","",$5,mknode("UPDATE","",$7,mknode("","INNER_ENV",NULL,$10)))),mkleaf(")",""));}
;
/*======================================================Return======================================================*/
return: RETURN expression ';' 								{$$ = mknode("RETURN","",$2,mkleaf(")",""));}
;
/*======================================================Expression======================================================*/
expression: expression PLUS expression   					{$$=mknode("","+",mknode($2,"OP",$1,$3),mkleaf(")",""));}           				
| expression MINUS expression			 					{$$=mknode("","-",mknode($2,"OP",$1,$3),mkleaf(")",""));}       												
| expression MULT expression								{$$=mknode("","*",mknode($2,"OP",$1,$3),mkleaf(")",""));}
| expression DIV expression  			 					{$$=mknode("","/",mknode($2,"OP",$1,$3),mkleaf(")",""));}
| expression OR expression									{$$=mknode("","||",mknode($2,"OP",$1,$3),mkleaf(")",""));}					
| expression AND expression									{$$=mknode("","&&",mknode($2,"OP",$1,$3),mkleaf(")",""));}   											
| expression EQ expression									{$$=mknode("","==",mknode($2,"OP",$1,$3),mkleaf(")",""));}		
| expression NOTEQ expression			 					{$$=mknode("","!=",mknode($2,"OP",$1,$3),mkleaf(")",""));}
| expression L expression									{$$=mknode("","<",mknode($2,"OP",$1,$3),mkleaf(")",""));}					
| expression LE expression									{$$=mknode("","<=",mknode($2,"OP",$1,$3),mkleaf(")",""));}				
| expression GR expression									{$$=mknode("",">",mknode($2,"OP",$1,$3),mkleaf(")",""));}							
| expression GRE expression									{$$=mknode("",">=",mknode($2,"OP",$1,$3),mkleaf(")",""));}
| unary_operator expression %prec UNARY						{$$=mknode($1,"UN_OP",$2,NULL);}									
| literal_val                   							{$$=$1;}
| ID														{$$=mkleaf($1,"ID");}	
| call														{$$=mknode("FUNC_CALL","",$1,mkleaf(")",""));} 
| ADDRS ID													{$$=mknode("ADDRESS","ADDR",mkleaf($2,"ID"),NULL);}
| ADDRS ID '[' expression ']'								{$$=mknode("ADDRESS","ADDR_INDEX",mknode($2,"ID",$4,NULL),NULL);}		
| '|' ID '|'												{$$=mknode("LEN","IDLEN",mkleaf($2,"ID"),NULL);}								
| '(' expression ')'      								    {$$=$2;}																																						
| ID '[' expression ']'										{$$=mknode($1,"ID_INDEX",$3,NULL);}
;
unary_operator: PLUS 										{$$=$1;}
| MINUS 													{$$=$1;}
| NOT 														{$$=$1;}
| MULT 														{$$=$1;}
;
/*======================================================Literal Values======================================================*/
literal_val: BOOLVAL										{$$=mkleaf($1,"BOOL");}									
|CHARVAL													{$$=mkleaf($1,"CHAR");}							
|integer 			                                        {$$=$1;}
|REALVAL 													{$$=mkleaf($1,"REAL");}
|STRVAL														{$$=mkleaf($1,"STR");}							
|PNULL														{$$=mkleaf("null_ptr","NULLP");}	
;
integer: DECVAL                                            {$$=mkleaf($1,"INT");}
|HEXVAL                                                    {$$=mkleaf($1,"INT");}
epsilon: 
;
%%
/*####################################################END################################################################################*/

/* PART 1*/
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
node *mknode(char* value,char *desc,node *left,node *right){
 node *newnode = (node*)malloc(sizeof(node));
 newnode->left = left;
 newnode->right = right;
 newnode->desc = strdup(desc);
 newnode->value = strdup(value);
 newnode->code = strdup("");
 newnode->var  = NULL;
 newnode->label = NULL;
 newnode->truelabel = NULL;
 newnode->falselabel = NULL;
 return newnode;
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
node* mkleaf(char* value,char* desc){
	return mknode(value,desc,NULL,NULL);
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
    free(tree->desc);
	free(tree->value);
	free(tree);
}


func* mkFunc(char* name,int* argsType,int returnType,int numOfArgs){
	func* node = (func*)malloc(sizeof(func));
	node->name = strdup(name);
	if(numOfArgs>0){
		node->argsType = argsType;
	}else{
		node->argsType = NULL;
	}
	node->numOfArgs = numOfArgs;
	node->returnType = returnType;
	node->next = NULL;
	if(!strcmp(name,"main")){
		numOfmains++;
	}
	return node;

}
func* addFunc(func* head,func* node){
	node->next = head;
	return node;
}
var* mkVar(char* name, int type){
	var* node = (var*)malloc(sizeof(var));
	node->name = strdup(name);
	node->type = type;
	node->next = NULL;
	return node;
}
var* addVar(var* head, var* node){
	node->next = head;
	return node;
}
env* mkEnv(int returnType){
	env* node = (env*)malloc(sizeof(env));
	node-> funcs = NULL;
	node->vars = NULL;
	node->next = NULL;
	node->returnType =returnType;
	return node;
}
env* addFuncToEnv(env* env, func* func){
	if(!env || !func){
		printf("Logical Error in AddFuncToEnv ");
		exit(1);
	}
	if(env->funcs == NULL){
		env->funcs=func;
	}else{
		if(!findFunc(env->funcs,func->name)){
			env->funcs = addFunc(env->funcs,func);
		}else{
			printf("\nFunction with name %s already declared\n",func->name);
			exitWithError();
		}
	}
	return env;
}
env* addVarToEnv(env* env, var* var){
	if(env->vars == NULL){
		env->vars=var;
	}else{
		if(!findVar(env->vars,var->name)){
			env->vars = addVar(env->vars,var);
		}else{
			printf("\nVariable with name %s already declared\n",var->name);
			exitWithError();
		}
	}
	return env;
}
env* pushEnv(env* head, env* node){
	node->next = head;
	return node;
}
env* popEnv(env* head){
	env * temp = head;
	head = head->next;
	freeEnvNode(temp);
	return head;
}

void freeEnvNode(env* node){
	if(node!=NULL){
		freeFuncs(node->funcs);
		freeVars(node->vars);
		free(node);
	}
}
void freeFuncs( func* head){
	func* temp = NULL;
	while(head != NULL){
		temp = head;
		head = head->next;
		free(temp->name);
		free(temp->argsType);
		free(temp);
	}
}
void freeVars( var* head){
	var* temp = NULL;
	while(head != NULL){
		temp = head;
		head = head->next;
		free(temp->name);
		free(temp);
	}
}
int getType(char* type){
	if(!strcmp(type,"bool") || !strcmp(type,"(bool") ){
		return BOOL;
	}else if(!strcmp(type,"real") || !strcmp(type,"(real")){
		return REAL;
	} else if(!strcmp(type,"int") || !strcmp(type,"(int")){
		return INT;
	} else if(!strcmp(type,"char") || !strcmp(type,"(char")){
		return CHAR;
	} else if(!strcmp(type,"int*")|| !strcmp(type,"(int*")){
		return INTP;
	} else if(!strcmp(type,"char*") || !strcmp(type,"(char*")){
		return CHARP;
	} else if(!strcmp(type,"real*")|| !strcmp(type,"(real*")){
		return REALP;
	}else if(!strcmp(type,"string")|| !strcmp(type,"(string")){
		return STR;
	}else return VOIDF;
}
char* getTypeByNumber(int type){
	if(type == BOOL ){
		return strdup("bool");
	}else if(type == REAL ){
		return strdup("real");
	}else if(type == INT ){
		return strdup("int");
	}else if(type == CHAR ){
		return strdup("char");
	}
	else if(type == INTP ){
		return strdup("int*");
	}else if(type == CHARP ){
		return strdup("char*");
	}
	else if(type == REALP ){
		return strdup("real*");
	}else if(type == STRING ){
		return strdup("string");
	}else if(type == NULLPTR){
		return strdup("null");
	}
	return strdup("Unsupported type!\n");
}

int getConcreteType(int type){
	if(type == INTP ){
		return INT;
	}else if(type == CHARP){
		return CHAR;
	}
	else if(type ==REALP){
		return REAL;
	}else{
		printf("/nLogical error inside getConcreteType /n");
		exitWithError();
	}
}
void freeENVs(env* head){
	env* temp = NULL;
	while(head != NULL){
		temp = head;
		head = head->next;
		freeEnvNode(temp);
	}
}
var* findVar(var* head,char* name){
	if(head == NULL)
	{
		return NULL;
	}
	while(head){
		if(!strcmp(head->name,name)){
			return head;
		}
		head = head->next;
	}
	return NULL;
}
bool findFunc(func* head,char* name){
	if(head == NULL)
	{
		return FALSE;
	}
	while(head){
		if(!strcmp(head->name,name)){
			return TRUE;
		}
		head = head->next;
	}
	return FALSE;
}



void exitWithError(){
	freeENVs(global);
	freeTree(root);
	getchar();
	exit(1);
}
int* getArgsTypeFromRoot(node* root,int* len){
	
	if(!root){
		printf("Logical Error in getArgsTypeFromRoot ");
		exit(1);
	}
	if(!strcmp(root->value,"(NO_ARGS)")){
		*len = 0;
		return NULL;
	}
	int size=0;
	node* temp = root;
	node* temp2 = NULL;
	while(temp != NULL){
		temp2 = temp->left->left;
		while(temp2){
			size++;
			temp2=temp2->right;
		}
		temp = temp->right;
	}
	int* argsTypeList = (int*)malloc(sizeof(int)*size);
	*len = size;
	size = 0;
	int type;
	while(root){
		type = getType(root->left->value);
		temp2 = root->left->left;
		while(temp2){
			argsTypeList[size]= type;
			size++;
			temp2=temp2->right;
		}
		root = root->right;
	}
	return argsTypeList;
}
env* addArgsToEnv(env* env,node* root,int numOfArgs){
	if(numOfArgs == 0){
		return env;
	}
	int type;
	var* varT;
	node * temp = NULL;
	while(root){
		type = getType(root->left->value);
		temp = root->left->left;
		while(temp){
			varT = mkVar(temp->value,type);
			env = addVarToEnv(env,varT);
			temp=temp->right;
		}
		root = root->right;
	}
	return env;
}
void findCheckMain(){
	if(global->returnType != GLOBAL){
		printf("\nLogical Error in findCheckMain\n ");
		exit(1);
	}
	func* funcs = global->funcs;
	if(strcmp(funcs->name,"main")!=0){
		printf("\nNo 'main' function found , last function is '%s'\n",funcs->name);
		func* temp =  global->funcs;
		
		exitWithError(root);
	}
	if(funcs->returnType != VOIDF){
		printf("\n'main' should not return any values\n");
		exitWithError(root);
	}
	if(funcs->numOfArgs != 0){
		printf("\n'main' should not receive any arguments\n");
		exitWithError(root);
	}

}
int evalExpType(node* root, env* enviroment){

	if(!strcmp(root->desc, "UN_OP") && !strcmp(root->left->desc, "UN_OP")) {
		printf("Unsupported operator chaining!\nCannot chain type %s and type %s", root->value, root->left->value);
		exitWithError();
	}

	if(!strcmp(root->desc, "BOOL")){
		root->var = newVarLabel();
		return BOOL;
	} 
	else if(!strcmp(root->desc, "CHAR")){
		root->var = newVarLabel();
		return CHAR;
	} 
	else if(!strcmp(root->desc, "INT")){
		root->var = newVarLabel();
		return INT;
	} 
	else if(!strcmp(root->desc, "REAL")){
		root->var = newVarLabel();
		return REAL;
	} 
	else if(!strcmp(root->desc, "STR")){
		root->var = newVarLabel();
		return STRING;
	} 
	else if(!strcmp(root->desc, "NULLP")){
		root->var = newVarLabel();
		return NULLPTR;
	} 

	else if(!strcmp(root->desc, "ID")){
		if(!root->var){
			root->var = strdup(root->value);
		}
		return getIDVarType(enviroment, root->value);
	}else if(!strcmp(root->desc, "ID_INDEX")){
		int value = getIDVarType(enviroment, root->value);
		int leftson = evalExpType(root -> left, enviroment);
		root->var = newVarLabel();
		if(leftson != INT){
			printf("Unsupported index value type %s for string type!\n", getTypeByNumber(leftson));
			exitWithError();
		}
		else if(value != STRING){
			printf("Unsupported index operator for type %s!\n", getTypeByNumber(value));
			exitWithError();
		}
		else{
			return CHAR;
		}
	}
	else if(!strcmp(root->desc, "UN_OP") && (!strcmp(root->value, "+") || !strcmp(root->value, "-"))) {
		int leftson = evalExpType(root -> left, enviroment);
		root->var = newVarLabel();
		if (leftson != REAL && leftson != INT){
			printf("Unsupported operator %s for type %s\n", root->value, getTypeByNumber(leftson));
			exitWithError();
		}
		else return leftson;
	}else if(!strcmp(root->desc, "UN_OP") && !strcmp(root->value, "!")){
		int leftson = evalExpType(root -> left, enviroment);
		root->var = newVarLabel();
		if (leftson != BOOL ){
			printf("Unsupported operator %s for type %s\n", root->value, getTypeByNumber(leftson));
			exitWithError();
		}
		else return BOOL;
	}else if(!strcmp(root->desc, "UN_OP")&& !strcmp(root->value, "*")){
		int leftson = evalExpType(root -> left, enviroment);
		root->var = newVarLabel();
		if ( leftson != INTP && leftson != CHARP && leftson != REALP ){
			printf("Attempted pointer assignment for unsupported type %s!\n",getTypeByNumber(leftson));
			exitWithError();
		}
		else{
			return getConcreteType(leftson);
		}
	}else if(!strcmp(root->desc, "ADDR")){
		int leftson = evalExpType(root -> left, enviroment);
		root->var = newVarLabel();
		if (leftson != CHAR && leftson != REAL && leftson != INT){
			printf("Unsupported operator %s for type %s\n", root->value, getTypeByNumber(leftson));
			exitWithError();
		}else{
			char* ptrType= getTypeByNumber(leftson);
			ptrType = mkcat(ptrType,"*");
			return getType(ptrType);
		}
	}else if(!strcmp(root->desc, "ADDR_INDEX")){
		int leftson = evalExpType(root -> left, enviroment);
		int leftgrandson = evalExpType(root->left->left, enviroment);
		root->var = newVarLabel();
		if(leftson != STRING){
			printf("Unsupported operator %s for non string type!\n", root->value);
			exitWithError();
		}
		else if(leftgrandson != INT){
			printf("Unsupported index value type %s for string type!\n", getTypeByNumber(leftgrandson));
			exitWithError();
		}
		else{
			return CHARP;
		}
	}
	else if(!strcmp(root->desc, "+") || !strcmp(root->desc, "-") ){
		int leftson = evalExpType(root ->left->left, enviroment);
		int rightson = evalExpType(root -> left->right, enviroment);
		root->var = newVarLabel();
		if( (isPointer(leftson) && rightson == INT)){
			if(isPointer(leftson)){
				return leftson;
			}else{
				return rightson;
			} 
		}else if ((leftson != REAL && leftson != INT) || (rightson != REAL && rightson != INT)  ){
			printf("Unsupported operator %s for types %s and %s\n", root->desc, getTypeByNumber(leftson),getTypeByNumber(rightson));
			exitWithError();
		}
		else{
			if(leftson == rightson)
				return leftson;
			else return REAL;
		}

	}else if(!strcmp(root->desc, "*")|| !strcmp(root->desc, "/")){
		int leftson = evalExpType(root -> left->left, enviroment);
		int rightson = evalExpType(root -> left->right, enviroment);
		root->var = newVarLabel();
		if ((leftson != REAL && leftson != INT) || (rightson != REAL && rightson != INT) ){
			printf("Unsupported operator %s for types %s and %s\n", root->desc, getTypeByNumber(leftson),getTypeByNumber(rightson));
			exitWithError();
		}
		else{
			if(leftson == rightson)
				return leftson;
			else return REAL;
		}
	}else if(!strcmp(root->desc, "&&") || !strcmp(root->desc, "||")){
		int leftson = evalExpType(root -> left->left, enviroment);
		int rightson = evalExpType(root -> left->right, enviroment);
		root->var = newVarLabel();
		if (leftson != BOOL || rightson != BOOL ){
			printf("Unsupported operator %s for types %s and %s\n", root->desc, getTypeByNumber(leftson),getTypeByNumber(rightson));
			exitWithError();
		}
		else return BOOL;
	}else if(!strcmp(root->desc, "<") || !strcmp(root->desc, ">")|| !strcmp(root->desc, "<=")|| !strcmp(root->desc, ">=")){
		int leftson = evalExpType(root -> left->left, enviroment);
		int rightson = evalExpType(root -> left->right, enviroment);
		root->var = newVarLabel();
		if ((leftson != REAL && leftson != INT) || (rightson != REAL && rightson != INT) ){
			printf("Unsupported operator %s for types %s and %s\n", root->desc, getTypeByNumber(leftson),getTypeByNumber(rightson));
			exitWithError();
		}
		else return BOOL;
	}else if(!strcmp(root->desc, "!=") || !strcmp(root->desc, "==")){
		int leftson = evalExpType(root -> left->left, enviroment);
		int rightson = evalExpType(root -> left->right, enviroment);
		root->var = newVarLabel();
		if (leftson != rightson || leftson == STRING){
			printf("Unsupported operator %s for types %s and %s\n", root->desc, getTypeByNumber(leftson),getTypeByNumber(rightson));
			exitWithError();
		}
		else return BOOL;
	}else if(!strcmp(root->desc, "IDLEN")){
		int leftson = evalExpType(root -> left, enviroment);
		root->var = newVarLabel();
		if ( leftson != STRING){
			printf("Unsupported operator %s for type %s\n", root->value, getTypeByNumber(leftson));
			exitWithError();
		}
		else return INT;
	}else if(!strcmp(root->value, "FUNC_CALL")){
		func* function = getFuncByID(enviroment,root->left->value);
		root->var = newVarLabel();
		if(function->returnType == VOID){
			printf("\n Assignment type mismatch: function '%s' has return type VOID", root->left->value);
			exitWithError();
		}
		return evaluateFuncCall(root->left->left, function,enviroment);
	}
	else{
		
		printf("\nLogical Error inside evalExpType!\n" );
		exitWithError();
	}
}

int getIDVarType(env* enviroment, char* name){
	if(enviroment->returnType == GLOBAL){
		printf("Undeclared ID usage!\nVariable %s does not exist!\n", name);
		exitWithError();
	}
	var* vars = enviroment->vars;
	while(vars){
		if(!strcmp(vars->name, name))
			return vars->type;
		vars = vars->next;
	}
	getIDVarType(enviroment->next,name);
}

func* getFuncByID(env* enviroment, char* name){
	if(!enviroment){
		printf("Undeclared ID usage!\nFunction %s does not exist!\n", name);
		exitWithError();
	}
	func* funcs = enviroment->funcs;
	while(funcs){
		if(!strcmp(funcs->name, name))
			return funcs;
		funcs = funcs->next;
	}
	getFuncByID(enviroment->next,name);
}

int getTypeForPrimDec(node *root){
	if(root->right){
		if(!strcmp(root->right->desc,"DEC_PRIM") ){
		return getType(root->right->value);
		}else if(!strcmp(root->right->desc,"DEC_ASS_PRIM")){
		return getType(root->right->left->value);
		}
	}
	if(root->left){
		if(!strcmp(root->left->desc,"DEC_PRIM")){
			return getType(root->left->value);
		}else if(!strcmp(root->left->desc,"DEC_ASS_PRIM")){
			return getType(root->left->left->value);
		}
	}
	
}


void checkPrimDeclarations(node* root,int type){
	if(!root){
		return;
	}
	if(!strcmp(root->desc,"DEC_PRIM")){
		 global = addVarToEnv(global,mkVar(root->left->value,type));
		 root->left->var = strdup(root->left->value);
	}
	else if(!strcmp(root->desc,"DEC_ASS_PRIM")){

		int expType = evalExpType(root->left->left->right,global);
		if(assignmentCheck(type,expType)){
			 global = addVarToEnv(global,mkVar(root->left->left->left->value,type));
			 root->left->left->left->var = strdup(root->left->left->left->value);
		}
		else{
			printf("Assignment type mismatch %s != %s in VAR:%s [%s]\n",getTypeByNumber(type),getTypeByNumber(expType),root->left->left->left->value,getTypeByNumber(type) );
			exitWithError();
		}
	}else if(!strcmp(root->desc,"ID")){
		global = addVarToEnv(global,mkVar(root->value,type));
		root->var = strdup(root->value);
		checkPrimDeclarations(root->right,type);
	}else if(!strcmp(root->desc,"DEC_ASS_PRIM_NODE")){
		int expType = evalExpType(root->left->left->right,global);
		if(assignmentCheck(type,expType)){
			global = addVarToEnv(global,mkVar(root->left->left->left->value,type));
			root->left->left->left->var = strdup(root->left->left->left->value );
		}
		else{
			printf("Assignment type mismatch %s != %s in VAR:%s [%s]\n",getTypeByNumber(type),getTypeByNumber(expType),root->left->left->left->value ,getTypeByNumber(type));
			exitWithError();
		}
		checkPrimDeclarations(root->right,type);
	}else if(!strcmp(root->desc,"DEC_ASS_PRIM_LEAF")){
		int expType = evalExpType(root->left->right,global);
		if(assignmentCheck(type,expType)){
			global = addVarToEnv(global,mkVar(root->left->left->value,type));
			root->left->left->var = strdup(root->left->left->value);
		}
		else{
			printf("\n Assignment type mismatch %s != %s in VAR : %s [%s] \n",getTypeByNumber(type),getTypeByNumber(expType),root->left->left->value,getTypeByNumber(type) );
			exitWithError();
		}
	}
}
int evaluateFuncCall(node* root,func* function, env* enviroment){
	int i = 0;
	node* exp = NULL;
	while(root){
		exp = root->left;
		if(i<function->numOfArgs){
			if(!assignmentCheck(function->argsType[i],evalExpType(exp,enviroment))){
				printf("\n Passing wrong type of arguments in function call '%s'\n",function->name  );
				exitWithError();
			}
		}else{
			printf("\n Passing wrong amount of arguments in function call '%s'\n",function->name  );
			exitWithError();
		}
			
		i++;
		root= root->right;
	}
	if(i==function->numOfArgs){
		return function->returnType;
	}else{
		printf("\n Passing wrong amount of arguments in function call '%s'\n",function->name  );
		exitWithError();
	}

}
void checkVarDecs(node* root){
	if(!root) return;

	if(!strcmp(root->desc,"VARDECS")){
		checkVarDecs(root->left);
		checkVarDecs(root->right);
	}
	else if(!strcmp(root->desc,"PRIM_DECS")){
		if(root->right->left){
			checkPrimDeclarations(root->right->left,getTypeForPrimDec(root->right));
		}
		checkPrimDeclarations(root->right->right,getTypeForPrimDec(root->right));
	} else if(!strcmp(root->desc,"STR_DECS")){
		checkStrDeclarations(root->right);
	}else if(!strcmp(root->desc,"STATEMENTS")){
		startSemanticalAnalysis(root->left);
		startSemanticalAnalysis(root->right);
	}else if( !strcmp(root->desc,"FUNCTIONS")){
		startSemanticalAnalysis(root);
	}
	else{
		printf("\nLogical Error inside checkVarDecs\n" );
		exitWithError();
	}	
}
bool assignmentCheck(int varType, int expType){
	if(varType==expType){
		return TRUE;
	}
	else if(varType==REAL && expType == INT){
		return TRUE;
	}else if(varType==REALP && expType == NULLPTR){
		return TRUE;
	}else if(varType==INTP && expType == NULLPTR){
		return TRUE;
	}else if(varType==CHARP && expType == NULLPTR){
		return TRUE;
	}else{
		return FALSE;
	}
}
void checkStrDeclarations(node * root){
	if(!root){
		return;
	}
	if(!strcmp(root->desc,"STR_1L")){
		 global = addVarToEnv(global,mkVar(root->left->value,STRING));
		 root->left->var = strdup(root->left->value);
	}else if(!strcmp(root->desc,"STR_2L")){
		 global = addVarToEnv(global,mkVar(root->left->left->value,STRING));
		 root->left->left->var = strdup(root->left->left->value);
	}else if(!strcmp(root->desc,"STR_3L")){
		global = addVarToEnv(global,mkVar(root->left->left->left->value,STRING));
		root->left->left->left->var = strdup(root->left->left->left->value);
	}else if(!strcmp(root->desc,"D-RIGHT")){
		checkStrDeclarations(root->left);
	}else if(!strcmp(root->value,")")){
		return;
	}
	else{
		printf("\nLogical Error inside checkStrDeclarations\n" );
		exitWithError();
	}	
	checkStrDeclarations(root->right);	

	
}
void startSemanticalAnalysis(node* root){
	if(root==NULL){
		return;
	}
	if(!strcmp(root->value,"CODE")){
		global = mkEnv(GLOBAL);
		pointerToGlobalOnly = global;
		startSemanticalAnalysis(root->left);
		if(numOfmains>1){
			printf("\nThere should be exact one main\n");
			exitWithError(root);
		}else{
			findCheckMain();
		}
	}
	else if(!strcmp(root->desc,"FUNCTIONS")){
		startSemanticalAnalysis(root->left);
		global = popEnv(global);
		startSemanticalAnalysis(root->right);
	}
	else if(!strcmp(root->value,"FUNCTION") || !strcmp(root->value,"PROCEDURE")){
		int numOfArgs;
		int* argsType = NULL;
		argsType = getArgsTypeFromRoot(root->left->left->left,&numOfArgs);
		int type = getType(root->left->left->right->right->value);
		func * newF =mkFunc(root->left->value,argsType,type,numOfArgs);
		asprintf(&root->label,"%s:\n",root->left->value);
		global = addFuncToEnv(global,newF);
		env* newEnv = mkEnv(type);
		global = pushEnv(global,newEnv);
		global = addArgsToEnv(global,root->left->left->left,numOfArgs);
		startSemanticalAnalysis(root->left->left->right->right->right);
		if(!strcmp(root->value,"FUNCTION") && strcmp(root->left->value,"main")){
			hasReturn(root->left->left->right->right->right->left);
		}
	}else if(!strcmp(root->value,"BODY")){
		startSemanticalAnalysis(root->left);
		startSemanticalAnalysis(root->right);
	}
	else if(!strcmp(root->desc,"VARDECS")){
		checkVarDecs(root);
	} else if(!strcmp(root->desc,"STATEMENTS")){
		startSemanticalAnalysis(root->left);
		startSemanticalAnalysis(root->right);
	}else if(!strcmp(root->desc,"ENV_BLOCK")){
		global = pushEnv(global,mkEnv(BLOCK));
		startSemanticalAnalysis(root->left);
		global = popEnv(global);
	}else if(!strcmp(root->desc,"COND_STAT")){
		checkConditionStatement(root->right);
	}
	return;
}
void checkConditionStatement(node* root){
	if(!strcmp(root->desc,"FUNC_CALL")){
		func* function = getFuncByID(global,root->left->value);
		evaluateFuncCall(root->left->left,function, global);
	} else if(!strcmp(root->desc,"CONDS")){
		checkConds(root->left);
	}else if(!strcmp(root->desc,"LOOPS")){
		checkLoops(root->left);
	}else if(!strcmp(root->desc,"RET")){
		if(global->returnType==BLOCK){
			printf("\nReturn statement should not appear outside of function block\n" );
			exitWithError();
		}else if(global->returnType==VOIDF){
			printf("\nProcedure should not return any value!\n" );
			exitWithError();
		}
		int type = evalExpType(root->left->left,global);
		if(type!= global->returnType){
			printf("\nFunction should return type: %s instead type:%s \n",getTypeByNumber(global->returnType),getTypeByNumber(type));
			exitWithError();
		}
	}else if(!strcmp(root->desc,"RIGHT")){
		checkAssigment(root->left);
	}else{
		printf("\nLogical Error inside checkConditionStatement\n" );
		exitWithError();
	}

}
var* findVarForAssigment (env* enviroment,char* name){
	var* variable = NULL;
	while(enviroment->returnType != GLOBAL){
		variable = findVar(enviroment->vars,name);
		if(variable){
			return variable;
		}else{
			enviroment = enviroment->next;
		}
	}
	printf("\nVariable '%s' is undeclared \n",name );
	exitWithError();
}
void checkAssigment(node *root){
	if(!root){
		return;
	}
	if(!strcmp(root->desc,"PRIMASS")){
		var* var_ass = findVarForAssigment(global,root->right->left->left->value);
		int expType = evalExpType(root->right->left->right,global);
		if(!assignmentCheck(var_ass->type,expType)){
			printf("\n Assignment type mismatch %s != %s in VAR : %s [%s] \n",getTypeByNumber(var_ass->type),getTypeByNumber(expType),var_ass->name,getTypeByNumber(var_ass->type));
			exitWithError();
		}
		root->right->left->left->var = strdup(root->right->left->left->value);
	}
	else if(!strcmp(root->desc,"INDEXASS")){
		var* var_ass = findVarForAssigment(global,root->right->left->left->value);
		int indexExpType = evalExpType(root->right->left->left->left,global);
		if(indexExpType != INT){
			printf("\n Index expression type is %s,but should be INT (in VAR : %s)\n",getTypeByNumber(indexExpType),var_ass->name);
			exitWithError();
		}
		int assExpType = evalExpType(root->right->left->right,global);
		if(!assignmentCheck(CHAR,assExpType)){
			printf("\n Assignment type mismatch for STRING[INDEX], expected CHAR but received %s\n",getTypeByNumber(assExpType));
			exitWithError();
		}
		root->right->left->left->var = strdup(root->right->left->left->value);
	}
	else if(!strcmp(root->desc,"PTRASS")){
		var* var_ass = findVarForAssigment(global,root->right->left->left->left->value);
		int conreteType = getConcreteType(var_ass->type);
		int expType = evalExpType(root->right->left->right,global);
		if(!assignmentCheck(conreteType,expType)){
			printf("\n Assignment type mismatch for *ID, expected %s but received %s\n",getTypeByNumber(conreteType),getTypeByNumber(expType));
			exitWithError();
		}
		root->right->left->left->left->var = strdup(root->right->left->left->left->value);
	}
	else{
		printf("\nLogical Error inside checkAssigment\n" );
		exitWithError();
	}
}
void hasReturn(node* root){
	if(root){
		while(root->right){
			root = root->right;
		}
		if( root->left ){
			if(strcmp(root->left->value,"BLOCK")){
				if(root->left->right){
					if(!strcmp(root->left->right->desc,"RET")){
						return;
					}
				}
			}
		}
	}
	printf("\nFunction missing final return statement!\n" );
	exitWithError();
}
void checkLoops(node* root){
	if(!root){
		return;
	}else if(!strcmp(root->value,"DO")){
		checkLoops(root->left);
		int type = evalExpType(root->right->left,global);
		if(type != BOOL){
			printf("Do-while condition must be of type BOOL! received %s\n", getTypeByNumber(type));
			exitWithError();
		}
		global = popEnv(global);
	}else if(!strcmp(root->value,"WHILE")){
		int type = evalExpType(root->left,global);
		if(type != BOOL){
			printf("While condition must be of type BOOL! received %s\n", getTypeByNumber(type));
			exitWithError();
		}
		checkLoops(root->right->left);
		if(!strcmp(root->right->left->desc,"INNER_ENV")){
			global = popEnv(global);
		}
	}else if(!strcmp(root->value,"FOR")){
		var* initVar = findVarForAssigment(global,root->left->left->left->left->value);
		if(!initVar){
			printf("Undeclared variable received! %s\n", root->left->left->left->left->value);
			exitWithError();
		}else if(initVar->type != INT){
			printf("For loop init variable must be an integer! received %s\n", getTypeByNumber(initVar->type));
			exitWithError();
		}
		int initExp = evalExpType(root->left->left->left->right,global);
		if(initExp != INT){
			printf("For loop init variable assignment mismatch! expected integer but received %s\n", getTypeByNumber(initExp));
			exitWithError();
		}
		int condExp = evalExpType(root->left->right->left,global);
		if(condExp != BOOL){
			printf("Illegal for loop condition expression! expected boolean but received %s\n", getTypeByNumber(condExp));
			exitWithError();
		}
		var* updVar = findVarForAssigment(global,root->left->right->right->left->left->left->value);
		if(!updVar){
			printf("Undeclared variable received! %s\n", root->left->right->right->left->left->left->value);
			exitWithError();
		}else if(updVar->type != INT){
			printf("For loop update variable must be an integer! received %s\n", getTypeByNumber(updVar->type));
			exitWithError();
		}
		int updExp = evalExpType(root->left->right->right->left->left->right,global);
		if(updExp != INT){
			printf("For loop update variable assignment mismatch! expected integer but received %s\n", getTypeByNumber(updExp));
			exitWithError();
		}
		checkLoops(root->left->right->right->right);
		if(!strcmp(root->left->right->right->right->desc,"INNER_ENV")){
			global = popEnv(global);
		}
	}else if(!strcmp(root->desc,"INNER_ENV")){
		env* tmp = mkEnv(global->returnType);
		global = pushEnv(global,tmp);

		checkVarDecs(root->right);
	}else if(!strcmp(root->desc,"COND_STAT")){
		checkConditionStatement(root->right);
	}else{
		printf("\nLogical Error inside checkLoops\n" );
		exitWithError();
	}
}
void checkConds(node* root){
	if(!root){
		return;
	}else if(!strcmp(root->desc,"IF")){
		int condExp = evalExpType(root->left,global);
		if(condExp != BOOL){
			printf("Illegal if condition expression! expected boolean but received %s\n", getTypeByNumber(condExp));
			exitWithError();
		}
		checkConds(root->right->left);
		if(!strcmp(root->right->left->desc,"INNER_ENV")){
			global = popEnv(global);
		}
	}else if(!strcmp(root->desc,"IFELSE")){
		checkConds(root->left);
		checkConds(root->right);
	}else if(!strcmp(root->desc,"ELSE")){
		checkConds(root->left->left);
		if(!strcmp(root->left->left->desc,"INNER_ENV")){
			global = popEnv(global);
		}
	}else if(!strcmp(root->desc,"INNER_ENV")){
		env* tmp = mkEnv(global->returnType);
		global = pushEnv(global,tmp);
		checkVarDecs(root->right);
	}
	else if(!strcmp(root->desc,"COND_STAT")){
		checkConditionStatement(root->right);
	}else{
		printf("\nLogical Error inside checkConds\n" );
		exitWithError();
	}
}
bool isPointer(int type){
	if(type != CHARP && type != INTP && type != REALP){
		return FALSE;
	}else{
		return TRUE;
	}
}


char* newVarLabel(){
	char* t;
	asprintf(&t,"t%d",varLabel++);
	return t;
}
char* newCodeLabel(){
	char* l;
	asprintf(&l,"L%d:",codeLabel++);
	return l;
}
void print (node* root){
	if(!root)
		return;
	print(root->left);
	print(root->right);
	if(root->var != NULL)
		printf("3AC:%s \t C: value-%s | desc:%s\n",root->var,root->value,root->desc);
}
/*PART 3 */ 
void To3AC(node* root){
	if(root==NULL){
		return;
	}
	if(!strcmp(root->value,"CODE")){
		To3AC(root->left);
		printf("%s",root->left->code);
		//printToTxt(root->left->code);
	}else if(!strcmp(root->value,"FUNCFROMBODY")){
		//Add recursive call to root that is not FUNCTIONS 
		return;
	}
	else if(!strcmp(root->desc,"FUNCTIONS")){
		To3AC(root->left);
		To3AC(root->right);
		if(root->left && root->right){
			asprintf(&root->code,"%s%s",root->left->code,root->right->code);
		}
		else if(root->left){
			root->code = strdup(root->left->code);
		}
	}
	else if(!strcmp(root->value,"FUNCTION") || !strcmp(root->value,"PROCEDURE")){
		To3AC(root->left->left->right->right->right);
		asprintf(&root->code,"%s\t\tBeginFunc\n%s\t\tEndFunc\n",root->label,root->left->left->right->right->right->code);
	}else if(!strcmp(root->value,"BODY")){
		To3AC(root->left);
		To3AC(root->right);
		if(root->left && root->right){
			asprintf(&root->code,"%s%s",root->left->code,root->right->code);
		}
		else if(root->left){
			root->code = strdup(root->left->code);
		}
	}
	else if(!strcmp(root->desc,"VARDECS")){
		varDecsTo3AC(root);
	} else if(!strcmp(root->desc,"STATEMENTS")){
		To3AC(root->left);
		To3AC(root->right);
		if(root->left && root->right){
			asprintf(&root->code,"%s%s",root->left->code,root->right->code);
		}
		else if(root->left){
			root->code = strdup(root->left->code);
		}
	}else if(!strcmp(root->desc,"COND_STAT")){
		conditionStatementTo3AC(root->right);
		root->code = strdup(root->right->code);

	}
	return;
}

void condsTo3AC(node* root){
	return;
	/*if(!root){
		return;
	}else if(!strcmp(root->desc,"IF")){
		int condExp = evalExpType(root->left,global);
		if(condExp != BOOL){
			printf("Illegal if condition expression! expected boolean but received %s\n", getTypeByNumber(condExp));
			exitWithError();
		}
		condsTo3AC(root->right->left);
		if(!strcmp(root->right->left->desc,"INNER_ENV")){
			global = popEnv(global);
		}
	}else if(!strcmp(root->desc,"IFELSE")){
		condsTo3AC(root->left);
		condsTo3AC(root->right);
	}else if(!strcmp(root->desc,"ELSE")){
		condsTo3AC(root->left->left);
		if(!strcmp(root->left->left->desc,"INNER_ENV")){
			global = popEnv(global);
		}
	}else if(!strcmp(root->desc,"INNER_ENV")){
		env* tmp = mkEnv(global->returnType);
		global = pushEnv(global,tmp);
		condsTo3AC(root->right);
	}
	else if(!strcmp(root->desc,"COND_STAT")){
		conditionStatementTo3AC(root->right);
	}else{
		printf("\nLogical Error inside checkConds\n" );
		exitWithError();
	}*/
}
void loopsTo3AC(node* root){
	/*if(!root){
		return;
	}else if(!strcmp(root->value,"DO")){
		loopsTo3AC(root->left);
		int type = evalExpType(root->right->left,global);
		if(type != BOOL){
			printf("Do-while condition must be of type BOOL! received %s\n", getTypeByNumber(type));
			exitWithError();
		}
		global = popEnv(global);
	}else if(!strcmp(root->value,"WHILE")){
		int type = evalExpType(root->left,global);
		if(type != BOOL){
			printf("While condition must be of type BOOL! received %s\n", getTypeByNumber(type));
			exitWithError();
		}
		loopsTo3AC(root->right->left);
		if(!strcmp(root->right->left->desc,"INNER_ENV")){
			global = popEnv(global);
		}
	}else if(!strcmp(root->value,"FOR")){
		var* initVar = findVarForAssigment(global,root->left->left->left->left->value);
		if(!initVar){
			printf("Undeclared variable received! %s\n", root->left->left->left->left->value);
			exitWithError();
		}else if(initVar->type != INT){
			printf("For loop init variable must be an integer! received %s\n", getTypeByNumber(initVar->type));
			exitWithError();
		}
		int initExp = evalExpType(root->left->left->left->right,global);
		if(initExp != INT){
			printf("For loop init variable assignment mismatch! expected integer but received %s\n", getTypeByNumber(initExp));
			exitWithError();
		}
		int condExp = evalExpType(root->left->right->left,global);
		if(condExp != BOOL){
			printf("Illegal for loop condition expression! expected boolean but received %s\n", getTypeByNumber(condExp));
			exitWithError();
		}
		var* updVar = findVarForAssigment(global,root->left->right->right->left->left->left->value);
		if(!updVar){
			printf("Undeclared variable received! %s\n", root->left->right->right->left->left->left->value);
			exitWithError();
		}else if(updVar->type != INT){
			printf("For loop update variable must be an integer! received %s\n", getTypeByNumber(updVar->type));
			exitWithError();
		}
		int updExp = evalExpType(root->left->right->right->left->left->right,global);
		if(updExp != INT){
			printf("For loop update variable assignment mismatch! expected integer but received %s\n", getTypeByNumber(updExp));
			exitWithError();
		}
		loopsTo3AC(root->left->right->right->right);
		if(!strcmp(root->left->right->right->right->desc,"INNER_ENV")){
			global = popEnv(global);
		}
	}else if(!strcmp(root->desc,"INNER_ENV")){
		env* tmp = mkEnv(global->returnType);
		global = pushEnv(global,tmp);

		checkVarDecs(root->right);
	}else if(!strcmp(root->desc,"COND_STAT")){
		checkConditionStatement(root->right);
	}else{
		printf("\nLogical Error inside checkLoops\n" );
		exitWithError();
	}*/
}

void assigmentTo3AC(node *root){
	/*if(!root){
		return;
	}
	if(!strcmp(root->desc,"PRIMASS")){
		var* var_ass = findVarForAssigment(global,root->right->left->left->value);
		int expType = evalExpType(root->right->left->right,global);
		if(!assignmentCheck(var_ass->type,expType)){
			printf("\n Assignment type mismatch %s != %s in VAR : %s [%s] \n",getTypeByNumber(var_ass->type),getTypeByNumber(expType),var_ass->name,getTypeByNumber(var_ass->type));
			exitWithError();
		}
		root->right->left->left->var = strdup(root->right->left->left->value);
	}
	else if(!strcmp(root->desc,"INDEXASS")){
		var* var_ass = findVarForAssigment(global,root->right->left->left->value);
		int indexExpType = evalExpType(root->right->left->left->left,global);
		if(indexExpType != INT){
			printf("\n Index expression type is %s,but should be INT (in VAR : %s)\n",getTypeByNumber(indexExpType),var_ass->name);
			exitWithError();
		}
		int assExpType = evalExpType(root->right->left->right,global);
		if(!assignmentCheck(CHAR,assExpType)){
			printf("\n Assignment type mismatch for STRING[INDEX], expected CHAR but received %s\n",getTypeByNumber(assExpType));
			exitWithError();
		}
		root->right->left->left->var = strdup(root->right->left->left->value);
	}
	else if(!strcmp(root->desc,"PTRASS")){
		var* var_ass = findVarForAssigment(global,root->right->left->left->left->value);
		int conreteType = getConcreteType(var_ass->type);
		int expType = evalExpType(root->right->left->right,global);
		if(!assignmentCheck(conreteType,expType)){
			printf("\n Assignment type mismatch for *ID, expected %s but received %s\n",getTypeByNumber(conreteType),getTypeByNumber(expType));
			exitWithError();
		}
		root->right->left->left->left->var = strdup(root->right->left->left->left->value);
	}
	else{
		printf("\nLogical Error inside checkAssigment\n" );
		exitWithError();
	}*/
}
void conditionStatementTo3AC(node* root){
	if(!strcmp(root->desc,"FUNC_CALL")){
		func* function = findFuncInGlobal(root->left->value);
		FuncCallTo3AC(root->left->left,function);
		if(root->left->left){
			root->code = strdup(root->left->left->code);
		}else{
			asprintf(&root->code,"\t\tLCall %s\n",function->name);
		}	
	} /*else if(!strcmp(root->desc,"CONDS")){
		condsTo3AC(root->left);
		root->code = strdup(root->left->code);
	}else if(!strcmp(root->desc,"LOOPS")){
		loopsTo3AC(root->left);
		root->code = strdup(root->left->code);
	}else if(!strcmp(root->desc,"RET")){
		expTo3AC(root->left->left);
		root->code = strdup(root->left->left->code);
	}else if(!strcmp(root->desc,"RIGHT")){
		assigmentTo3AC(root->left);
		root->code = strdup(root->left->code);
	}*/
	
	return;

}
void strDeclarationsTo3AC(node * root){
	//TODO ASSIGNMENT: add string assignment that we did not check in semantics
	/*if(!root){
		return;
	}
	if(!strcmp(root->desc,"STR_1L")){
		 global = addVarToEnv(global,mkVar(root->left->value,STRING));
	}else if(!strcmp(root->desc,"STR_2L")){
		 global = addVarToEnv(global,mkVar(root->left->left->value,STRING));
	}else if(!strcmp(root->desc,"STR_3L")){
		global = addVarToEnv(global,mkVar(root->left->left->left->value,STRING));
	}else if(!strcmp(root->desc,"D-RIGHT")){
		checkStrDeclarations(root->left);
		root->code = strdup(root->left->code);
	}else if(!strcmp(root->value,")")){
		return;
	}
	else{
		return;
	}	
	checkStrDeclarations(root->right);	
	root->code = strdup(root->right->code);*/
	
}
void varDecsTo3AC(node* root){
	if(!root) return;

	if(!strcmp(root->desc,"VARDECS")){
		varDecsTo3AC(root->left);
		varDecsTo3AC(root->right);
		if(root->left && root->right){
			asprintf(&root->code,"%s%s",root->left->code,root->right->code);
		}
		else if(root->left){
			root->code = strdup(root->left->code);
		}
	}
	else if(!strcmp(root->desc,"PRIM_DECS")){
		if(root->right->left){
			primDeclarationsTo3AC(root->right->left);
		}
		primDeclarationsTo3AC(root->right->right);
		if(root->right->left){
			asprintf(&root->code,"%s%s",root->right->left->code,root->right->right->code);
		}else{
			root->code = strdup(root->right->right->code);
		}
	} else if(!strcmp(root->desc,"STR_DECS")){
		strDeclarationsTo3AC(root->right);
		if(root->right->code){
			root->code = strdup(root->right->code);
		}
		
	}else if(!strcmp(root->desc,"STATEMENTS")){
		To3AC(root->left);
		To3AC(root->right);
		if(root->left && root->right){
			asprintf(&root->code,"%s%s",root->left->code,root->right->code);
		}
		else if(root->left){
			root->code = strdup(root->left->code);
		}
	}else if( !strcmp(root->desc,"FUNCTIONS")){
		To3AC(root);
	}
	else{
		printf("\nLogical Error inside checkVarDecs\n" );
		exitWithError();
	}	
}

void FuncCallTo3AC(node* root,func* function){
	if(!root){
		return;
	}
	printf("/nHERE1/n");
	node* exp = NULL;
	root->code = strdup(root->right->code);
	char* code = NULL;
	char* temp = NULL;
	while(root){
		exp = root->left;
		expTo3AC(exp);
		temp = code;
		if(code){
			free(code);
			code = NULL;
		}
		if(temp){
			asprintf(&code,"%s%s\t\tPushParam %s\n",temp,exp->code,exp->var);
		}
		else{
			asprintf(&code,"%s\t\tPushParam %s\n",exp->code,exp->var);
		}
		free(temp);
		temp = NULL;
		root= root->right;
	}
	printf("/nHERE/n");
	asprintf(&root->code,"%s\t\tLCall %s\n\t\tPopParams %d\n",code,function->name,calcBytesForFunc(function));
}

void primDeclarationsTo3AC(node* root){
	if(!root){
		return;
	}
	if(!strcmp(root->desc,"DEC_ASS_PRIM")){
		expTo3AC(root->left->left->right);
		asprintf(&root->code,"%s\t\t%s = %s\n",
		root->left->left->right->code,root->left->left->left->var,root->left->left->right->var);
	}else if(!strcmp(root->desc,"ID")){
		primDeclarationsTo3AC(root->right);
		root->code = strdup(root->right->code);
	}else if(!strcmp(root->desc,"DEC_ASS_PRIM_NODE")){
		expTo3AC(root->left->left->right);
		primDeclarationsTo3AC(root->right);
		asprintf(&root->code,"%s\t\t%s = %s\n%s",
		root->left->left->right->code,
		root->left->left->left->var,
		root->left->left->right->var,
		root->right->code);
	}else if(!strcmp(root->desc,"DEC_ASS_PRIM_LEAF")){
		expTo3AC(root->left->right);
		asprintf(&root->code,"%s\t\t%s = %s\n",
		root->left->right->code,
		root->left->left->var,
		root->left->right->var);
	}
	return;
}

void expTo3AC(node* root){
	if(!strcmp(root->desc, "BOOL")
		|| !strcmp(root->desc, "CHAR")
		|| !strcmp(root->desc, "INT")
	    || !strcmp(root->desc, "REAL")
	){
		asprintf(&root->code,"\t\t%s = %s\n",root->var,root->value);
	}
	else if(!strcmp(root->desc, "STR")){
		asprintf(&root->code,"\t\t%s = \"%s\"\n",root->var,root->value);
	}
	else if(!strcmp(root->desc, "NULLP")){
		asprintf(&root->code,"\t\t%s = 0\n",root->var);
	} 
	else if(!strcmp(root->desc, "ID")){
		root->code = strdup("");
	}else if(!strcmp(root->desc, "ID_INDEX")){
		expTo3AC(root->left);
		asprintf(&root->code,"%s\t\t%s = *%s + %s\n",root->left->code,root->var,root->value,root->left->var);
	}
	else if(!strcmp(root->desc, "UN_OP") && (!strcmp(root->value, "+") 
		|| !strcmp(root->value, "-")
		|| strcmp(root->value, "*")
		|| strcmp(root->value, "!"))) {
		expTo3AC(root->left);
		asprintf(&root->code,"%s\t\t%s = %s%s\n",root->left->code,root->var,root->value,root->left->var);
	}else if(!strcmp(root->desc, "ADDR")){
		expTo3AC(root->left);
		asprintf(&root->code,"%s\t\t%s = &%s\n",root->left->code,root->var,root->left->var);
	}else if(!strcmp(root->desc, "ADDR_INDEX")){
		expTo3AC(root->left->left);
		asprintf(&root->code,"%s\t\t%s = &%s + %s\n",root->left->left->code,root->var,root->left->value,root->left->left->var);
	}
	else if(!strcmp(root->desc, "+") || !strcmp(root->desc, "-") || !strcmp(root->desc, "*")|| !strcmp(root->desc, "/") ){
		expTo3AC(root->left->left);
		expTo3AC(root->left->right);
		asprintf(&root->code,"%s%s\t\t%s = %s %s %s\n",
		root->left->left->code,
		root->left->right->code,
		root->var,
		root->left->left->var,
		root->desc,
		root->left->right->var);
	}else if(!strcmp(root->desc, "&&") || !strcmp(root->desc, "||") || !strcmp(root->desc, "!=") || !strcmp(root->desc, "==")){
		/*TODO:
		int leftson = evalExpType(root -> left->left, enviroment);
		int rightson = evalExpType(root -> left->right, enviroment);
		root->var = newVarLabel();
		if (leftson != BOOL || rightson != BOOL ){
			printf("Unsupported operator %s for types %s and %s\n", root->desc, getTypeByNumber(leftson),getTypeByNumber(rightson));
			exitWithError();
		}
		else return BOOL;*/

	}else if(!strcmp(root->desc, "<") || !strcmp(root->desc, ">")|| !strcmp(root->desc, "<=")|| !strcmp(root->desc, ">=")){
		expTo3AC(root->left->left);
		expTo3AC(root->left->right);
		asprintf(&root->code,"%s%s\t\t%s = %s %s %s\n",
		root->left->left->code,
		root->left->right->code,
		root->var,
		root->left->left->var,
		root->desc,
		root->left->right->var);
	}else if(!strcmp(root->desc, "IDLEN")){
		asprintf(&root->code,"\t\t%s = |%s|\n",
		root->var,
		root->left->left->var);
	}else if(!strcmp(root->value, "FUNC_CALL")){
		//TODO:
		/*func* function = getFuncByID(enviroment,root->left->value);
		evaluateFuncCall(root->left->left, function,enviroment);*/
	}
	return;
}
func* findFuncInGlobal(char* name){
	func* funcs = pointerToGlobalOnly->funcs;
	while(funcs){
		if(!strcmp(funcs->name,name)){
			return funcs;
		}
		funcs = funcs->next;
	}
	printf("\nLogical Error inside findFuncInGlobal!\n" );
	exitWithError();
}

int calcBytesForFunc(func* f){
	int counter = 0;
	for(int i = 0 ; i<f->numOfArgs; i++)
		counter += getBytesByType(f->argsType[i]);
	return counter;
}
int getBytesByType(int type){
	if(type == BOOL){
		return 4;
	}else if(type == REAL){
		return 8;
	}else if(type == CHAR){
		return 1;
	}else if(type == INTP){
		return 8;
	}else if(type == CHARP){
		return 8;
	}else if(type == REALP){
		return 8;
	}
	return 0;

}