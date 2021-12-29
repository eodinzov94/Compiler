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
 char *var2; //FOR INDEX ASSIGMENT
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
void writeToTxtFile(char* code);
void print (node* root);
void To3AC(node* root);
void condsTo3AC(node* root);
void loopsTo3AC(node* root);
bool isRelativeOp(char * value);
void assigmentTo3AC(node *root);
bool isSimpleCond(node* root);
void conditionStatementTo3AC(node* root);
void strDeclarationsTo3AC(node * root);
void varDecsTo3AC(node* root);
void FuncCallTo3AC(node* root,func* function,bool isAssigemnt,char* varLabel);
void primDeclarationsTo3AC(node* root);
void expTo3AC(node* root);
bool isOperator(char* value);
void shortCircuitEvalTo3AC(node* root);
void getCodeLabelsForShortCircuit(node* root);
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
																freeTree($1);
																freeENVs(global);
																getchar();
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
															{$$=mknode("FOR","",mknode("INIT","",mknode("","PRIMASS",NULL,$3),mknode("CONDITION","",$5,mknode("UPDATE","",mknode("","PRIMASS",NULL,$7),mknode("","COND_STAT",NULL,$9)))),mkleaf(")",""));}
| FOR '(' primitive_assignment ';' expression ';' primitive_assignment ')' '{' decs_with_stats '}'
															{$$=mknode("FOR","",mknode("INIT","",mknode("","PRIMASS",NULL,$3),mknode("CONDITION","",$5,mknode("UPDATE","",mknode("","PRIMASS",NULL,$7),mknode("","INNER_ENV",NULL,$10)))),mkleaf(")",""));}
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
 newnode->var2  = NULL;
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
	freeTree(tree->left);
	freeTree(tree->right);
    free(tree->desc);
	free(tree->value);
	if(tree->truelabel){
		free(tree->truelabel);
	}
	if(tree->falselabel){
		free(tree->falselabel);
	}
	if(tree->code){
		free(tree->code);
	}
	if(tree->var){
		free(tree->var);
	}
	if(tree->var2){
		free(tree->var2);
	}
	if(tree->label){
		free(tree->label);
	}
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
		if(!strcmp(root->value,"true")){
			root->var = newVarLabel();
			asprintf(&root->code,"\t\t%s = 1\n", root->var);
		}
		else{
			root->var = newVarLabel();
			asprintf(&root->code,"\t\t%s = 0\n", root->var);
		}	
		return BOOL;
	} 
	else if(!strcmp(root->desc, "CHAR")){
		root->var = newVarLabel();
		asprintf(&root->code,"\t\t%s = %s\n", root->var,root->value);
		return CHAR;
	} 
	else if(!strcmp(root->desc, "INT")){
		root->var = newVarLabel();
		asprintf(&root->code,"\t\t%s = %s\n", root->var,root->value);
		return INT;
	} 
	else if(!strcmp(root->desc, "REAL")){
		root->var = newVarLabel();
		asprintf(&root->code,"\t\t%s = %s\n", root->var,root->value);
		return REAL;
	} 
	else if(!strcmp(root->desc, "STR")){
		root->var = newVarLabel();
		asprintf(&root->code,"\t\t%s = %s\n", root->var,root->value);
		return STRING;
	} 
	else if(!strcmp(root->desc, "NULLP")){
		root->var = newVarLabel();
		asprintf(&root->code,"\t\t%s = 0\n", root->var);
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
		root->var2 = newVarLabel();
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
		if(isSimpleCond(root)){
			root->var = newVarLabel();
		}
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
		root->var = strdup(root->right->left->left->value);
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
		root->value = strdup(root->right->left->left->value);
		root->var = newVarLabel();
		root->var2 = newVarLabel();
	}
	else if(!strcmp(root->desc,"PTRASS")){
		var* var_ass = findVarForAssigment(global,root->right->left->left->left->value);
		int conreteType = getConcreteType(var_ass->type);
		int expType = evalExpType(root->right->left->right,global);
		if(!assignmentCheck(conreteType,expType)){
			printf("\n Assignment type mismatch for *ID, expected %s but received %s\n",getTypeByNumber(conreteType),getTypeByNumber(expType));
			exitWithError();
		}
		root->var = strdup(root->right->left->left->left->value);
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
		var* initVar = findVarForAssigment(global,root->left->left->right->left->left->value);
		root->left->left->var= strdup(root->left->left->right->left->left->value);
		if(!initVar){
			printf("Undeclared variable received! %s\n", root->left->left->right->left->left->value);
			exitWithError();
		}else if(initVar->type != INT){
			printf("For loop init variable must be an integer! received %s\n", getTypeByNumber(initVar->type));
			exitWithError();
		}
		int initExp = evalExpType(root->left->left->right->left->right,global);
		if(initExp != INT){
			printf("For loop init variable assignment mismatch! expected integer but received %s\n", getTypeByNumber(initExp));
			exitWithError();
		}
		int condExp = evalExpType(root->left->right->left,global);
		if(condExp != BOOL){
			printf("Illegal for loop condition expression! expected boolean but received %s\n", getTypeByNumber(condExp));
			exitWithError();
		}
		var* updVar = findVarForAssigment(global,root->left->right->right->left->right->left->left->value);
		root->left->right->right->left->var = strdup(root->left->right->right->left->right->left->left->value);
		if(!updVar){
			printf("Undeclared variable received! %s\n", root->left->right->right->left->right->left->left->value);
			exitWithError();
		}else if(updVar->type != INT){
			printf("For loop update variable must be an integer! received %s\n", getTypeByNumber(updVar->type));
			exitWithError();
		}
		int updExp = evalExpType(root->left->right->right->left->right->left->right,global);
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

/*PART 3 */ 
char* newVarLabel(){
	char* t;
	asprintf(&t,"t%d",varLabel++);
	return t;
}
char* newCodeLabel(){
	char* l;
	asprintf(&l,"L%d",codeLabel++);
	return l;
}
void print (node* root){
	if(!root)
		return;
	printf("desc:%s \n val:%s\n",root->desc,root->value);
	print(root->left);
	print(root->right);
	
}

void To3AC(node* root){
	if(root==NULL){
		return;
	}
	if(!strcmp(root->value,"CODE")){
		To3AC(root->left);
		printf("CODE OK, CHECK output.txt file for 3AC :)\n ");
		writeToTxtFile(root->left->code);
	}else if(!strcmp(root->desc,"FUNCTIONS")){
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
	}else if(!strcmp(root->desc,"ENV_BLOCK")){
		To3AC(root->left);
		root->code = strdup(root->left->code);
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
	if(!root){
		return;		
	}else if(!strcmp(root->desc,"IF")){
		if (isSimpleCond(root->left)){
			root->truelabel = newCodeLabel();
			condsTo3AC(root->right->left);
			root->falselabel = newCodeLabel();
			expTo3AC(root->left);
			asprintf(&root->code,"%s\t\tifz %s goto %s\n%s:%s\t\tgoto %s\n%s:",
				root->left->code,
				root->left->var,
				root->falselabel,
				root->truelabel,
				root->right->left->code,
				root->falselabel,
				root->falselabel
			);
		}else{
			getCodeLabelsForShortCircuit(root->left);
			shortCircuitEvalTo3AC(root->left);
			condsTo3AC(root->right->left);
			asprintf(&root->code,"%s%s:%s%s:",
				root->left->code,
				root->left->truelabel,
				root->right->left->code,
				root->left->falselabel
			);
			
		}
	}else if(!strcmp(root->desc,"IFELSE")){
		node* condExp = root->left->left; 
		node* bodyIf = root->left->right->left;
		node* bodyElse = root->right;
		if (isSimpleCond(condExp)){	
			root->truelabel = newCodeLabel();
			condsTo3AC(bodyIf);
			root->falselabel = newCodeLabel();
			condsTo3AC(bodyElse);
			root->label = newCodeLabel();
			expTo3AC(condExp);
			asprintf(&root->code,"%s\t\tifz %s goto %s\n%s:%s\t\tgoto %s\n%s:%s%s:",
				condExp->code,
				condExp->var,
				root->falselabel,
				root->truelabel,
				bodyIf->code,
				root->label,
				root->falselabel,
				root->right->code,
				root->label
			);
		}else{	
			getCodeLabelsForShortCircuit(condExp);
			shortCircuitEvalTo3AC(condExp);
			condsTo3AC(bodyIf);
			condsTo3AC(root->right);
			root->label = newCodeLabel();
			printf("%s\n\n\n",condExp->code);
			asprintf(&root->code,"%s%s:%s\t\tgoto %s\n%s:%s%s:",
				condExp->code,
				condExp->truelabel,
				bodyIf->code,
				root->label,
				condExp->falselabel,
				bodyElse->code,
				root->label
			);
		}
	}else if(!strcmp(root->desc,"ELSE")){
		condsTo3AC(root->left->left);
		root->code = strdup(root->left->left->code);
	}else if(!strcmp(root->desc,"INNER_ENV")){
		varDecsTo3AC(root->right);
		root->code = strdup(root->right->code);
	}
	else if(!strcmp(root->desc,"COND_STAT")){
		conditionStatementTo3AC(root->right);
		root->code = strdup(root->right->code);
	}else{
		printf("\nLogical Error inside condsTo3AC\n" );
		exitWithError();
	}
}
void loopsTo3AC(node* root){
	if(!root){
		return;
	}else if(!strcmp(root->value,"DO")){
		node * rootExp = root->right;
		node* bodyDoWhile = root->left;
		if (isSimpleCond(rootExp->left)){
			root->truelabel = newCodeLabel();
			loopsTo3AC(bodyDoWhile);
			root->falselabel = newCodeLabel();
			expTo3AC(rootExp->left);
			asprintf(&root->code,"%s%s:%s\t\tifz %s goto %s\n\t\tgoto %s\n%s:",
			rootExp->left->code,
			root->truelabel,
			root->left->code,
			rootExp->left->var,
			root->falselabel,
			root->truelabel,
			root->falselabel
			);
		}else{
			loopsTo3AC(bodyDoWhile);
			getCodeLabelsForShortCircuit(rootExp->left);
			shortCircuitEvalTo3AC(rootExp->left);
			asprintf(&root->code,"%s:%s%s%s:",
				rootExp->left->truelabel,
				bodyDoWhile->code,
				rootExp->left->code,
				rootExp->left->falselabel
			);
		}
		
	}else if(!strcmp(root->value,"WHILE")){
		node* bodyWhile = root->right->left;
		node* condEXP = root->left;
		if (isSimpleCond(root->left)){
			root->truelabel = newCodeLabel();
			loopsTo3AC(bodyWhile);
			root->falselabel = newCodeLabel();
			expTo3AC(condEXP);
			asprintf(&root->code,"%s%s:\t\tifz %s goto %s\n%s\t\tgoto %s\n%s:",
				condEXP->code,
				root->truelabel,
				condEXP->var,
				root->falselabel,
				bodyWhile->code,
				root->truelabel,
				root->falselabel
			);
		}else{
			root->truelabel = newCodeLabel();
			getCodeLabelsForShortCircuit(condEXP);
			shortCircuitEvalTo3AC(condEXP);
			loopsTo3AC(bodyWhile);
			asprintf(&root->code,"%s:%s%s:%s\t\tgoto %s\n%s:",
				root->truelabel,
				condEXP->code,
				condEXP->truelabel,
				bodyWhile->code,
				root->truelabel,
				condEXP->falselabel
			);
		}
	}else if(!strcmp(root->value,"FOR")){
		node* blockCode = root->left->right->right->right;
		node* initAss = root->left->left;
		node* condExp = root->left->right->left;
		node* updAss = root->left->right->right->left;
		if (isSimpleCond(condExp)){
			root->truelabel = newCodeLabel();
			loopsTo3AC(blockCode);
			root->falselabel = newCodeLabel();
			expTo3AC(condExp->left);//condEXP
			assigmentTo3AC(updAss);
			assigmentTo3AC(initAss);
			asprintf(&root->code,"%s%s:%s\t\tifz %s goto %s\n%s%s\t\tgoto %s\n%s:",
				initAss->code,
				root->truelabel,
				condExp->code,
				condExp->var,
				root->falselabel,
				blockCode->code,
				updAss->code,
				root->truelabel,
				root->falselabel
			);
		}else{
			root->truelabel = newCodeLabel();
			getCodeLabelsForShortCircuit(condExp);
			shortCircuitEvalTo3AC(condExp);
			loopsTo3AC(blockCode);
			assigmentTo3AC(updAss);
			assigmentTo3AC(initAss);
			asprintf(&root->code,"%s%s:%s%s:%s%s\t\tgoto %s\n%s:",
				initAss->code,
				root->truelabel,
				condExp->code,
				condExp->truelabel,
				blockCode->code,
				updAss->code,
				root->truelabel,
				condExp->falselabel
			);
		}
	}else if(!strcmp(root->desc,"INNER_ENV")){
		varDecsTo3AC(root->right);
		root->code = strdup(root->right->code);
	}else if(!strcmp(root->desc,"COND_STAT")){
		conditionStatementTo3AC(root->right);
		root->code = strdup(root->right->code);
	}else{
		printf("\nLogical Error inside LoopsTo3AC\n" );
		exitWithError();
	}
}

void assigmentTo3AC(node *root){
	if(!root){
		return;
	}
	if(!strcmp(root->desc,"PRIMASS")){
		if(isSimpleCond(root->right->left->right)){
			expTo3AC(root->right->left->right);
			if(root->right->left->right->var2){
				asprintf(&root->code,"%s\t\t%s = *%s\n",root->right->left->right->code,root->var,root->right->left->right->var2);
			}
			else{
				asprintf(&root->code,"%s\t\t%s = %s\n",root->right->left->right->code,root->var,root->right->left->right->var);
			}
		}else{
			getCodeLabelsForShortCircuit(root->right->left->right);
			shortCircuitEvalTo3AC(root->right->left->right);
			root->label = newCodeLabel();
			asprintf(&root->code,"%s%s:\t\t%s = 1\n\t\tgoto %s\n%s:\t\t%s = 0\n%s:",
			root->right->left->right->code,
			root->right->left->right->truelabel,
			root->var,
			root->label,
			root->right->left->right->falselabel,
			root->var,
			root->label);
		}
		
	}
	else if(!strcmp(root->desc,"INDEXASS")){
		expTo3AC(root->right->left->left->left); // [EXP]
		expTo3AC(root->right->left->right);//ass EXP
		asprintf(&root->code,"%s%s\t\t%s = &%s\n\t\t%s = %s + %s\n\t\t*%s = %s\n",
		root->right->left->left->left->code,
		root->right->left->right->code,
		root->var,
		root->value,
		root->var2,
		root->var,
		root->right->left->left->left->var,
		root->var2,
		root->right->left->right->var);

	}
	else if(!strcmp(root->desc,"PTRASS")){
		expTo3AC(root->right->left->right);
		asprintf(&root->code,"%s\t\t*%s = %s\n",root->right->left->right->code,root->var,root->right->left->right->var);
	}
	return;
}
bool isRelativeOp(char * value){
	if (!strcmp("==", value) || !strcmp(">", value) || !strcmp("&&", value) 
		|| !strcmp("!=", value)|| !strcmp("||", value) || !strcmp("<=", value) 
		|| !strcmp("<", value) || !strcmp(">=", value)) {
		return TRUE;
	}
	return FALSE;

}
bool isOperator(char* value){
	if(isRelativeOp(value)){
		return TRUE;
	}else if( !strcmp("+", value) || !strcmp("-", value) || !strcmp("*", value) 
		|| !strcmp("/", value)|| !strcmp("UN_OP",value))
	{
		return TRUE;	
	}
	return FALSE;
}
void conditionStatementTo3AC(node* root){
	if(!strcmp(root->desc,"FUNC_CALL")){
		func* function = findFuncInGlobal(root->left->value);
		FuncCallTo3AC(root->left->left,function,FALSE,NULL);
		if(root->left->left){
			root->code = strdup(root->left->left->code);
		}else{
			asprintf(&root->code,"\t\tLCall %s\n",function->name);
		}	
	} else if(!strcmp(root->desc,"CONDS")){
		condsTo3AC(root->left);
		root->code = strdup(root->left->code);
	}else if(!strcmp(root->desc,"LOOPS")){
		loopsTo3AC(root->left);
		root->code = strdup(root->left->code);
	}else if(!strcmp(root->desc,"RET")){
		expTo3AC(root->left->left);
		asprintf(&root->code,"%s\t\tReturn %s\n",root->left->left->code,root->left->left->var);
	}else if(!strcmp(root->desc,"RIGHT")){
		assigmentTo3AC(root->left);
		root->code = strdup(root->left->code);
	}
	
	return;

}
void strDeclarationsTo3AC(node * root){
	if(!root){
		return;
	}
	if(!strcmp(root->desc,"STR_1L")){
		if(root->right){
			strDeclarationsTo3AC(root->right);	
			root->code = strdup(root->right->code);
		}
	}else if(!strcmp(root->desc,"STR_2L")){
		 root->var = strdup(root->left->left->value);
		 if(root->right){
			strDeclarationsTo3AC(root->right);	
			asprintf(&root->code,"\t\t%s = %s\n%s",root->var,root->left->right->value,root->right->code);
		}else{
			asprintf(&root->code,"\t\t%s = %s\n",root->var,root->left->right->value);
		}
		 
	}else if(!strcmp(root->desc,"STR_3L")){
		root->var = strdup(root->left->left->left->value);
		if(root->right){
			strDeclarationsTo3AC(root->right);	
			asprintf(&root->code,"\t\t%s = %s\n%s",root->var,root->left->left->right->value,root->right->code);
		}else{
			asprintf(&root->code,"\t\t%s = %s\n",root->var,root->left->left->right->value);
		}
		
	}else if(!strcmp(root->desc,"D-RIGHT")){
		strDeclarationsTo3AC(root->left);
		if(root->right){
			strDeclarationsTo3AC(root->right);	
			root->code = strdup(root->right->code);
			asprintf(&root->code,"%s%s",root->left->code,root->right->code);
		}
		else{
			root->code = strdup(root->left->code);
		}
	}else{
		return;
	}	
	
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
		root->code = strdup(root->right->code);
		
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

void FuncCallTo3AC(node* root,func* function,bool isAssigemnt,char* varLabel){
	if(!root){
		return;
	}
	node* temp_root = root;
	node* exp = NULL;
	if(root->right){
		root->code = strdup(root->right->code);
	}
	char* code = NULL;
	char* temp = NULL;
	char* pushParams = NULL;
	char* temp_pushParams = NULL;
	exp = temp_root->left;
	expTo3AC(exp);
	asprintf(&code,"%s",exp->code);
	asprintf(&pushParams,"\t\tPushParam %s\n",exp->var);
	temp_root= temp_root->right;
	while(temp_root){
		exp = temp_root->left;
		expTo3AC(exp);
		temp = code;
		code = NULL;
		temp_pushParams = pushParams;
		pushParams = NULL;
		asprintf(&code,"%s%s",temp,exp->code);
		asprintf(&pushParams,"%s\t\tPushParam %s\n",temp_pushParams,exp->var);
		free(temp);
		temp = NULL;
		free(temp_pushParams);
		temp_pushParams = NULL;
		temp_root= temp_root->right;
	}
	if(isAssigemnt){
		asprintf(&root->code,"%s%s\t\t%s = LCall %s\n\t\tPopParams %d\n",code,pushParams,varLabel,function->name,calcBytesForFunc(function));
	}
	else{
		asprintf(&root->code,"%s%s\t\tLCall %s\n\t\tPopParams %d\n",code,pushParams,function->name,calcBytesForFunc(function));
	}
	free(code);
	free(pushParams);
}

void primDeclarationsTo3AC(node* root){
	if(!root){
		return;
	}
	if(!strcmp(root->desc,"DEC_ASS_PRIM")){
		
		if(isSimpleCond(root->left->left->right)){
			expTo3AC(root->left->left->right);
			asprintf(&root->code,"%s\t\t%s = %s\n",
			root->left->left->right->code,root->left->left->left->var,root->left->left->right->var);
		}else{
			getCodeLabelsForShortCircuit(root->left->left->right);
			shortCircuitEvalTo3AC(root->left->left->right);
			root->label = newCodeLabel();
			asprintf(&root->code,"%s%s:\t\t%s = 1\n\t\tgoto %s\n%s:\t\t%s = 0\n%s:",
			root->left->left->right->code,
			root->left->left->right->truelabel,
			root->left->left->left->var,
			root->label,
			root->left->left->right->falselabel,
			root->left->left->left->var,
			root->label);
		}
	}else if(!strcmp(root->desc,"ID")){
		primDeclarationsTo3AC(root->right);
		if(root->right){
			root->code = strdup(root->right->code);
		}
	}else if(!strcmp(root->desc,"DEC_ASS_PRIM_NODE")){
		if(isSimpleCond(root->left->left->right)){
			expTo3AC(root->left->left->right);
			primDeclarationsTo3AC(root->right);
			asprintf(&root->code,"%s\t\t%s = %s\n%s",
			root->left->left->right->code,
			root->left->left->left->var,
			root->left->left->right->var,
			root->right->code);
		}else{
			getCodeLabelsForShortCircuit(root->left->left->right);
			shortCircuitEvalTo3AC(root->left->left->right);
			primDeclarationsTo3AC(root->right);
			root->label = newCodeLabel();
			asprintf(&root->code,"%s%s:\t\t%s = 1\n\t\tgoto %s\n%s:\t\t%s = 0\n%s",
			root->left->left->right->code,
			root->left->left->right->truelabel,
			root->left->left->left->var,
			root->label,
			root->left->left->right->falselabel,
			root->left->left->left->var,
			root->right->code);
		}
	}else if(!strcmp(root->desc,"DEC_ASS_PRIM_LEAF")){
		if(isSimpleCond(root->left->right)){
			expTo3AC(root->left->right);
			asprintf(&root->code,"%s\t\t%s = %s\n",
			root->left->right->code,
			root->left->left->var,
			root->left->right->var);
		}else{
			getCodeLabelsForShortCircuit(root->left->right);
			shortCircuitEvalTo3AC(root->left->right);
			root->label = newCodeLabel();
			asprintf(&root->code,"%s%s:\t\t%s = 1\n\t\tgoto %s\n%s:\t\t%s = 0\n%s:",
			root->left->right->code,
			root->left->right->truelabel,
			root->left->left->var,
			root->label,
			root->left->right->falselabel,
			root->left->left->var,
			root->label);
		}	
	}
	return;
}

void expTo3AC(node* root){
	if(!strcmp(root->desc, "BOOL")
		|| !strcmp(root->desc, "CHAR")
		|| !strcmp(root->desc, "INT")
	    || !strcmp(root->desc, "REAL")
		|| !strcmp(root->desc, "NULLP")
		|| !strcmp(root->desc, "ID"))
	{
			return;
	}else if(!strcmp(root->desc, "ID_INDEX")){
		expTo3AC(root->left);
		asprintf(&root->code,"%s\t\t%s = &%s\n\t\t%s = %s + %s\n",
		root->left->code,
		root->var,
		root->value,
		root->var2,
		root->var,
		root->left->var);
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
	else if(!strcmp(root->desc, "+") || !strcmp(root->desc, "-") || !strcmp(root->desc, "*")|| !strcmp(root->desc, "/") 
	        || !strcmp(root->desc, "!=") || !strcmp(root->desc, "==") || !strcmp(root->desc, "<")
			|| !strcmp(root->desc, ">")  || !strcmp(root->desc, "<=") || !strcmp(root->desc, ">=") || !strcmp(root->desc, "||") || !strcmp(root->desc, "&&")){
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
		root->left->var);
	}else if(!strcmp(root->value, "FUNC_CALL")){
		func* function = findFuncInGlobal(root->left->value);
		FuncCallTo3AC(root->left->left,function,TRUE,root->var);
		asprintf(&root->code,"%s",root->left->left->code);
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
	int size =f->numOfArgs ;
	for(int i = 0 ; i<size; i++)
		counter += getBytesByType(f->argsType[i]);
	return counter;
}
int getBytesByType(int type){
	if(type == BOOL || type == INT){
		return 4;
	}else if(type == REAL || type == INTP || type == CHARP || type == REALP){
		return 8;
	}else if(type == CHAR){
		return 1;
	}
	return 0;

}
void shortCircuitEvalTo3AC(node* root){
	node* leftson = root->left->left;
	node* rightson = root->left->right;
	bool leftsonIsSimple = isSimpleCond(leftson);
	bool rightsonIsSimple = isSimpleCond(rightson);
	bool isAndOperator = !strcmp(root->desc,"&&");
    bool isOrOperator = !isAndOperator;
	if(leftsonIsSimple){
		expTo3AC(leftson);
	}else{
		shortCircuitEvalTo3AC(leftson);
	}


	if(rightsonIsSimple){
		expTo3AC(rightson);
	}else{
		shortCircuitEvalTo3AC(rightson);
	}
	if(isAndOperator && leftsonIsSimple && rightsonIsSimple){
		//same t & f labels
		asprintf(&root->code,"%s\t\tifz %s goto %s\n%s\t\tifz %s goto %s\n",
			leftson->code,
			leftson->var,          
			root->falselabel, 
			rightson->code,
			rightson->var,          
			root->falselabel
		);
	}else if(isOrOperator && leftsonIsSimple && rightsonIsSimple){
		//same t & f labels
		asprintf(&root->code,"%s\t\tif %s goto %s\n%s\t\tif %s goto %s\n\t\tgoto %s\n",
			leftson->code,
			leftson->var,          
			root->truelabel, 
			rightson->code,
			rightson->var,          
			root->truelabel,
			root->falselabel
		);
	}else if(isAndOperator && !leftsonIsSimple && rightsonIsSimple){
		if(!strcmp(leftson->desc,"&&")){
			asprintf(&root->code,"%s%s:%s\t\tifz %s goto %s\n\t\tgoto %s\n",
			leftson->code,
			leftson->truelabel,
			rightson->code,
			rightson->var,          
			root->falselabel,
			root->truelabel
			);
		}else{
			asprintf(&root->code,"%s%s:%s\t\tifz %s goto %s\n\t\tgoto %s\n",
			leftson->code,
			leftson->truelabel,
			rightson->code,
			rightson->var,          
			root->falselabel,
			root->truelabel
			);
		}
	}else if(isOrOperator && !leftsonIsSimple && rightsonIsSimple){
		if(!strcmp(leftson->desc,"&&")){
			//same t but diff f labes
			asprintf(&root->code,"%s\t\tgoto %s\n%s:%s\t\tif %s goto %s\n\t\tgoto %s\n",
			leftson->code, 
			leftson->truelabel,
			leftson->falselabel,
			rightson->code,
			rightson->var,          
			root->truelabel,
			root->falselabel
			);
		}else{
			//same t but diff f labes
			asprintf(&root->code,"%s%s:\t\tif %s goto %s\n%s\t\tgoto %s\n",
			leftson->code,
			leftson->falselabel,
			rightson->var,          
			root->truelabel,
			rightson->code,
			root->falselabel
			);
		}
		
	}else if(isAndOperator && leftsonIsSimple && !rightsonIsSimple){
		if(!strcmp(rightson->desc,"&&")){
			asprintf(&root->code,"%s\t\tifz %s goto %s\n%s", 
			leftson->code,
			leftson->var,          
			root->falselabel, 
			rightson->code
			);
		}else{
			asprintf(&root->code,"%s\t\tifz %s goto %s\n%s",
			leftson->code,
			leftson->var,          
			root->falselabel, 
			rightson->code
			);
		}
		
	}else if(isOrOperator && leftsonIsSimple && !rightsonIsSimple){
		//same t & f labels 
		if(!strcmp(rightson->desc,"&&")){
			asprintf(&root->code,"%s\t\tif %s goto %s\n%s", 
			leftson->code,
			leftson->var,          
			root->truelabel, 
			rightson->code
			);
		}else{
			asprintf(&root->code,"%s\t\tif %s goto %s\n%s",
			leftson->code,
			leftson->var,          
			root->truelabel, 
			rightson->code
			);
		}
			
	}else if(isAndOperator && !leftsonIsSimple && !rightsonIsSimple){
		if(!strcmp(leftson->desc,"&&") && !strcmp(rightson->desc,"||") ){
			asprintf(&root->code,"%s%s",
			leftson->code,
			rightson->code      
			);
		}else if(!strcmp(leftson->desc,"||") &&!strcmp(rightson->desc,"&&") ){
			asprintf(&root->code,"%s%s:%s\t\tgoto %s\n",
			leftson->code,
			leftson->truelabel,
			rightson->code,
			root->truelabel      
			);
		}else if(!strcmp(leftson->desc,"&&") &&!strcmp(rightson->desc,"&&") ){
			asprintf(&root->code,"%s%s\t\tgoto %s\n",
			leftson->code,
			rightson->code,
			root->truelabel      
			);
		}else if(!strcmp(leftson->desc,"||") &&!strcmp(rightson->desc,"||") ){
			asprintf(&root->code,"%s%s:%s\t\tgoto %s\n",
			leftson->code,
			leftson->truelabel,
			rightson->code,
			root->falselabel      
			);
		}else{
			printf("Logical error inside shortCircuitEvalTo3AC 1\n");
			exitWithError();
		}
	}else if(isOrOperator && !leftsonIsSimple && !rightsonIsSimple){
		if(!strcmp(leftson->desc,"&&") && !strcmp(rightson->desc,"||") ){
			asprintf(&root->code,"%s\t\tgoto %s\n%s:%s",
			leftson->code,
			root->truelabel,
			leftson->falselabel,
			rightson->code      
			);
		}else if(!strcmp(leftson->desc,"&&") && !strcmp(rightson->desc,"&&") ){
			//same t but diff f labes
			asprintf(&root->code,"%s\t\tgoto %s\n%s:%s\t\tgoto %s\n",
			leftson->code,
			leftson->truelabel,
			leftson->falselabel,
			rightson->code,
			root->truelabel      
			);
		}else if(!strcmp(leftson->desc,"||") && !strcmp(rightson->desc,"&&")){
			asprintf(&root->code,"%s\t\tgoto %s\n%s:%s",
			leftson->code,
			leftson->falselabel,
			leftson->falselabel,
			rightson->code      
			);
		}else if(!strcmp(leftson->desc,"||") && !strcmp(rightson->desc,"||")){
			asprintf(&root->code,"%s%s:%s",
			leftson->code,
			leftson->falselabel,
			rightson->code      
			);
		}else{
			printf("Logical error inside shortCircuitEvalTo3AC 2\n");
			exitWithError();
		}
	}else{
		printf("\nLogical Error inside shortCircuitEvalTo3AC!\n" );
		exitWithError();
	}
}
bool isSimpleCond(node* root){
	return !isOperator(root->desc) ||(!isRelativeOp(root->left->left->desc) && !isRelativeOp(root->left->right->desc)) || (strcmp(root->desc,"&&") &&  strcmp(root->desc,"||"));
}
void getCodeLabelsForShortCircuit(node* root){
	if(!root || !root->left || !root->right || !root->left->left || !root->left->right){
		return;
	}
	node* leftson = root->left->left;
	node* rightson = root->left->right;
	bool leftsonIsSimple = isSimpleCond(leftson);
	bool rightsonIsSimple = isSimpleCond(rightson);
	bool isAndOperator = !strcmp(root->desc,"&&");
    bool isOrOperator = !strcmp(root->desc,"||");
	if(leftsonIsSimple && rightsonIsSimple && (isAndOperator || isOrOperator)){
		root->truelabel = newCodeLabel();
		root->falselabel = newCodeLabel();
	}else if(isAndOperator && !leftsonIsSimple && rightsonIsSimple){
		getCodeLabelsForShortCircuit(leftson);
		root->truelabel = newCodeLabel();
		root->falselabel = strdup(leftson->falselabel);
	}else if(isOrOperator && !leftsonIsSimple && rightsonIsSimple){
		getCodeLabelsForShortCircuit(leftson);
		root->falselabel = newCodeLabel();
		root->truelabel = strdup(leftson->truelabel);
	}else if(leftsonIsSimple && !rightsonIsSimple){
		getCodeLabelsForShortCircuit(rightson);
		root->truelabel = strdup(rightson->truelabel);
		root->falselabel = strdup(rightson->falselabel);
	}else if(isAndOperator && !leftsonIsSimple && !rightsonIsSimple){
		if(!strcmp(leftson->desc,"&&")){
			//same f but diff t labels
			getCodeLabelsForShortCircuit(leftson);
			rightson->truelabel = strdup(leftson->truelabel);
			rightson->falselabel = strdup(leftson->falselabel);
			root->truelabel = strdup(rightson->truelabel);
			root->falselabel = strdup(rightson->falselabel);
			getCodeLabelsForShortCircuit(rightson->left->left);
			getCodeLabelsForShortCircuit(rightson->left->right);
		}else if(!strcmp(leftson->desc,"||")){
			getCodeLabelsForShortCircuit(leftson);
			root->truelabel = newCodeLabel();
			root->falselabel = strdup(leftson->falselabel);
			rightson->truelabel = strdup(root->truelabel);
			rightson->falselabel = strdup(root->falselabel);
			getCodeLabelsForShortCircuit(rightson->left->left);
			getCodeLabelsForShortCircuit(rightson->left->right);
		}
	}else if(isOrOperator && !leftsonIsSimple && !rightsonIsSimple){
			getCodeLabelsForShortCircuit(leftson);
			root->truelabel = strdup(leftson->truelabel);
			root->falselabel = newCodeLabel();
			rightson->truelabel = strdup(root->truelabel);
			rightson->falselabel = strdup(root->falselabel);
			getCodeLabelsForShortCircuit(rightson->left->left);
			getCodeLabelsForShortCircuit(rightson->left->right);
	}
}
void writeToTxtFile(char* code){
	FILE* file  = fopen("output.txt", "w+");
	if (file == NULL){
    	printf("Error opening file!\n");
    	exitWithError();
	}
	fprintf(file, "%s", code);
	fclose(file);
}
