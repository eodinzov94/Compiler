%{
    #include <string.h>
    #include <stdlib.h>
    #include <stdio.h> 
    #include "y.tab.h"
%}

%x COMMENT

%%
 /*PRIMITIVE + POINTER TYPES*/
"bool"|"real"|"int"|"char"|"int*"|"char*"|"real*"		{yylval.string = strdup(yytext); return VALTYPE;}
 /*STRING TYPE*/
"string"                                                       {yylval.string = strdup(yytext); return STR;}
 /*STATEMENTS*/
"if"                                                           {return IF;}   
"else"                                                         {return ELSE;}                                                              
 /*LOOPS*/
"while"                                                		{return WHILE;}
"do"                                                 			{return DO;}
"for"															{return FOR;}
 /*ASSIGMENT*/
"var"															{return VAR;}
 /*FUNCTION COMPONENTS*/
"return"                                                       {return RETURN;}
"void"                                                         {return VOID;}
 /*SPECIAL VALUE*/
"null"                                                         {return PNULL;}
 /*ARITMETHIC OPERATORS*/
"+"                                                            {yylval.string = strdup(yytext); return PLUS;}
"-"                                                            {yylval.string = strdup(yytext); return MINUS;}
"*"                                                            {yylval.string = strdup(yytext); return MULT;}
"/"                                                            {yylval.string = strdup(yytext); return DIV;}
 /*LOGICAL OPERATORS*/
"||"                                                           {yylval.string = strdup(yytext); return OR;}  
"&&"                                                           {yylval.string = strdup(yytext); return AND;}
"=="                                                           {yylval.string = strdup(yytext); return EQ;}
"<"                                                            {yylval.string = strdup(yytext); return L;}
">"                                                            {yylval.string = strdup(yytext); return GR;}
"<="                                                           {yylval.string = strdup(yytext); return LE;}                                                               
">="                                                           {yylval.string = strdup(yytext); return GRE;} 
"!"                                                            {yylval.string = strdup(yytext); return NOT;}         
"!="                                                           {yylval.string = strdup(yytext); return NOTEQ;} 
 /*OTHER OPERATORS*/
"&"                                                            {yylval.string = strdup(yytext); return ADDRS;} 
"="                                                            {yylval.string = strdup(yytext); return ASS;}
 /*LITERAL VALUES*/
"true" | "false"												{yylval.string = strdup(yytext); return BOOLVAL;}
'[^\']'                                                        {yylval.string = strdup(yytext); return CHARVAL;}
0|[1-9][0-9]*													{yylval.string = strdup(yytext); return DECVAL;}
0[xX][0-9A-F]+													{yylval.string = strdup(yytext); return HEXVAL;}
[0-9]*[\.]?[0-9]*?[eE][-+]?[0-9][1-9]*|[0-9]*[\.]?[0-9]+       {yylval.string = strdup(yytext); return REALVAL;}		
\"[^\"]*\"														{yylval.string = strdup(yytext); return STRVAL;}
[a-zA-Z][a-zA-Z0-9_]*											{yylval.string = strdup(yytext); return ID;}
 /*OTHERS*/
"\n"															{yylineno++;}
[ \t\r]													        {;}
 /*COMMENT*/
\/\*															{BEGIN(COMMENT);}
<COMMENT>\*\/													{BEGIN(INITIAL);}
<COMMENT>\n													{yylineno++;}
<COMMENT>.														{;}
 /*ALL OTHER LEXEMS*/
.																{return yytext[0];}   
%%
