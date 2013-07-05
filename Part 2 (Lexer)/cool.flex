/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 *  to the code in the file.  Don't remove anything that was here initially
 */
%{ /* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = */
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
bool bad_str = false;
int cmnt_lvl = 0;             //for nested comments
int str_buf_indx = 0;     //same as string_buf_ptr, but also keeps size

%} /* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = */

/*
 * Define names for regular expressions here.
 */

DARROW          =>
LE              <=
WSPACE          [ \t\n\f\r\v]+
NUM             [0-9]+
NUM_OP          [+\-*\\^]
VAR             [a-zA-Z]+[0-9a-zA-Z_]*
ASSIGN          <-
COMMENT_SL      --
COMMENT_START   (*
COMMENT_END     *)
OBJECT_ID       [a-z]([0-9a-zA-Z_]*)
TYPE_ID         [A-Z]([0-9a-zA-Z_]*)

%x COMMENT STRING SL_COMMENT

%% /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

 /*
  *  Nested comments
  */
<INITIAL,COMMENT>\(\*   {
    BEGIN(COMMENT);
    ++cmnt_lvl;
}

<INITIAL,COMMENT>\*\)   {
    switch(YY_START){
        case INITIAL:
            cool_yylval.error_msg = "Unmatched *)";
            return ERROR;
        case COMMENT:
            --cmnt_lvl;                       //end of (one) level comment
            if(cmnt_lvl <= 0) BEGIN(INITIAL); //done with comment, else still in
    }
}

<COMMENT><<EOF>>    {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "EOF in comment";
    return ERROR;
}

{COMMENT_SL}      { BEGIN(SL_COMMENT);            } //In single line comment
<SL_COMMENT>\\n   { curr_lineno++; BEGIN(INITIAL); } //End of single line comment

<COMMENT,SL_COMMENT>.  { /* Ignoring Comments */ }

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW);  }
{ASSIGN}    { return (ASSIGN);  }
{LE}        { return (LE);      }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

[Cc][Ll][Aa][Ss][Ss]              { return CLASS;     }
[Ee][Ll][Ss][Ee]                  { return ELSE;      }
[Ff][Ii]                          { return FI;        }
[Ii][Ff]                          { return IF;        }
[Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]  { return INHERITS;  }
[Ii][Ss][Vv][Oo][Ii][Dd]          { return ISVOID;    }
[Ll][Ee][Tt]                      { return LET;       }
[Ll][Oo][Oo][Pp]                  { return LOOP;      }
[Pp][Oo][Oo][Ll]                  { return POOL;      }
[Tt][Hh][Ee][Nn]                  { return THEN;      }
[Ww][Hh][Ii][Ll][Ee]              { return WHILE;     }
[Cc][Aa][Ss][Ee]                  { return CASE;      }
[Ee][Ss][Aa][Cc]                  { return ESAC;      }
[Nn][Ee][Ww]                      { return NEW;       }
[Oo][Ff]                          { return OF;        }
[Nn][Oo][Tt]                      { return NOT;       }

f[Aa][Ll][Ss][Ee]             { cool_yylval.boolean = 0; return BOOL_CONST; }
t[Rr][Uu][Ee]                 { cool_yylval.boolean = 1; return BOOL_CONST; }

"+"   { return '+'; }
"-"   { return '-'; }
"<"   { return '<'; }
"="   { return '='; }
"/"   { return '/'; }
"*"   { return '*'; }
"."   { return '.'; }
","   { return ','; }
"~"   { return '~'; }
";"   { return ';'; }
":"   { return ':'; }
"("   { return '('; }
")"   { return ')'; }
"{"   { return '{'; }
"}"   { return '}'; }
"@"   { return '@'; }

{OBJECT_ID}   {
    cool_yylval.symbol = idtable.add_string(yytext, yyleng);
    return OBJECTID;
}

{TYPE_ID}   {
    cool_yylval.symbol = idtable.add_string(yytext, yyleng);
    return TYPEID;
}

{NUM}   {
    cool_yylval.symbol = inttable.add_int(atoi(yytext));
    return INT_CONST;
}
 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\"   {
    BEGIN(STRING);
    bad_str = (str_buf_indx = 0); 
}

<STRING>\"    {
    BEGIN(INITIAL);
    if(str_buf_indx > MAX_STR_CONST){
        cool_yylval.error_msg = "String constant too long";
        return ERROR;
    }
    else if(!bad_str){
        string_buf[str_buf_indx] = '\0';
        cool_yylval.symbol = stringtable.add_string(string_buf);
        return STR_CONST;
    }
}

<STRING>\n  {
    BEGIN(INITIAL);
    curr_lineno++;
    cool_yylval.error_msg = "Unterminated string constant";
    return ERROR;
}

<STRING>\\(.|\n)    {
  char c = 0;
  switch(yytext[1]){
      case 'n': string_buf[str_buf_indx++] = '\n'; break;
      case 'b': string_buf[str_buf_indx++] = '\b'; break;
      case 'f': string_buf[str_buf_indx++] = '\f'; break;
      case 't': string_buf[str_buf_indx++] = '\t'; break;
      case '\n': curr_lineno++;
      default: string_buf[str_buf_indx++] = yytext[1];
  }
}

<STRING>[\0]  {
  cool_yylval.error_msg = "String contains null character";
  bad_str = 1;
  return ERROR;
}

<STRING><<EOF>> {
  BEGIN(INITIAL);
  bad_str = 1;
  cool_yylval.error_msg = "EOF in string constant";
  return ERROR;
}

<STRING>[^\\\n\"]+   {
  char *yptr = yytext;
  while(*yptr) string_buf[str_buf_indx++] = *yptr++;
}

<INITIAL,COMMENT>\\n      { curr_lineno++; }

{WSPACE} { /* Ignore White Space */ }

. { cool_yylval.error_msg = yytext; return(ERROR); }

%% /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
