/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
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
char *string_buffer_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

unsigned int comment_depth = 0;
unsigned int string_buffer_left;
bool string_error;


char * handle_backslash() {
  char *c = &yytext[1];
  if (*c == '\n') {
    curr_lineno++;
  }
  return c;
}

int append_string(char *str, unsigned int len) {
  if (len < string_buffer_left) {
    strncpy(string_buffer_ptr, str, len);
    string_buffer_ptr += len;
    string_buffer_left -= len;
    return 0;
  } else {
    string_error = true;
    yylval.error_msg = "String constant too long";
    return -1;
  }
}

/*
 *  Add Your own definitions here
 */

%}
CLASS           [cC][lL][aA][sS][sS]
DARROW          =>
DIGIT           [0-9]
ELSE            [eE][lL][sS][eE]
FALSE           f[aA][lL][sS][eE]
FI              [fF][iI]
IF              [iI][fF]
IN              [iI][nN]
INHERITS        [iI][nN][hH][eE][rR][iI][tT][sS]
ISVOID          [iI][sS][vV][oO][iI][dD]
LET             [lL][eE][tT]
LOOP            [lL][oO][oO][pP]
POOL            [pP][oO][oO][lL]
THEN            [tT][hH][eE][nN]
WHILE           [wW][hH][iI][lL][eE]
CASE            [cC][aA][sS][eE]
ESAC            [eE][sS][aA][cC]
NEW             [nN][eE][wW]
OF              [oO][fF]
NOT             [nN][oO][tT]
TRUE            t[rR][uU][eE]
OBJECTID        [a-z][_a-zA-Z0-9]*
TYPEID          [A-Z][_a-zA-Z0-9]*
NEWLINE         [\n]
NOTCOMMENT      [^\n*(\\]
NOTSTRING       [^\n\0\\\"]
WHITESPACE      [ \t\r\f\v]+
LE              <=
ASSIGN          <-
BACKSLASH       [\\]
NOTLEFTPAREN    [^(]
RIGHTPAREN      [)]
LINE_COMMENT    "--"
START_COMMENT   "(*"
END_COMMENT     "*)"
QUOTES          \"
%x COMMENT
%x STRING

%%

<INITIAL,COMMENT>{NEWLINE} {
    curr_lineno++;
}

{START_COMMENT} {
  comment_depth++;
  BEGIN(COMMENT);
}

<COMMENT><<EOF>> {
  yylval.error_msg = "EOF in comment";
  BEGIN(INITIAL);
  return (ERROR);
}

<COMMENT>[*]/[^)]    ;
<COMMENT>[(]/[^*]     ;
<COMMENT>{NOTCOMMENT}*             ;

<COMMENT>{BACKSLASH}(.|{NEWLINE}) {
  handle_backslash();
};
<COMMENT>{BACKSLASH}               ;
<COMMENT>{START_COMMENT} {
  comment_depth++;
}

<COMMENT>{END_COMMENT} {
  comment_depth--;
  if (comment_depth == 0) {
    BEGIN(INITIAL);
  }
}
<INITIAL>{END_COMMENT} {
  yylval.error_msg = "Unmatched *)";
  return (ERROR);
}
<INITIAL>{LINE_COMMENT}[^\n]*  ;
<INITIAL>{QUOTES} {
  BEGIN(STRING);
  string_buffer_ptr = string_buf;
  string_buffer_left = MAX_STR_CONST;
  string_error = false;
}

<STRING><<EOF>> {
  yylval.error_msg = "EOF in string constant";
  BEGIN(INITIAL);
  return ERROR;
}
<STRING>{NOTSTRING}* {
  int returnCode = append_string(yytext, strlen(yytext));
  if (returnCode != 0) {
    return (ERROR);
  }
}
<STRING>[\0] {
	yylval.error_msg = "String contains null character";
  string_error = true;
  return (ERROR);
}
<STRING>{NEWLINE} {
  BEGIN(INITIAL);
  curr_lineno++;
  if (!string_error) {
    yylval.error_msg = "Unterminated string constant";
    return (ERROR);
  }
}

<STRING>{BACKSLASH}(.|{NEWLINE}) {
  char *c = handle_backslash();
  int returnCode;
  switch (*c) {
    case 'n':
      returnCode = append_string("\n", 1);
      break;
    case 'b':
      returnCode = append_string("\b", 1);
      break;
    case 't':
      returnCode = append_string("\t", 1);
      break;
    case 'f':
      returnCode = append_string("\f", 1);
      break;
    case '\0':
			yylval.error_msg = "String contains null character";
			string_error = true;
      returnCode = -1;
      break;
    default:
      returnCode = append_string(c, 1);
  }
  if (returnCode != 0) {
    return (ERROR);
  }
}
<STRING>{BACKSLASH}             ;
<STRING>{QUOTES} {
  BEGIN(INITIAL);
  if (!string_error) {
    yylval.symbol = stringtable.add_string(string_buf, string_buffer_ptr - string_buf);
    return (STR_CONST);
  }
}

{WHITESPACE}                     ;
<INITIAL>{TRUE}                  { yylval.boolean = true; return (BOOL_CONST); }
<INITIAL>{FALSE}                 { yylval.boolean = false; return (BOOL_CONST); }
<INITIAL>{CLASS}                 { return (CLASS); }
<INITIAL>{ELSE}                  { return (ELSE); }
<INITIAL>{FI}                    { return (FI); }
<INITIAL>{IF}                    { return (IF); }
<INITIAL>{IN}                    { return (IN); }
<INITIAL>{INHERITS}              { return (INHERITS); }
<INITIAL>{ISVOID}                { return (ISVOID); }
<INITIAL>{LET}                   { return (LET); }
<INITIAL>{LOOP}                  { return (LOOP); }
<INITIAL>{POOL}                  { return (POOL); }
<INITIAL>{THEN}                  { return (THEN); }
<INITIAL>{WHILE}                 { return (WHILE); }
<INITIAL>{CASE}                  { return (CASE); }
<INITIAL>{ESAC}                  { return (ESAC); }
<INITIAL>{NEW}                   { return (NEW); }
<INITIAL>{OF}                    { return (OF); }
<INITIAL>{NOT}                   { return (NOT); }
<INITIAL>{DARROW}		 { return (DARROW); }
<INITIAL>{ASSIGN}                { return (ASSIGN); }
<INITIAL>{LE}                    { return (LE); }
<INITIAL>{TYPEID}                { yylval.symbol = stringtable.add_string(yytext); return (TYPEID); }
<INITIAL>{OBJECTID}              { yylval.symbol = stringtable.add_string(yytext); return (OBJECTID); }
<INITIAL>{DIGIT}+                { yylval.symbol = stringtable.add_string(yytext); return (INT_CONST); }
<INITIAL>";"                     { return int(';'); }
<INITIAL>","                     { return int(','); }
<INITIAL>":"                     { return int(':'); }
<INITIAL>"{"                     { return int('{'); }
<INITIAL>"}"                     { return int('}'); }
<INITIAL>"+"                     { return int('+'); }
<INITIAL>"-"                     { return int('-'); }
<INITIAL>"*"                     { return int('*'); }
<INITIAL>"/"                     { return int('/'); }
<INITIAL>"<"                     { return int('<'); }
<INITIAL>"="                     { return int('='); }
<INITIAL>"~"                     { return int('~'); }
<INITIAL>"."                     { return int('.'); }
<INITIAL>"@"                     { return int('@'); }
<INITIAL>"("                     { return int('('); }
<INITIAL>")"                     { return int(')'); }
<INITIAL>.                       { yylval.error_msg = yytext; return (ERROR); }

%%
