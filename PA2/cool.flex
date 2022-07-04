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
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int comment_cnt = 0;

%}

/*
 * Define names for regular expressions here.
 */

%Start            COMMENT
%Start            INLINE
%Start            STRING

CLASS             [cC]lass
INHERITS          inherits
IF                if
THEN              then
ELSE              else
FI                fi
WHILE             while
LOOP              loop
POOL              pool
LET               let
IN                in
NEW               new
CASE              case
ESAC              esac
OF                of
ISVOID            isvoid
NOT               not
TRUE              true
FALSE             false

DARROW            "=>"
LE                "<="
ASSIGN            "<-"
MULTIPLY          "*"
DIVIDE            "/"
TILDE             "~"
EMAIL             "@"
LEFT_BRACE        "{"
RIGHT_BRACE       "}"
COLON             ":"
SEMICOLON         ";"
COMMA             ","
LEFT_PARENTHESE   "("
RIGHT_PARENTHESE  ")"
POINT             "."
EQUAL             "="
MINUS             "-"
PLUS              "+"
LESS              "<"
INT_CONST         [0-9]+
BLANK             [ \t\f\r\v]+

TYPEID            [A-Z][a-zA-Z0-9_]*
OBJECTID          [a-z][a-zA-Z0-9_]*

%%

<INITIAL>\n { curr_lineno++; }

 /*
  *  Nested comments
  */

<INITIAL>"(*" {
    comment_cnt++;
    BEGIN (COMMENT);
}
<INITIAL>"*)" {
    yylval.error_msg = "Unmatched *)";
    return (ERROR);
}
<INITIAL>"--" { BEGIN (INLINE); }

<COMMENT>\n { curr_lineno++; }
<COMMENT>[^\n(*]* {}
<COMMENT>[*()] {}
<COMMENT>"*)" {
    comment_cnt--;
    if(comment_cnt == 0) BEGIN (INITIAL);
}
<COMMENT><<EOF>> {
    yylval.error_msg = "EOF in comment";
    BEGIN (INITIAL);
    return (ERROR);
}

<INLINE>[^\n]* {}
<INLINE>\n {
    curr_lineno++;
    BEGIN (INITIAL);
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

<INITIAL>\" { 
    BEGIN (STRING); 
    yymore();
}
<STRING>[^\"\n\\]+ { yymore(); }
<STRING>\\\n {
    curr_lineno++;
    yymore();
}
<STRING>\\. { yymore(); }
<STRING>\n {
    yylval.error_msg = "New line in STR_CONST";
    curr_lineno++;
    BEGIN (INITIAL);
    return (ERROR);
}
<STRING><<EOF>> {
    yylval.error_msg = "EOF in STR_CONST";
    BEGIN (INITIAL);
    return (ERROR);
}
<STRING>\" {
    std::string tmp = yytext, s = "";
    tmp = tmp.substr(1, yyleng - 2);
    size_t i = 0, j = i;
    while(j < tmp.length()) {
        while(j < tmp.length() && tmp[j] != '\\')
            ++j;
        s += tmp.substr(i, j - i);
        if(j == tmp.length()) break;
        switch(tmp[j + 1]) {
            case 'b': s += "\b"; break;
            case 'n': s += "\n"; break;
            case 't': s += "\t"; break;
            case 'f': s += "\f"; break;
            default: s += tmp[j + 1];
        }
        j += 2;
        i = j;
    }
    yytext = (char*)s.c_str();
    yylval.symbol = stringtable.add_string(yytext);
    BEGIN (INITIAL);
    return (STR_CONST);
}

  /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS}             { return (CLASS);    }
{INHERITS}          { return (INHERITS); }
{IF}                { return (IF);       }
{THEN}              { return (THEN);     }
{ELSE}              { return (ELSE);     }
{FI}                { return (FI);       }
{WHILE}             { return (WHILE);    }
{LOOP}              { return (LOOP);     }
{POOL}              { return (POOL);     }
{LET}               { return (LET);      }
{IN}                { return (IN);       }
{NEW}               { return (NEW);      }
{CASE}              { return (CASE);     }
{OF}                { return (OF);       }
{ISVOID}            { return (ISVOID);   }
{ESAC}              { return (ESAC);     }
{NOT}               { return (NOT);      }

 /*
  *  The multiple-character operators.
  */

{LEFT_BRACE}        { return ('{');    }
{RIGHT_BRACE}       { return ('}');    }
{COLON}             { return (':');    }
{SEMICOLON}         { return (';');    }
{COMMA}             { return (',');    }
{LEFT_PARENTHESE}   { return ('(');    }
{RIGHT_PARENTHESE}  { return (')');    }
{POINT}             { return ('.');    }
{EQUAL}             { return ('=');    }
{MINUS}             { return ('-');    }
{PLUS}              { return ('+');    }
{MULTIPLY}          { return ('*');    }
{DIVIDE}            { return ('/');    }
{LESS}              { return ('<');    }
{TILDE}             { return ('~');    }
{EMAIL}             { return ('@');    }
{DARROW}            { return (DARROW); }
{ASSIGN}            { return (ASSIGN); }
{LE}                { return (LE);     }
{BLANK}             {}
{TRUE} {
    yylval.boolean = 1;
    return (BOOL_CONST);
}
{FALSE} {
    yylval.boolean = 0;
    return (BOOL_CONST);
}

{TYPEID} {
    yylval.symbol = idtable.add_string(yytext);
    return (TYPEID);
}
{OBJECTID} {
    yylval.symbol = idtable.add_string(yytext);
    return (OBJECTID);
}
{INT_CONST} {
    yylval.symbol = idtable.add_string(yytext);
    return (INT_CONST);
}

 /*
  *  Error 
  */

[^\n] {
    yylval.error_msg = yytext;
    return (ERROR);
}

%%
