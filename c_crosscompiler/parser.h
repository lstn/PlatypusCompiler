/* File name:	parser.h
*  Author:		Lucas Estienne
*  Purpose:		Parser implementation declarations & prototypes
*/

#ifndef  PARSER_H_
#define  PARSER_H_ 

#include "token.h"
#include "stable.h"
#include "varstack.h"

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

enum
{
	ELSE,
	IF,
	INPUT,
	OUTPUT,
	PLATYPUS,
	REPEAT,
	THEN,
	USING
};

/* KW codes */
#define NO_ATTR -1

/* declare globals */
static Token lookahead;
static Buffer *sc_buf;
int synerrno = 0;
int indent_tabs = 1;

/* externals */
extern int line; /* line number */
extern STD sym_table; /* symbol table */
extern Buffer * str_LTBL; /*String literal table */
extern char * kw_table[]; /* keyword table */
extern Token mlwpar_next_token(Buffer *);
extern VarStack * var_stack;

/* prototypes */
void parser(Buffer* in_buf);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe(void);
void gen_incode(char *in_code);
void gen_ic_indent(char *in_code);

/* syntax prototypes */
void program(void);

void opt_statements(void);
void statements(void);
void statements_p(void);
void statement(void);

void assignment_statement(void);
void iteration_statement(void);
void input_statement(void);
void output_statement(void);
void selection_statement(void);

void output_list(void);
void opt_variable_list(void);
void variable_list(void);
void variable_identifier(void);
void variable_list_p(void);

void assignment_expression(void);
void arithmetic_expression(void);
void string_expression(void);
void conditional_expression(void);
void relational_expression(void);

void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);

void primary_string_expression(void);
void string_expression_p(void);

void logical_or_expression(void);
void logical_or_expression_p(void);
void logical_and_expression(void);
void logical_and_expression_p(void);

void primary_a_relational_expression(void);
void primary_s_relational_expression(void);

void relational_operator(void);


#endif
