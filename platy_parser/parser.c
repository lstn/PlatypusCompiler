/* File name:	parser.c
*  Author:		Lucas Estienne
*  Purpose:		Implements the functions and logic required for the Parser component of the compiler.
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
* to suppress the warnings about using "unsafe" functions like fopen()
* and standard sting library functions defined in string.h.
* The define does not have any effect in Borland compiler projects.
*/
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "parser.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Purpose:			 Begins parsing for PLATYPUS, by using the passed buffer as input
*					 and grabbing the first token using mlwpar_next_token(), and beginning
*					 to parse the program in the buffer using program().
*  Parameters:		 [in_buf: (Buffer *)]
*  Return value:	 void
*/
void parser(Buffer* in_buf){
	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program(); match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/* Purpose:			 Matches two tokens: the current input token (lookahead) and the
*					 token required by the parser.
*  Parameters:		 [pr_token_code: int, pr_token_attribute: int]
*  Return value:	 void
*/
void match(int pr_token_code, int pr_token_attribute){
	if (lookahead.code == pr_token_code){
		switch (pr_token_code){
			case KW_T:
			case LOG_OP_T:
			case ART_OP_T:
			case REL_OP_T:
				if (lookahead.attribute.get_int != pr_token_attribute)
					break; /* attributes do not match, break out */
			default: /* match */
				if (lookahead.code == SEOF_T)
					return;

				/* code was not SEOF, advance to next input*/
				lookahead = mlwpar_next_token(sc_buf);
				if (lookahead.code == ERR_T){
					syn_printe();
					lookahead = mlwpar_next_token(sc_buf);
					++synerrno;
				}
				return; /* successfully matched */
		}
	}
	syn_eh(pr_token_code); /* unsuccessful match */
}

/* Purpose:			 This function implements a simple panic mode error recovery.
*  Parameters:		 [sync_token_code: int]
*  Return value:	 void
*/
void syn_eh(int sync_token_code){
	syn_printe();
	++synerrno;

	while (lookahead.code != SEOF_T){ /* panic recovery, advance until we have sync_token_code match */
		lookahead = mlwpar_next_token(sc_buf);
		if (lookahead.code == sync_token_code){ /* match */
			if (lookahead.code != SEOF_T) /* check if we need to advance one more time */
				lookahead = mlwpar_next_token(sc_buf);
			return; /* done */
		}
	}

	if (sync_token_code != SEOF_T){  /* reached end of source with no match, exit */
		exit(synerrno);
	}

}

/* Purpose:			 Parser error printing function
*  Author:			 Svillen Ranev
*  Parameters:		 
*  Return value:	 void
*/
void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n");
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", sym_table.pstvr[t.attribute.get_int].plex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_cbhead(str_LTBL) + t.attribute.str_offset);
		break;
	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;
	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;
	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}

/* Purpose:			 Takes a string from the argument and prints it
*  Parameters:		 [in_code: char *]
*  Return value:	 void
*/
void gen_incode(char *in_code){
	printf("%s\n", in_code);
}

/**********************/
/* syntax productions */
/**********************/

/*
	<program>  ->
		PLATYPUS { <opt_statements> }

	FIRST set: { KW_T(but only PLATYPUS) }
*/
void program(void){
	match(KW_T, PLATYPUS); match(LBR_T, NO_ATTR); opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
	<opt_statements>  ->
		<statements> | e

	FIRST set: { AVID_T, SVID_T, KW_T(but not PLATYPUS, ELSE, THEN, REPEAT), e }
*/
void opt_statements(void){
	switch (lookahead.code){
		case AVID_T:
		case SVID_T: statements(); break;
		case KW_T:
			/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
			statements_p()*/
			if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE
					&& lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != REPEAT){
				statements();
				break;
			}
			break;
		default: /*empty string - optional statements*/
			gen_incode("PLATY: Opt_statements parsed");
			return;
	}
}

/*
	<statements>  ->
		<statement><statements_p>

	FIRST set: { AVID_T, SVID_T, KW_T(but not PLATYPUS, ELSE, THEN, REPEAT) }
*/
void statements(void){
	statement(); statements_p();
}

/*
	<statements_p>  ->
		<statement><statements_p> | e

	FIRST set: { AVID_T, SVID_T, KW_T(but not PLATYPUS, ELSE, THEN, REPEAT), e}
*/
void statements_p(void){
	switch (lookahead.code){
		case AVID_T:
		case SVID_T: statement(); statements_p(); return;
		case KW_T:
			/* check for PLATYPUS, ELSE, THEN, REPEAT */
			if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE
					&& lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != REPEAT) {
				statement(); /* USING, IF, INPUT, OUTPUT */
				statements_p(); 
				return;
			} else {
				return; 
			} 
	}
}

/*
	<statement>  ->
		<assignment_statement>
		| <iteration_statement>
		| <selection_statement>
		| <input_statement>
		| <output_statement>

	FIRST set: { AVID_T, SVID_T, KW_T(but not PLATYPUS, ELSE, THEN, REPEAT)}
*/
void statement(void){
	switch (lookahead.code){
		case AVID_T:
		case SVID_T: assignment_statement(); break;
		case KW_T:
			switch (lookahead.attribute.get_int){
				case USING: iteration_statement(); break;
				case IF: selection_statement(); break;
				case INPUT: input_statement(); break;
				case OUTPUT: output_statement(); break;
				default: syn_printe(); return; /* no match */
			}
			break;
		default: /* no match */
			syn_printe();
	}
}

/*
	<assignment_statement> ->
		<assignment_expression>

	FIRST set: { AVID_T, SVID_T }
*/
void assignment_statement(void){
	assignment_expression(); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment_statement parsed");
}

/*
	<iteration statement> ->
		USING  (<assignment_expression> , <conditional_expression> , <assignment_expression>)
		REPEAT {
			<opt_statements>
		};

	FIRST set: { KW_T(but only USING) }
*/
void iteration_statement(void){
	match(KW_T, USING); 

	match(LPR_T, NO_ATTR); assignment_expression(); match(COM_T, NO_ATTR); 
	conditional_expression(); match(COM_T, NO_ATTR); assignment_expression();

	match(RPR_T, NO_ATTR); match(KW_T, REPEAT); match(LBR_T, NO_ATTR);
	opt_statements(); match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Iteration_statement parsed");
}

/*
	<input_statement> ->
		INPUT (<variable_list>);

	FIRST set: { KW_T(but only INPUT) }
*/
void input_statement(void){
	match(KW_T, INPUT); match(LPR_T, NO_ATTR); variable_list();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*
	<output statement> ->
		OUTPUT (<output_list>); 

	FIRST set: { KW_T(but only OUTPUT) }
*/
void output_statement(void){
	match(KW_T, OUTPUT); match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output_statement parsed");
}

/*
	<selection_statement> ->
		IF (<conditional_expression>)  THEN
			<opt_statements>
		ELSE { <opt_statements> } ;

	FIRST set: { KW_T(but only IF) }
*/
void selection_statement(void){
	match(KW_T, IF); match(LPR_T, NO_ATTR);
	conditional_expression(); /* condition */
	match(RPR_T, NO_ATTR);match(KW_T, THEN);
	opt_statements(); /* if true */
	match(KW_T, ELSE); match(LBR_T, NO_ATTR);
	opt_statements(); /* else */
	match(RBR_T, NO_ATTR); match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection_statement parsed");
}

/*
	<output_list> ->
		<opt_variable_list> | STR_T

	FIRST set: { AVID_T, SVID_T, STR_T, E }
*/
void output_list(void){
	if (lookahead.code == STR_T){
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: STR_T Output_list parsed");
	} else {
		opt_variable_list();
	}
}

/*
	<opt_variable_list> ->
		<variable_list> | E

	FIRST set: { AVID_T, SVID_T, E }
*/
void opt_variable_list(void){
	(lookahead.code == AVID_T || lookahead.code == SVID_T) ? variable_list() : gen_incode("PLATY: Empty Opt_variable_list parsed");
}

/*
	<variable_list> ->
		<variable_identifier><variable_list_p>

	FIRST set: { AVID_T, SVID_T }
*/
void variable_list(void){
	variable_identifier(); variable_list_p();
	gen_incode("PLATY: Variable_list parsed.");
}

/*
	<variable_identifier> ->
		AVID_T | SVID_T

	FIRST set: { AVID_T, SVID_T }
*/
void variable_identifier(void){
	if (lookahead.code == AVID_T || lookahead.code == SVID_T) {
		match(lookahead.code, NO_ATTR);
		gen_incode("PLATY: Variable_identifier parsed.");
		return; /* return so we skip the syn_printe() call*/
	}
	syn_printe(); /* no match */
}

/*
	<variable_list_p> ->
		COM_T <variable_identifier><variable_list_p> | E

	FIRST set: { COM_T, E }
*/
void variable_list_p(void){
	if (lookahead.code == COM_T){
		match(COM_T, NO_ATTR);
		variable_identifier(); variable_list_p();
		gen_incode("PLATY: Variable_list parsed.");
	}
}

/*
	<assignment_expression> ->
		AVID = <arithmetic_expression>
		| SVID = <string_expression>

	FIRST set: { AVID_T, SVID_T }
*/
void assignment_expression(void){
	switch (lookahead.code){
		case AVID_T:
			match(AVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); /* AVID = ... */
			arithmetic_expression();
			break;
		case SVID_T:
			match(SVID_T, NO_ATTR); match(ASS_OP_T, NO_ATTR); /* SVID = ... */
			string_expression();
			break;
		default: /* no match */
			syn_printe();
			return;
	}
	gen_incode("PLATY: Assignment_expression parsed");
}

/*
	<arithmetic_expression> - >
		<unary_arithmetic_expression>
		| <additive_arithmetic_expression>


	FIRST set: { AVID_T, FPL_T, INL_T, ART_OP_T(but not MULT, DIV), LPR_T }
*/
void arithmetic_expression(void){
	switch (lookahead.code){
		case ART_OP_T:
			if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV){
				syn_printe(); return; /* beginning of expression, '*' and '/' cannot start an expression so fail*/
			}
			unary_arithmetic_expression();
			break;
		case FPL_T:
		case INL_T:
		case AVID_T:
		case LPR_T:
			additive_arithmetic_expression();
			break;
		default: /* no match */
			syn_printe();
			return;
	}
	gen_incode("PLATY: Arithmetic_expression parsed");
}

/*
	<string_expression> ->
		<primary_string_expression><string_expression_p>

	FIRST set: { SVID_T, STR_T }
*/
void string_expression(void){
	primary_string_expression(); string_expression_p();
	gen_incode("PLATY: String_expression parsed");
}

/*
	<conditional_expression> ->
		<logical_or_expression>

	FIRST set: { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void conditional_expression(void){
	logical_or_expression(); gen_incode("PLATY: Conditional_expression parsed");
}

/*
	<relational_expression> - >
		<primary_a_relational_expression> <relational_operator> <primary_a_relational_expression>
		| <primary_s_relational_expression> <relational_operator> <primary_s_relational_expression>


	FIRST set: { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*/
void relational_expression(void){
	switch (lookahead.code){
		case AVID_T: /* Arithmetic expressions */
		case FPL_T:
		case INL_T:
			primary_a_relational_expression(); relational_operator(); primary_a_relational_expression(); 
			break;
		case SVID_T: /* String expressions */
		case STR_T:
			primary_s_relational_expression(); relational_operator(); primary_s_relational_expression();
			break;
		default:
			syn_printe(); /* no match */
	}
	gen_incode("PLATY: Relational_expression parsed");
}

/*
	<arithmetic_expression> - >
		MINUS <primary_arithmetic_expression>
		| PLUS <primary_arithmetic_expression>


	FIRST set: { ART_OP_T(but not MULT, DIV) }
*/
void unary_arithmetic_expression(void){
	if (lookahead.code == ART_OP_T){
		if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV){
			syn_printe(); return; /* beginning of expression, '*' and '/' cannot start an expression so fail*/
		}
		match(ART_OP_T, lookahead.attribute.arr_op);
		primary_arithmetic_expression();
		gen_incode("PLATY: Unary_arithmetic_expression parsed");
	} else {
		syn_printe(); /* no match */
	}
}

/*
	<additive_arithmetic_expression> ->
		<multiplicative_arithmetic_expression><additive_arithmetic_expression_p>

	FIRST set: { AVID_T, FPL_T, INL_T, LPR_T }
*/
void additive_arithmetic_expression(void){
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_p();
}

/*
	<additive_arithmetic_expression_p> ->
		(+|-) <multiplicative_arithmetic_expression><additive_arithmetic_expression_p> | E

	FIRST set: { +, -, E }
*/
void additive_arithmetic_expression_p(void){
	if (lookahead.code == ART_OP_T){
		if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV){
			return; /* just return */
		}
		match(ART_OP_T, lookahead.attribute.arr_op);
		multiplicative_arithmetic_expression();
		additive_arithmetic_expression_p();
		gen_incode("PLATY: Additive_arithmetic_expression parsed");
	}
}

/*
	<multiplicative_arithmetic_expression> ->
		<primary_arithmetic_expression><multiplicative_arithmetic_expression_p>

	FIRST set: { AVID_T, FPL_T, INL_T, LPR_T }
*/
void multiplicative_arithmetic_expression(void){
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_p();

}

/*
	<multiplicative_arithmetic_expression_p> ->
		(*|/) <primary_arithmetic_expression><multiplicative_arithmetic_expression_p> | E

	FIRST set: { *, /, E }
*/
void multiplicative_arithmetic_expression_p(void){
	if (lookahead.code == ART_OP_T){
		if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS){
			return; /* just return */
		}
		match(ART_OP_T, lookahead.attribute.arr_op);
		primary_arithmetic_expression();
		multiplicative_arithmetic_expression_p();
		gen_incode("PLATY: Multiplicative_arithmetic_expression parsed");
	}
}

/*
	<primary_arithmetic_expression> ->
		AVID_T | FPL_T | INL_T | (<arithmetic_expression>)

	FIRST set: { AVID_T, FPL_T, INL_T, LPR_T }
*/
void primary_arithmetic_expression(void){
	switch (lookahead.code){
		case AVID_T: /* Arithmetic expressions */
		case FPL_T:
		case INL_T: 
			match(lookahead.code, lookahead.attribute.arr_op);
			break;
		case LPR_T: 
			match(lookahead.code, lookahead.attribute.arr_op); 
			arithmetic_expression();
			match(RPR_T, NO_ATTR);
			break;
		default:
			syn_printe(); /* no match */
			return;
	}
	gen_incode("PLATY: Primary_arithmetic_expression parsed");
}

/*
	<primary_string_expression> ->
		SVID_T | STR_T

	FIRST set: { SVID_T, STR_T }
*/
void primary_string_expression(void){
	if (lookahead.code == STR_T || lookahead.code == SVID_T){
		match(lookahead.code, lookahead.attribute.arr_op);
	} else {
		syn_printe();
	}
	gen_incode("PLATY: Primary_string_expression parsed");
}

/*
	<string_expression_p> ->
		<primary_string_expression> # <string_expression_p> | E

	FIRST set: { SVID_T, STR_T, # }
*/
void string_expression_p(void){
	if (lookahead.code == SCC_OP_T){
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_p();
		gen_incode("PLATY: String_expression_p parsed");
	}
}

/*
	<logical_or_expression> ->
		<logical_and_expression> <logical_or_expression_p>

	FIRST set: { AVID_T, FPL_T, INL_T, SVID_T, STR_T, E }
*/
void logical_or_expression(void){
	logical_and_expression();
	logical_or_expression_p();
}

/*
	<logical_or_expression_p> ->
		OR <logical_and_expression> <logical_or_expression_p> | E

	FIRST set: { LOG_OP_T(but not AND), E }
*/
void logical_or_expression_p(void){
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op != AND){
		match(LOG_OP_T, OR);
		logical_and_expression();
		logical_or_expression_p();
		gen_incode("PLATY: Logical_or_expression parsed");
	}
}

/*
	<logical_and_expression> ->
		<relational_expression> <logical_and_expression_p>

	FIRST set: { AVID_T, FPL_T, INL_T, SVID_T, STR_T, E }
*/
void logical_and_expression(void){
	relational_expression(); 
	logical_and_expression_p();
}

/*
	<logical_and_expression_p> ->
		AND <relational_expression> <logical_and_expression_p> | E

	FIRST set: { LOG_OP_T(but not OR), E }
*/
void logical_and_expression_p(void){
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op != OR){
		match(LOG_OP_T, AND);
		relational_expression();
		logical_and_expression_p();
		gen_incode("PLATY: Logical_and_expression parsed");
	}
}

/*
	<primary_a_relational_expression> ->
		AVID_T
		| FPL_T
		| INL_T

	FIRST set: { AVID_T, FPL_T, INL_T }
*/
void primary_a_relational_expression(void){
	switch (lookahead.code){
		case AVID_T: 
		case FPL_T:
		case INL_T:
			match(lookahead.code, lookahead.attribute.arr_op);
			break;
		default:
			syn_printe(); /* no match */
	}
	gen_incode("PLATY: Primary_a_relational_expression parsed");
}

/*
	<primary_s_relational_expression> ->
		<primary_string_expression>

	FIRST set: { SVID_T, STR_T }
*/
void primary_s_relational_expression(void){
	primary_string_expression();
	gen_incode("PLATY: Primary_s_relational_expression parsed");
}

/*
	<relational_operator> ->
		==
		| <>
		| >
		| <

	FIRST set: { REL_OP_T(EQ, NE, GT, LT) }
*/
void relational_operator(void){
	if (lookahead.code == REL_OP_T){
		switch (lookahead.attribute.rel_op){
			case EQ: 
			case NE:
			case GT:
			case LT: 
				match(REL_OP_T, lookahead.attribute.rel_op); 
				gen_incode("PLATY: Relational_operator parsed");
				break;
			default:
				syn_printe(); /* unknown relational operator */
				return;
		}
	}
	else {
		syn_printe(); /* no match */
	}
}

