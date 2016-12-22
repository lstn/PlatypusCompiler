/* File name:	scanner.c
*  Author:		Lucas Estienne
*  Purpose:		Implements the functions and logic required for the Scanner component of the compiler.
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
#include "token.h"
#include "table.h"
#include "stable.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG


/* Global objects - variables */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */
extern STD sym_table;

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definition are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup function */
static int discard_line(Buffer * sc_buf); /* discard end of line after comment*/
static void validate_symbol_install(); /* error handling if trying to add a symbol to a full table */

/* Purpose:			 Performs the init for the scanner buffer by resetting it and
*					 the string literal buffer. Sets line to 1
*  Author:			 Svillen Ranev
*  Parameters:		 [sc_buf: (Buffer *)]
*  Return value:	 [ (int, values: {EXIT_SUCCESS}) ]
*/
int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/* Purpose:			 Matches a lexeme with a pattern and return next token
*  Parameters:		 [sc_cur: (Buffer*)]
*  Return value:	 [ (Token}) ]
*/
Token mlwpar_next_token(Buffer * sc_buf)
{
	Token t; /* token to return after recognition */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input buffer */
	short lexend;    /*end   offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */
        
        
		/*DECLARE YOUR VARIABLES HERE IF NEEDED */
	int continue_loop = 1; /* whether to keep running the while loop */
	unsigned int j; /* loop var */
	int i; /* loop var */
	unsigned char nextc; /* input symbol */

	char and_op_check[] = "ND."; /* string to check for when looking for the .AND. operator */
	char or_op_check[] = "R."; /* string to check for when looking for the .OR. operator */
        
                
	while (continue_loop){ /* loops until token returns */
        c = b_getc(sc_buf);
#ifdef DEBUG
		printf("** Char to scan: %c\n", c);
#endif
		switch (c){
			case ' ': /* whitespace */
			case '\t':
			case '\v':
			case '\f':
				break;
			case '\r': 
				nextc = b_getc(sc_buf);
				if (nextc != '\n'){
					b_retract(sc_buf);
				}

			case '\n': /* newlines */
				++line; /* only increment count, don't break loop */
				break;
			case SEOF_N: /* 255 */
			case SEOF: /* null term / SEOF */
				t.code = SEOF_T;
				continue_loop = 0; break;
			case ';': /* EOS */
				t.code = EOS_T;
				continue_loop = 0; break;
			case '=': /* also == */
			{
				if (b_getcoffset(sc_buf) < b_size(sc_buf)){
					nextc = b_getc(sc_buf);
					if (nextc == '='){
						t.code = REL_OP_T;
						t.attribute.rel_op = EQ;
						continue_loop = 0; break;
					}
					b_retract(sc_buf); /* go back 1, next char was not '=' */
				}
				t.code = ASS_OP_T;
				continue_loop = 0; break;
			}
			case '!': /*!<*/
			{
				lexstart = b_getcoffset(sc_buf); /* set lexstart and mark to current getcoffset */
				b_setmark(sc_buf, lexstart);
				if (lexstart < b_size(sc_buf)){
					nextc = b_getc(sc_buf);
					if (nextc == '<'){ /* comment syntax found */
						discard_line(sc_buf);
						break;
					}
					b_retract(sc_buf); /* go back 1, next char was not '<' */
				}

				/* count not find comment, set token to error */
				t.code = ERR_T;
				b_retract_to_mark(sc_buf);
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = b_getc(sc_buf);
				t.attribute.err_lex[2] = '\0';
				discard_line(sc_buf); /* rest of the line is garbage, get rid of it */
				continue_loop = 0; break;
			}
			case '<': /* also <> */
			{
				if (b_getcoffset(sc_buf) < b_size(sc_buf)){
					nextc = b_getc(sc_buf);
					if (nextc == '>'){ /* found not equal <> operator */
						t.code = REL_OP_T;
						t.attribute.rel_op = NE; 
						continue_loop = 0; break;
					}
					b_retract(sc_buf); /* go back 1, next char was not '>' */
				}
				/* found '<' operator */
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				continue_loop = 0; break;
			}
			case '>': /* found '>' operator */
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				continue_loop = 0; break;
			case '+': /* found '+' operator */
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				continue_loop = 0; break;
			case '-': /* found '-' operator */
				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				continue_loop = 0; break;
			case '*': /* found '*' operator */
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				continue_loop = 0; break;
			case '/': /* found '/' operator */
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				continue_loop = 0; break;
			case '.': /* .AND. .OR. */
			{
				lexstart = b_getcoffset(sc_buf); /* set lexstart and mark to current getcoffset */
				b_setmark(sc_buf, lexstart);
				if (b_getcoffset(sc_buf) < b_size(sc_buf)){
					nextc = b_getc(sc_buf);
					if (nextc == 'O'){ /* try to find the .OR. operator*/
						for (j = 1; j < sizeof(or_op_check); j++)
							if (b_getc(sc_buf) != or_op_check[j - 1]) break;
						if (j == 3){ /* found the OR operator */
							t.code = LOG_OP_T;
							t.attribute.log_op = OR;
							continue_loop = 0; break;
						}
					}
					if (nextc == 'A'){ /* try to find the .AND. operator*/
						for (j = 1; j < sizeof(and_op_check); j++)
							if (b_getc(sc_buf) != and_op_check[j - 1]) break;
						if (j == 4){ /* found the AND operator */
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							continue_loop = 0; break;
						}
					}
				}

				/* could not find either operators, error */
				t.code = ERR_T;
				b_retract_to_mark(sc_buf); /* retract to lexstart */
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
				continue_loop = 0; break;
			}
			case '"': /* strings */
			{
				lexstart = b_getcoffset(sc_buf); /* set lexstart and mark to current getcoffset */
				b_setmark(sc_buf, lexstart);
				if (b_setmark(str_LTBL, b_size(str_LTBL)) == R_FAIL1){ /* attempt to set the mark for str_LTBL to its b_size() */
					scerrnum = 1;
					t = aa_table[ES]("RUN TIME ERROR: "); /* if failed to set the mark */
					continue_loop = 0; break;
				}

				while (b_getcoffset(sc_buf) < b_size(sc_buf)){
					nextc = b_getc(sc_buf);
					if (nextc == SEOF_N || nextc == SEOF || nextc == EOF){ /* eof/null term */
						t.code = ERR_T;
						b_retract(sc_buf); /* go back one */
						lexend = b_getcoffset(sc_buf); /* set lexend to current getcoffset */

						/* begin generating error message */
						b_setmark(sc_buf, lexstart - 1); /* set mark to lexstart-1 */
						b_retract_to_mark(sc_buf); /* retract to lexstart-1 */

						for (i = 0; i < (ERR_LEN - 3); i++){
							nextc = b_getc(sc_buf);
							if (nextc == '\0'){
								break;
							}
							t.attribute.err_lex[i] = nextc;
						}
						t.attribute.err_lex[i] = '\0';
						strcat(t.attribute.err_lex, "...");
						/* end error message */
						b_setmark(sc_buf, lexend); 
						b_retract_to_mark(sc_buf); /* reset buffer back to where it was */
						continue_loop = 0; break;
					}
					if (nextc == '"'){ /* found end of string */
						t.code = STR_T;
						break;
					}
					if (nextc == '\n') line++; /* new line */
				}
				if (continue_loop == 0) break;
				lexend = b_getcoffset(sc_buf)-1; /* set lexend to current getcoffset-1 */

				b_retract_to_mark(sc_buf); /* retract to beginning of string */
				while (b_getcoffset(sc_buf) < lexend) b_addc(str_LTBL, b_getc(sc_buf)); /* addc to str_LTBL until we reach lexend */

				b_getc(sc_buf); /* get the extra '"' character out*/

				if (!b_addc(str_LTBL, '\0')){ /* attempt to add null term to str_LTBL */
					scerrnum = 2;
					t = aa_table[ES]("RUN TIME ERROR: "); /* failed */
					continue_loop = 0; break;
				}

				t.attribute.str_offset = b_mark(str_LTBL);
				continue_loop = 0; break;
			}
			case '#': /* string concat */
				t.code = SCC_OP_T;
				continue_loop = 0; break;
			case ',': /* comma */
				t.code = COM_T;
				continue_loop = 0; break;
			case '{': /* left brace */
				t.code = LBR_T;
				continue_loop = 0; break;
			case '}': /* right brace */
				t.code = RBR_T;
				continue_loop = 0; break;
			case '(': /* left paren */
				t.code = LPR_T;
				continue_loop = 0; break;
			case ')': /* right brace */
				t.code = RPR_T;
				continue_loop = 0; break;
			default: /* all other chars */
				if (!isalnum(c)){ /* if not alphanumeric, error */
					t.code = ERR_T;
					t.attribute.err_lex[0] = c;
					t.attribute.err_lex[1] = '\0';
					continue_loop = 0; break;
				}
				lexstart = b_getcoffset(sc_buf) - 1; /* set lexstart and mark to getcoffset-1*/
#ifdef DEBUG
				printf("\t^^^^^^ %d\n", lexstart);
#endif
				b_setmark(sc_buf, lexstart);

				do{ /* FSM 0 */
					state = get_next_state(state, c, &accept); /* FSM 1 */
					if (accept != NOAS) break; /* FSM 3 */
					c = b_getc(sc_buf); /* FSM 2 */
				} while (accept == NOAS); /* FSM 3 */
				lexend = b_getcoffset(sc_buf); /* set lexend to getcoffset after exiting FSM */
#ifdef DEBUG
				printf("\tvvvvvv %d\n", lexend);
				printf("\t-----> %d\n", lexend - lexstart + 2);
#endif
				lex_buf = b_create(lexend - lexstart + 2, 0, 'f'); /* create temporary buffer */
				if (!lex_buf){ /* did we fail to create the buffer? */
					scerrnum = 3;
					t = aa_table[ES]("RUN TIME ERROR: ");
					continue_loop = 0; break;
				}

				if (accept == ASWR) --lexend; /* accept state with retracting, reduce lexend by 1 char */
#ifdef DEBUG
				printf("!22! oc: %d lexend: %d\n", b_getcoffset(sc_buf), lexend);
				printf("!!!! oc: %d lexend: %d\n", b_getcoffset(sc_buf), lexend);
#endif
				b_retract_to_mark(sc_buf);
				while (b_getcoffset(sc_buf) < lexend) b_addc(lex_buf, b_getc(sc_buf)); /* keep adding to temp buffer until we reach lexend */
				if (!b_addc(lex_buf, '\0')){ /* try to null term temporary buffer */
					scerrnum = 4;
					t = aa_table[ES]("RUN TIME ERROR: "); /* failed */
					continue_loop = 0; break;
				}

				t = aa_table[state](b_cbhead(lex_buf)); /* call accepted state function and pass temp buffer cbhead*/

				b_free(lex_buf); /* free up temp buffer memory */
				continue_loop = 0; break;
		}
   } /* end loop */
#ifdef DEBUG
   printf("~~~%d\n", t.code);
#endif
   return t; /* return token*/
}

/* Purpose:			 Checks transition table and returns the next state for the current state/char.
*  Author:			 Svillen Ranev
*  Parameters:		 [state: (int), c: (char), accept: (int*)]
*  Return value:	 [ (int) ]
*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
    assert(next != IS);
#ifdef DEBUG
	if(next == IS){
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

/* Purpose:			 Gets the transition table column for a symbol
*  Parameters:		 [c: (char)]
*  Return value:	 [ (int, values: {PVAR_INT, PVAR_FLT, PVAL_ZERO, PVAL_OCTAL, PVAL_DEC
*										PVAR_STR, PVAL_DOT, PVAL_DEFAULT}) ]
*/
int char_class (char c)
{
		if (isalpha(c))
			return (c == 'i' || c == 'o' || c == 'w' || c == 'd') ? PVAR_INT : PVAR_FLT;
		
		if (isdigit(c))
			return (c == '0') ? PVAL_ZERO : (c < '8') ? PVAL_OCTAL : PVAL_DEC;
		
		return (c == '%') ? PVAR_STR : (c == '.') ? PVAL_DOT : PVAL_DEFAULT;
}

/* Purpose:			 Checks if the lexeme is a keyword or an AVID, and accordingly sets
*				     token fields.
*  Parameters:		 [lexeme: (char[])]
*  Return value:	 [ (Token) ]
*/
Token aa_func05(char lexeme[]){
	Token t;
	int is_kw;
	char avid_type;
#ifdef DEBUG
	printf("*%*%* 05\n");
#endif
	is_kw = iskeyword(lexeme);
	if (is_kw != -1){
		t.code = KW_T;
		t.attribute.kwt_idx = is_kw;
		return t;
	}

	if (strlen(lexeme) > VID_LEN)
		lexeme[VID_LEN] = '\0';

	avid_type = (lexeme[0] == 'i' || lexeme[0] == 'o' || lexeme[0] == 'w' || lexeme[0] == 'd') ? 'I' : 'F';
	t.code = AVID_T;
	t.attribute.vid_offset = st_install(sym_table, lexeme, avid_type, line);
	validate_symbol_install(t.attribute.vid_offset);

	return t;
}

/* Purpose:			 Returns the SVID token equivalent for the lexeme passed.
*  Parameters:		 [lexeme: (char[])]
*  Return value:	 [ (Token) ]
*/
Token aa_func04(char lexeme[]){
	Token t;
#ifdef DEBUG
	printf("*%*%* 04\n");
#endif
	if (strlen(lexeme) > VID_LEN)
		lexeme[VID_LEN] = '\0';
	lexeme[strlen(lexeme) - 1] = '%';
	t.code = SVID_T;
	t.attribute.vid_offset = st_install(sym_table, lexeme, 'S', line);
	validate_symbol_install(t.attribute.vid_offset);

	return t;
}

/* Purpose:			 Returns a floating point token for the lexeme passed. Makes a couple
*					 of checks to make sure the floating point lexeme passed is valid.
*					 Can error and return an error token.
*  Parameters:		 [lexeme: (char[])]
*  Return value:	 [ (Token) ]
*/
Token aa_func13(char lexeme[]){
	Token t;
	float fl = strtof(lexeme, NULL);
	char* check;
	int declen, i, zero_count = 0;
#ifdef DEBUG
	printf("*%*%* 13\n");
#endif
	if (lexeme[0] == '0' && lexeme[1] == '.'){ /* calculate number of 0s after the decimal*/
		check = lexeme + 2;
		declen = strlen(check);
		for (i = 0; i < declen; i++, zero_count++){
			if (check[i] != '0') break;
		}
	}
	if (zero_count > 30 || (fl > 0 && (fl > FLT_MAX || fl < FLT_MIN)) || (fl < 0 && (fl < -FLT_MAX || fl > -FLT_MIN))){ /* checks fl against bounds*/
		t.code = ERR_T;
		sprintf_s(t.attribute.err_lex, ERR_LEN + 1, "%.*s", ERR_LEN, lexeme); /* error text */
		t.attribute.err_lex[ERR_LEN] = '\0';
		return t;
	}
	t.code = FPL_T; /* success */
	t.attribute.flt_value = fl;

	return t;
}

/* Purpose:			 Returns a decimal integer literal token from the passed lexeme.
*					 Checks that it does not exceed PLATYPUS bounds. Can error and returns an error token.
*  Parameters:		 [lexeme: (char[])]
*  Return value:	 [ (Token) ]
*/
Token aa_func08(char lexeme[]){
	Token t;
	int dec = (int)strtol(lexeme, NULL, 10); /* get decimal base 10 value from base10 lexeme string */
#ifdef DEBUG
	printf("*%*%* 08\n");
#endif
	if (dec > SHRT_MAX || dec < SHRT_MIN){ /* is our DIL within bounds? */
		t.code = ERR_T;
		sprintf_s(t.attribute.err_lex, ERR_LEN + 1, "%.*s", ERR_LEN, lexeme); /* error text*/
		t.attribute.err_lex[ERR_LEN] = '\0';
		return t;
	}

	t.code = INL_T; /* success */
	t.attribute.int_value = dec;

	return t;
}

/* Purpose:			 Returns a decimal integer literal token from the passed octal lexeme.
*					 Checks that it does not exceed PLATYPUS bounds. Can error and returns an error token.
*  Parameters:		 [lexeme: (char[])]
*  Return value:	 [ (Token) ]
*/
Token aa_func12(char lexeme[]){
	Token t;
	int dec = (int) strtol(lexeme, NULL, 8); /* get decimal base 10 value from base 8 octal lexeme string */
#ifdef DEBUG
	printf("*%*%* 12\n");
#endif
	if (dec > SHRT_MAX || dec < SHRT_MIN){ /* is our DIL within bounds? */
		t.code = ERR_T;
		sprintf_s(t.attribute.err_lex, ERR_LEN + 1, "%.*s", ERR_LEN, lexeme); /* error text*/
		t.attribute.err_lex[ERR_LEN] = '\0';
		return t;
	}

	t.code = INL_T; /* success */
	t.attribute.int_value = dec;

	return t;
}

/* Purpose:			 Returns an error token built from the passed lexeme.
*  Parameters:		 [lexeme: (char[])]
*  Return value:	 [ (Token) ]
*/
Token aa_func03(char lexeme[]){
	Token t;
#ifdef DEBUG
	printf("*%*%* 03\n");
#endif
	t.code = ERR_T;
	sprintf_s(t.attribute.err_lex, ERR_LEN+1, "%.*s", ERR_LEN, lexeme);
	t.attribute.err_lex[ERR_LEN] = '\0';

	return t;
}

/* Purpose:			 Checks if a lexeme is a PLATYPUS keyword
*  Parameters:		 [kw_lexeme: (char*)]
*  Return value:	 [ (int, values: {-1...n}) ]
*/
int iskeyword(char * kw_lexeme){
	int i;
#ifdef DEBUG
	printf("iskw: %s\n", kw_lexeme);
#endif
	for (i = 0; i < KWT_SIZE; i++){
		if (strcmp(kw_lexeme, kw_table[i]) == 0) {
			return i; /* return current index */
		}
	}
	return -1; /* not found */
}

/* Purpose:			 Discards the rest of a line in a buffer.
*  Parameters:		 [sc_buf: (Buffer*)]
*  Return value:	 [ (int) ]
*/
int discard_line(Buffer * sc_buf){
	unsigned char c;
	while (b_getcoffset(sc_buf) < b_size(sc_buf)){
		c = b_getc(sc_buf);
		if (c == '\n'){
			++line;
			break;
		}
		if (c == '\0'){
			b_retract(sc_buf); /* retract 1 when EOF */
			break;
		}
	}
	return 0;
}

/* Purpose:			 Checks that a vid_offset is valid, otherwise exit
*  Parameters:		 [vid_offset: int]
*  Return value:	 void
*/
void validate_symbol_install(int vid_offset){
	if (vid_offset == -1){
		printf("\nError: The Symbol Table is full - install failed.\n");
		st_store(sym_table);
		exit(1);
	}
}