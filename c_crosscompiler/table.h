/* File name:	table.h
*  Author:		Lucas Estienne
*  Purpose:		Transition Table and function declarations necessary for the scanner implementation.
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or only one of the following constants: 255, 0xFF , EOF
 */

/*  Single-lexeme tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
 *       space
 *  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', # ,
 *  .AND., .OR. , SEOF, 'wrong symbol',
 */

#define SEOF_N 255
#define SEOF '\0'
#define SEOF_H 0XFF

#define ES 3 /* Error state */
#define IS -1 /* Invalid state */

#define PVAR_FLT 0
#define PVAR_INT 1
#define PVAR_STR 2

#define PVAL_ZERO 4
#define PVAL_OCTAL 5
#define PVAL_DEC 6
#define PVAL_DOT 7
#define PVAL_DEFAULT 8

/* State transition table definition */


#define TABLE_COLUMNS 9
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
			   /*{  0,  1,  2,  3,  4,  5,  6,  7,  8 } */
	/* State 0 */{  1,  2, ES, IS,  7,  6,  6, IS, IS }, /* DEFAULT STATE */
	/* State 1 */{  1,  1,  4, IS,  1,  1,  1,  5,  5 }, /* AVID/SVID */
	/* State 2 */{  2,  2,  4, IS , 2,  2,  2,  5,  5 }, /* POSSIBLE INT AVID */
	/* State 3 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS }, /* ES */
	/* State 4 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS },
	/* State 5 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS }, 
	/* State 6 */{ ES, ES,  8, IS,  6,  6,  6, 11,  8 }, /* [1-9] first symbol */
	/* State 7 */{ ES, ES,  8, IS, ES,  9,  3, 11,  8 }, /* '0' first symbol */
	/* State 8 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS }, /* IS */
	/* State 9 */{ ES, ES, 12, IS,  9,  9, ES, ES, 12 },
   /* State 10 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS },
   /* State 11 */{ 13, 13, 13, IS, 11, 11, 11, 13, 13 }, /* after decimal dot */
   /* State 12 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS }, /* IS */
   /* State 13 */{ IS, IS, IS, IS, IS, IS, IS, IS, IS } /* IS */
};

/* Accepting state table definition */
#define ASWR     1  /* accepting state with retract */
#define ASNR     2  /* accepting state with no retract */
#define NOAS     3  /* not accepting state */

int as_table[ ] = {
	/* State 0 */ NOAS, /* */
	/* State 1 */ NOAS, /* */
	/* State 2 */ NOAS, /* */
	/* State 3 */ ASNR, /* */
	/* State 4 */ ASNR, /* */
	/* State 5 */ ASWR, /* */
	/* State 6 */ NOAS, /* */
	/* State 7 */ NOAS, /* */
	/* State 8 */  ASWR, /* */
	/* State 9 */  NOAS, /* */
	/* State 10 */ ASNR, /* */
	/* State 11 */ NOAS, /* */
	/* State 12 */ ASWR, /* */
	/* State 13 */ ASWR /* */
};

/* Accepting action function declarations */


typedef Token (*PTR_AAF)(char *lexeme);

Token aa_func03(char *lexeme); /* ES */
Token aa_func04(char *lexeme); /* SVID */
Token aa_func05(char *lexeme); /* KW/AVID */
Token aa_func08(char *lexeme); /* DIL */
Token aa_func12(char *lexeme); /* OIL */
Token aa_func13(char *lexeme); /* FPL */

/* Accepting function (action) callback table (array) definition */

PTR_AAF aa_table[ ] ={
	/* State 0 */  NULL,
	/* State 1 */  NULL,
	/* State 2 */  NULL,
	/* State 3 */  aa_func03,
	/* State 4 */  aa_func04,
	/* State 5 */  aa_func05,
	/* State 6 */  NULL,
	/* State 7 */  NULL,
	/* State 8 */  aa_func08,
	/* State 9 */  NULL,
	/* State 10 */ NULL,
	/* State 11 */ NULL,
	/* State 12 */ aa_func12,
	/* State 13 */ aa_func13
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table []= {
                      "ELSE",
                      "IF",
                      "INPUT",
                      "OUTPUT",
                      "PLATYPUS",
                      "REPEAT",
                      "THEN",
                      "USING"   
                     };

#endif
                     