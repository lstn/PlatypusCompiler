/* File name:	stable.h
*  Author:		Lucas Estienne
*  Purpose:		Symbol Table DB implementation declarations and ST manager prototypes
*/

#ifndef  STABLE_H_
#define  STABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/* constants */
#define RFAIL_INVALID_ST -3
#define RFAIL_ST_FULL -1
#define RFAIL_BUF_MEMCHANGED -2

#define ST_BUF_INIT_MODE 'a'
#define ST_BUF_INIT_CAP 100
#define ST_BUF_INIT_INC 10

#define ST_INIT_OFFSET 0
#define ST_FILENAME "$stable.ste"

#define SYMBOL_NOT_FOUND -1

/* flags */
#define SF_INIT 0x0000
#define SF_DEFAULT 0xFFF8
#define SF_INT_TYPE 0x0004
#define SF_STRING_TYPE 0x0006
#define SF_FLOAT_TYPE 0x0002
#define SF_UPDATE 0x0001
#define SF_TYPEMASK 0xFFF9

/* stdb implementation */
typedef union InitialValue {
	int int_val; /* integer variable initial value */
	float fpl_val; /* floating-point variable initial value */
	int str_offset; /* string variable initial value (offset) */
} InitialValue;
typedef struct SymbolTableVidRecord {
	unsigned short status_field; /* variable record status field*/
	char * plex; /* pointer to lexeme (VID name) in CA */
	int o_line; /* line of first occurrence */
	InitialValue i_value; /* variable initial value */
	size_t reserved; /*reserved for future use*/
}STVR;
typedef struct SymbolTableDescriptor {
	STVR *pstvr; /* pointer to array of STVR */
	int st_size; /* size in number of STVR elements */
	int st_offset; /*offset in number of STVR elements */
	Buffer *plsBD; /* pointer to the lexeme storage buffer descriptor */
} STD;

/* function prototypes */
STD st_create(int st_size);
int st_install(STD sym_table, char *lexeme, char type, int line);
int st_lookup(STD sym_table, char *lexeme);
int st_update_type(STD sym_table, int vid_offset, char v_type);
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value);
char st_get_type(STD sym_table, int vid_offset);
void st_destroy(STD sym_table);
int st_print(STD sym_table);
int st_store(STD sym_table);
int st_sort(STD sym_table, char s_order);


#endif
