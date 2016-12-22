/* File name:	stable.c
*  Author:		Lucas Estienne
*  Purpose:		Implements the symbol table manager
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
#include "stable.h"
#include "buffer.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* "internal" function prototypes */
static void st_setsize(void);
static void st_incoffset(void);

/* get global sym_table from main program */
extern STD sym_table;

/* symbol table manager functions implementations*/

/* Purpose:			 Creates a new empty symbol table.
*  Parameters:		 [st_size: int]
*  Return value:	 STD
*/
STD st_create(int st_size){
	STD new_st;

	new_st.st_size = 0; /* value in case passed size is invalid */
	if (st_size < 1) return new_st; /* check passed size is valid */

	new_st.pstvr = malloc(st_size*sizeof(STVR));
	if (new_st.pstvr == NULL) return new_st;

	new_st.plsBD = b_create(ST_BUF_INIT_CAP, ST_BUF_INIT_INC, ST_BUF_INIT_MODE);
	if (new_st.plsBD == NULL) { 
		/* have to free the pstvr we allocated, or leak */
		free(new_st.pstvr);
		return new_st;
	}

	new_st.st_offset = ST_INIT_OFFSET;


	new_st.st_size = st_size; /* nothing failed, can set st_size to indicate st is valid */
	return new_st;
}

/* Purpose:			 Tries to add a new VID record to the symbol table, 
*					 returns position of existing symbol if it is already in it.
*  Parameters:		 [sym_table: STD, lexeme: char*, type: char, line: int]
*  Return value:	 int
*/
int st_install(STD sym_table, char *lexeme, char type, int line){
	int temp_offset, i, lexlen, rcheck = 0, rlen, roffset;

	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return RFAIL_INVALID_ST;

	lexlen = strlen(lexeme);

	temp_offset = st_lookup(sym_table, lexeme);
	if (temp_offset > -1) {
		return temp_offset; /* symbol already exists*/
	}
	if (sym_table.st_offset >= sym_table.st_size) return RFAIL_ST_FULL; /* table is full */

	sym_table.pstvr[sym_table.st_offset].plex = b_cbhead(sym_table.plsBD) + b_size(sym_table.plsBD);

	b_setmark(sym_table.plsBD, b_size(sym_table.plsBD)); /* recovery mark if we fail */
	for (i = 0; i < lexlen+1; i++){
		b_addc(sym_table.plsBD, lexeme[i]);
		if (b_rflag(sym_table.plsBD)) rcheck = b_rflag(sym_table.plsBD);
	}

	/* while adding to buffer, there's a chance memory moved, so we have to check*/
	if (rcheck){
		for (roffset = 0, i = 0; i < sym_table.st_offset; i++, roffset += ++rlen){
			rlen = strlen(b_cbhead(sym_table.plsBD) + roffset);
			sym_table.pstvr[sym_table.st_offset].plex = b_cbhead(sym_table.plsBD) + roffset;
		}
	}
	if ((b_mark(sym_table.plsBD) + lexlen + 1) != (b_size(sym_table.plsBD))){ /* failed to add */
		sym_table.pstvr[sym_table.st_offset].plex = NULL; /*dangling pointer*/
		b_retract_to_mark(sym_table.plsBD); 
		return R_FAIL1;
	}

	sym_table.pstvr[sym_table.st_offset].o_line = line;
	sym_table.pstvr[sym_table.st_offset].status_field &= SF_INIT;
	sym_table.pstvr[sym_table.st_offset].status_field |= SF_DEFAULT;

	switch (type){
		case 'I':
			sym_table.pstvr[sym_table.st_offset].status_field |= SF_INT_TYPE;
			sym_table.pstvr[sym_table.st_offset].i_value.int_val = 0;
			break;
		case 'F':
			sym_table.pstvr[sym_table.st_offset].status_field |= SF_FLOAT_TYPE;
			sym_table.pstvr[sym_table.st_offset].i_value.fpl_val = 0.0;
			break;
		case 'S':
			sym_table.pstvr[sym_table.st_offset].status_field |= SF_STRING_TYPE;
			sym_table.pstvr[sym_table.st_offset].status_field |= SF_UPDATE;
			sym_table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
			break;
	}

	st_incoffset();

	return sym_table.st_offset;
}

/* Purpose:			 Checks if a symbol is already present in the table.
*  Parameters:		 [sym_table: STD, lexeme: char*]
*  Return value:	 int: -1 if not found, index otherwise
*/
int st_lookup(STD sym_table, char *lexeme){
	int current_offset;

	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return RFAIL_INVALID_ST;

	/* looking up backwards, so start at last entry */
	current_offset = sym_table.st_offset - 1;
	while (current_offset > -1){
		if (!strcmp(sym_table.pstvr[current_offset--].plex, lexeme)) /* todo make sure this works*/
			return current_offset+1;
	}
	return SYMBOL_NOT_FOUND; /* not found */
}

/* Purpose:			 Updates the type of a symbol.
*  Parameters:		 [sym_table: STD, vid_offset: int, v_type: char]
*  Return value:	 int: vid_offset or failure state
*/
int st_update_type(STD sym_table, int vid_offset, char v_type){
	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return RFAIL_INVALID_ST;
	if (vid_offset < 0 || vid_offset > sym_table.st_offset) return R_FAIL2;

	if (sym_table.pstvr[vid_offset].status_field & SF_UPDATE)
		return -1;

	sym_table.pstvr[vid_offset].status_field &= SF_UPDATE;
	switch (v_type){
		case 'I':
			sym_table.pstvr[sym_table.st_offset].status_field |= SF_INT_TYPE;
			break;
		case 'F':
			sym_table.pstvr[sym_table.st_offset].status_field |= SF_FLOAT_TYPE;
			break;
		default:
			return R_FAIL2;
	}

	sym_table.pstvr[vid_offset].status_field |= SF_UPDATE;
	return vid_offset;
}


/* Purpose:			 Updates the value of a symbol.
*  Parameters:		 [sym_table: STD, vid_offset: int, i_value: InitialValue]
*  Return value:	 int: vid_offset or failure state
*/
int st_update_value(STD sym_table, int vid_offset, InitialValue i_value){
	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return RFAIL_INVALID_ST;
	if (vid_offset < 0 || vid_offset > sym_table.st_offset) return R_FAIL2;

	sym_table.pstvr[vid_offset].i_value = i_value;
	return vid_offset;
}

/* Purpose:			 Gets type of symbol at vid_offset of table by performing bitwise operations.
*  Parameters:		 [sym_table: STD, vid_offset: int, i_value: InitialValue]
*  Return value:	 char: S, F, I or failure state
*/
char st_get_type(STD sym_table, int vid_offset){
	unsigned short temp_sf;
	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return RFAIL_INVALID_ST;
	if (vid_offset < 0 || vid_offset > sym_table.st_offset) return R_FAIL2;

	temp_sf = sym_table.pstvr[vid_offset].status_field;
	temp_sf &= ~SF_TYPEMASK; /* hide everything but the type */

	switch (temp_sf){
		case SF_STRING_TYPE:
			return 'S';
		case SF_FLOAT_TYPE:
			return 'F';
		case SF_INT_TYPE:
			return 'I';
		default:
			return R_FAIL1;
	}

}

/* Purpose:			 Frees up memory allocated for symbol table and nulls dangling pointers.
*  Parameters:		 [sym_table: STD]
*  Return value:	 void
*/
void st_destroy(STD sym_table){
	int current_offset;
	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return;

	/* free pstvr */
	current_offset = sym_table.st_offset - 1; /* starting backwards like st_lookup */
	for (; current_offset > -1; current_offset--){
		sym_table.pstvr[current_offset].plex = NULL; /* null dangling pointers */
	}
	free(sym_table.pstvr);
	sym_table.pstvr = NULL;

	/* free sym table buffer */
	b_free(sym_table.plsBD);
	sym_table.plsBD = NULL;

	st_setsize();
}

/* Purpose:			 Prints out the symbol table
*  Parameters:		 [sym_table: STD]
*  Return value:	 int number of entries printed
*/
int st_print(STD sym_table){
	int current_offset;

	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return RFAIL_INVALID_ST;

	printf("\nSymbol Table\n____________\n\nLine Number Variable Identifier\n");
	for (current_offset = 0; current_offset < sym_table.st_offset; current_offset++){
		printf("%2d          %s\n", sym_table.pstvr[current_offset].o_line, sym_table.pstvr[current_offset].plex);
	}

	return current_offset;
}

/* Purpose:			 Stores the symbol table to file
*  Parameters:		 [sym_table: STD]
*  Return value:	 int number of entries saved
*/
int st_store(STD sym_table){
	int current_offset;
	FILE * st_fi;
	char t;

	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return RFAIL_INVALID_ST;
	if ((st_fi = fopen(ST_FILENAME, "wt")) == NULL) return R_FAIL1;

	fprintf(st_fi, "%d ", sym_table.st_size);

	for (current_offset = 0; current_offset < sym_table.st_offset; current_offset++){
		t = st_get_type(sym_table, current_offset);
		switch (t){
			case 'S':
				fprintf(st_fi, "%hX %d %s %d %d ",
					sym_table.pstvr[current_offset].status_field,
					strlen(sym_table.pstvr[current_offset].plex),
					sym_table.pstvr[current_offset].plex,
					sym_table.pstvr[current_offset].o_line,
					sym_table.pstvr[current_offset].i_value.str_offset
					);
				break;
			case 'F':
				fprintf(st_fi, "%hX %d %s %d %.2f ",
					sym_table.pstvr[current_offset].status_field,
					strlen(sym_table.pstvr[current_offset].plex),
					sym_table.pstvr[current_offset].plex,
					sym_table.pstvr[current_offset].o_line,
					sym_table.pstvr[current_offset].i_value.fpl_val
					);
				break;
			case 'I':
				fprintf(st_fi, "%hX %d %s %d %d ",
					sym_table.pstvr[current_offset].status_field,
					strlen(sym_table.pstvr[current_offset].plex),
					sym_table.pstvr[current_offset].plex,
					sym_table.pstvr[current_offset].o_line,
					sym_table.pstvr[current_offset].i_value.int_val
					);
				break;
			default:
				return R_FAIL1;
		}
		/* status_field (in hex format), the length of the lexeme, the lexeme, the line number, and the initial value*/
		
			
	}
	fclose(st_fi);
	printf("\nSymbol Table stored.\n"); /* success */
	return current_offset;
}

int st_sort(STD sym_table, char s_order){
	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return RFAIL_INVALID_ST;

	return 0;
}

/* internal functions */

/* Purpose:			 Sets table size to 0.
*  Parameters:		 
*  Return value:	 void
*/
static void st_setsize(void){
	if (sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return;
	sym_table.st_size = 0;
}

/* Purpose:			 Increments global sym table offset.
*  Parameters:
*  Return value:	 void
*/
static void st_incoffset(void){
	if (!sym_table.st_size || sym_table.pstvr == NULL || sym_table.plsBD == NULL) /* check st is valid */
		return;
	++sym_table.st_offset;
}