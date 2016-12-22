/* File name:	buffer.h
*  Author:		Lucas Estienne
*  Purpose:		This contains macro definitions, as well as declarations and prototypes necessary for
*				the Buffer component of the Compiler.
*/
#ifndef BUFFER_H_
#  define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#  include <stdio.h>  /* standard input/output */
#  include <malloc.h> /* for dynamic memory allocation*/
#  include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#  define R_FAIL1	-1       /* fail return value */
#  define R_FAIL2	-2       /* fail return value */
#  define R_FAIL_INCFACTOR 256 /* value to return in b_incfactor on failure*/
#  define R_SUCCESS0 0		 /* success return value */
#  define LOAD_FAIL -2       /* load fail error */
#  define SET_R_FLAG 1       /* realloc flag set value changed*/
#  define SET_R_FLAG0 0		 /* realloc flag set value unchanged*/

#  ifndef B_FULL
#  define B_FULL
#    define b_isfull(pBD) \
		( \
			(!pBD || (!pBD->addc_offset && pBD->addc_offset != 0)) ? R_FAIL1 : /* check that offset exists */\
			(!pBD->capacity) ? R_FAIL1 : /* check that capacity exists and is not 0 */\
			((short)(pBD->addc_offset*sizeof(char) + sizeof(char)) > pBD->capacity) ? 1 : 0 /* 1 true 0 false */\
		) 
#  endif
#  ifndef B_EMPTY
#  define B_EMPTY
#    define b_isempty(pBD) \
		( \
			(!pBD || (!pBD->addc_offset && pBD->addc_offset != 0)) ? R_FAIL1 : /* check that offset exists */\
			(pBD->addc_offset == 0) ? 1 : 0 /* 1 true 0 false */\
		) 
#  endif

/* Uncomment any of the below line to use the function in the compiled code instead of the macro. */

/*#  undef B_FULL*/ 
/*#  undef B_EMPTY*/

/* If any of the above line is commented the macro will be used in the compiled code instead of the function. */

/* user data type declarations */
typedef struct BufferDescriptor {
    char *cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short mark_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  r_flag;     /* reallocation flag */
    char  mode;       /* operational mode indicator*/
    int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/


/* function declarations */
Buffer * b_create(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_reset(Buffer * const pBD);
void b_free(Buffer * const pBD);
short b_size(Buffer * const pBD);
short b_capacity(Buffer * const pBD);
short b_setmark(Buffer * const pBD, short mark);
short b_mark(Buffer * const pBD);
int b_mode(Buffer * const pBD);
size_t  b_incfactor(Buffer * const pBD);
int b_load(FILE * const fi, Buffer * const pBD);
int b_eob(Buffer * const pBD);
char b_getc(Buffer * const pBD);
int b_print(Buffer  * const pBD);
Buffer *b_pack(Buffer * const pBD);
char b_rflag(Buffer * const pBD);
short b_retract(Buffer * const pBD);
short b_retract_to_mark(Buffer * const pBD);
short b_getcoffset(Buffer * const pBD);
char * b_cbhead(Buffer * const pBD);

#  ifndef B_FULL /* if user chooses to not use B_FULL macro, declare prototype for b_isfull function and undefine the macro */
#    ifdef b_isfull
#      undef b_isfull
#    endif
int b_isfull(Buffer * const pBD);
#  endif
#  ifndef B_EMPTY /* if user chooses to not use B_FULL macro, declare prototype for b_isempty function and undefine the macro */
#    ifdef b_isempty
#      undef b_isempty
#    endif
int b_isempty(Buffer * const pBD);
#  endif

#endif

