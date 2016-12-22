/* File name:	buffer.c
*  Author:		Lucas Estienne
*  Purpose:		This implements the functions required for the Buffer component of the Compiler.
*/
#include "buffer.h"


/* Purpose:			 Creates a new buffer in memory using the parameters passed. Initializes
*					 the Buffer's other members with default values. Also derives the Buffer's
*					 operating mode from passed char. Returns NULL if a runtime error occurs.
*  Parameters:		 [ init_capacity: (short), inc_factor: (char, values: {range(0, 256)}), o_mode: (char, values: {'f', 'a', 'm'}) ]
*  Return value:	 [ (Buffer *), (NULL) ]
*/
Buffer * b_create(short init_capacity, char inc_factor, char o_mode){
	Buffer *pBuf;
	char *cb;

	if (init_capacity < sizeof(char) || init_capacity < 0) return NULL; /* cannot have buffer with no or negative capacity */

	pBuf = (Buffer*)calloc(1, sizeof(Buffer)); /* allocate initial memory for Buffer */
	if (!pBuf) return NULL;
	switch (o_mode){ /* checks for a valid o_mode: f, a or m and assigns proper values to mode and inc_factor */
		case 'f':
			pBuf->mode = 0;
			inc_factor = 0; /* always 0 in f mode */
			break;
		case 'a':
			if (inc_factor > 0 && inc_factor < 256){ /* inc factor must be between 1 and 255 inclusive */
				pBuf->mode = 1;
			}
			else {
				return NULL; 
			}
			break;
		case 'm':
			if (inc_factor > 0 && inc_factor < 101){ /* inc factor must be between 1 and 100 inclusive */
				pBuf->mode = -1;
			}
			else {
				return NULL;
			}
			break;
		default:
			return NULL;
	}

	cb = (char*)malloc(init_capacity); /* allocate memory for Buffer's character buffer*/
	if (!cb) return NULL;

	pBuf->cb_head = cb;
	pBuf->capacity = init_capacity;
	pBuf->inc_factor = inc_factor;
	pBuf->addc_offset = 0;
	pBuf->getc_offset = 0;
	pBuf->mark_offset = 0;
	pBuf->eob = 0;
	pBuf->r_flag = 0;

	return pBuf;
}

/* Purpose:			 Adds the symbol passed to the passed Buffer's character array at
*					 the position of the Buffer's `addc_offset`. Returns NULL if a runtime
*					 error occurs.
*  Parameters:		 [pBD: (Buffer * const), symbol: (char)]
*  Return value:	 [ (Buffer *), (NULL) ]
*/
pBuffer b_addc(pBuffer const pBD, char symbol){
	char **temp_loc;
	short avail_space;
	unsigned short new_inc, new_capacity;

	if (!pBD || !pBD->cb_head) return NULL;


	pBD->r_flag = 0;
	if (pBD->addc_offset >= SHRT_MAX) return NULL;
	if (!b_isfull(pBD)){
		b_cbhead(pBD)[pBD->addc_offset] = symbol;
		
		++pBD->addc_offset;
		return pBD; /* No need to continue. */
	}

	switch (pBD->mode){
		case -1:
			avail_space = SHRT_MAX - pBD->capacity;
			new_inc = (unsigned short) (avail_space * (pBD->inc_factor / 100.0));
			if (avail_space > 0 && new_inc == 0 && pBD->inc_factor != 0){
				new_inc++; /* if there is space and the increase was 0, that means it's a truncated 1 in this mode*/
			}
			new_capacity = pBD->capacity + new_inc;
			
			if (new_capacity >= SHRT_MAX) new_capacity = SHRT_MAX;
			break;
		case 1:
			if (pBD->addc_offset >= SHRT_MAX){
				new_capacity = pBD->addc_offset;
			} else{
				new_capacity = pBD->capacity + (pBD->inc_factor * sizeof(char));
			}
			if (new_capacity < 0 || new_capacity >= SHRT_MAX) return NULL;
			break;
		default: /* No need for a case 0 since it would do the same thing as the default case. */
			return NULL;
	}
	if (new_capacity != pBD->capacity) {
		temp_loc = &pBD->cb_head; 
		pBD->cb_head = realloc(pBD->cb_head, new_capacity);
		if (b_cbhead(pBD) == NULL) return NULL; /* realloc failed */
		pBD->r_flag = (temp_loc != &pBD->cb_head) ? SET_R_FLAG : SET_R_FLAG0; /* check if memory loc has changed */

		temp_loc = NULL; /* dangling pointer */
	}
	b_cbhead(pBD)[pBD->addc_offset] = symbol; /* actually add the character to the array */
	++pBD->addc_offset;
	pBD->capacity = new_capacity;
	return pBD;
}

/* Purpose:			 Resets the Buffer's `eob`, `addc_offset`, `getc_offset`, `mark_offset`
*					 and `r_flag` to their default values. Returns R_FAIL1 if a runtime error
*					 occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (int, values: {RFAIL_1, R_SUCCESS0}) ]
*/
int b_reset(Buffer * const pBD){
	if (!pBD) return R_FAIL1;

	pBD->eob = 0;
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->mark_offset = 0;
	pBD->r_flag = 0;

	return R_SUCCESS0;
}

/* Purpose:			 Frees up the memory allocated to the passed Buffer and its character array.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (none) ]
*/
void b_free(Buffer * const pBD){
	if (!pBD) return;
	if (b_cbhead(pBD)){
		free(pBD->cb_head);
		pBD->cb_head = NULL;
	}
	free(pBD);
}

#  ifndef B_FULL /* checks if the user chose to undef the B_FULL macro in order to use the function instead */

/* Purpose:			 Returns whether the passed Buffer character buffer is full. This function is only used if 
*					 B_FULL is not defined, otherwise the b_isfull macro is used. Returns R_FAIL1 if 
*					 a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (int, values: {0, 1}), (int, values: {R_FAIL1}) ]
*/
int b_isfull(Buffer * const pBD){
	if (!pBD || (!pBD->addc_offset && pBD->addc_offset != 0)) return R_FAIL1; /* check that offset exists */
	if (!pBD->capacity) return R_FAIL1; /* capacity must exist and not be 0 */

	return ((short)(pBD->addc_offset*sizeof(char) + sizeof(char)) > pBD->capacity) ? 1 : 0;
}

#  endif

/* Purpose:			 Returns the passed Buffer's current size using `addc_offset`. Returns
*				     R_FAIL1 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (short), (short, values: {R_FAIL1}) ]
*/
short b_size(Buffer * const pBD){
	if (!pBD || (!pBD->addc_offset && pBD->addc_offset != 0)) return R_FAIL1; /* check that offset exists */
	
	return pBD->addc_offset;
}

/* Purpose:			 Returns the passed Buffer's current capacity value. Returns
*				     R_FAIL1 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (short), (short, values: {R_FAIL1}) ]
*/
short b_capacity(Buffer * const pBD){
	if (!pBD || (!pBD->capacity || !pBD->capacity < 0)) return R_FAIL1; /* Capacity cannot be 0, so failing in that case is fine. */

	return pBD->capacity;
}

/* Purpose:			 Sets the passed Buffer's mark offset position to the one passed. Returns
*				     R_FAIL1 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const), mark: (short)]
*  Return value:	 [ (short), (short, values: {R_FAIL1}) ]
*/
short b_setmark(Buffer * const pBD, short mark){
	if (pBD != NULL && mark >= 0 && mark <= pBD->capacity) { /* 0 to addc_offset inclusive */
		pBD->mark_offset = mark;
		return mark;
	}
	return R_FAIL1;
}

/* Purpose:			 Returns the passed Buffer's current mark offset position. Returns
*				     R_FAIL1 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (short), (short, values: {R_FAIL1}) ]
*/
short b_mark(Buffer * const pBD){
	if (!pBD || (!pBD->mode && pBD->mode != 0)) return R_FAIL1; /* check that offset exists */

	return pBD->mark_offset;
}

/* Purpose:			 Returns the passed Buffer's current mode. Returns R_FAIL2 
*					 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (int, values: {-1, 0, 1}), (int, values: {R_FAIL2}) ]
*/
int b_mode(Buffer * const pBD){
	if (!pBD || (!pBD->mode && pBD->mode != 0)) return R_FAIL2; /* check that offset exists */

	return pBD->mode;
}

/* Purpose:			 Returns the passed Buffer's inc_factor. Returns R_FAIL_INCFACTOR
*					 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (size_t), (size_t, values: {R_FAIL_INCFACTOR}) ]
*/
size_t b_incfactor(Buffer * const pBD){
	if (!pBD || (!pBD->inc_factor && pBD->inc_factor != 0) || pBD->inc_factor < 0) return R_FAIL_INCFACTOR;

	return pBD->inc_factor;
}

/* Purpose:			 Loads characters from a file into the passed Buffer and returns
*					 the number of characters added. Returns R_FAIL1 if a runtime error occurs.
*  Parameters:		 [fi: (FILE * const), pBD: (Buffer * const)]
*  Return value:	 [ (int), (int, values: {R_FAIL1, LOAD_FAIL}) ]
*/
int b_load(FILE * const fi, Buffer * const pBD){
	char to_add;
	short added_num = 0;
	if (!fi || !pBD) return R_FAIL1;

	for (;;added_num++){ /* post increment */
		to_add = (char)fgetc(fi);
		if (feof(fi)) /* reached end of file*/
			break;
		if (b_addc(pBD, to_add) == NULL) return LOAD_FAIL;
	}

	return added_num;
}

#  ifndef B_EMPTY /* checks if the user chose to undef the B_FULL macro in order to use the function instead */
/* Purpose:			 Returns whether the passed Buffer character buffer is empty. Returns
*					 R_FAIL1 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (int, values: {0, 1}), (int, values: {R_FAIL1}) ]
*/
int b_isempty(Buffer * const pBD){
	if (!pBD || (!pBD->addc_offset && pBD->addc_offset != 0)) return R_FAIL1; /* check that offset exists */

	return (pBD->addc_offset == 0) ? 1 : 0; /* 1 if empty, 0 if not empty */
}
#  endif

/* Purpose:			 Returns the value of the eob member of the passed Buffer. Returns
*					 R_FAIL1 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (int, values: {0, 1}), (int, values: {R_FAIL1}) ]
*/
int b_eob(Buffer * const pBD){
	if (!pBD || (!pBD->eob && pBD->eob != 0)) return R_FAIL1; /* checks eob exists */

	return pBD->eob;
}

/* Purpose:			 Returns the character in the passed Buffer at the position of the
*					 getc_offset member. Then increments getc_offset by one. Returns 
*					 RFAIL_2 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (char), (char, values: {R_FAIL2, R_FAIL1}) ]
*/
char b_getc(Buffer * const pBD){
	if (!pBD || (!pBD->addc_offset && pBD->addc_offset != 0)) return R_FAIL2; /* check that offset exists */
	if (!pBD->getc_offset && pBD->getc_offset != 0) return R_FAIL2; /* check that offset exists */

	if (pBD->getc_offset == pBD->addc_offset){ /* have we reached the end of the buffer? */
		pBD->eob = 1;
		return R_FAIL1;
	} else {
		pBD->eob = 0;
		++pBD->getc_offset; /* increment before returning */
		return b_cbhead(pBD)[pBD->getc_offset - 1];
	}
}

/* Purpose:			 Prints the contents of the passed Buffer to stdout. Returns NULL if a
*                    runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (int), (int, values: {R_FAIL1}) ]
*/
int b_print(Buffer  * const pBD){
	short temp_getc_offset;
	int eob;
	char symbol;

	if (!pBD || (!pBD->getc_offset && pBD->getc_offset != 0)) return R_FAIL1; /* check that offset exists */
	if (!b_cbhead(pBD)) return R_FAIL1; /* checks character buffer is initialized */

	if (b_isempty(pBD)){
		printf("The buffer is empty.");
	} else {
		temp_getc_offset = pBD->getc_offset;
		pBD->getc_offset = 0;

		do { /* loop until we reach the end of the buffer */
			symbol = b_getc(pBD);
			eob = b_eob(pBD);
			if (!eob) printf("%c", symbol);
		} while (!eob);

		pBD->getc_offset = temp_getc_offset;
	}
	printf("\n");

	return pBD->getc_offset;
}

/* Purpose:			 Packs the passed Buffer by shrinking (or in some cases, expanding) the 
*					 buffer. The new capacity is the current size + 1.  Returns NULL if a
*                    runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (Buffer *), (NULL) ]
*/
Buffer *b_pack(Buffer * const pBD){
	char **temp_loc;

	if (!pBD || !pBD->cb_head) return NULL;

	if (pBD->addc_offset == SHRT_MAX) return NULL;

	pBD->r_flag = 0;
	temp_loc = &pBD->cb_head;

	pBD->cb_head = realloc(pBD->cb_head, (pBD->addc_offset+1)*sizeof(char));
	pBD->capacity = pBD->addc_offset + 1;

	if (!b_cbhead(pBD)) { 
		temp_loc = NULL; /* dangling pointer */
		return NULL; 
	}
	pBD->r_flag = (temp_loc != &pBD->cb_head) ? SET_R_FLAG : SET_R_FLAG0; /* check if memory loc has changed */

	temp_loc = NULL; /* dangling pointer */
	return pBD;
}

/* Purpose:			 Returns the r_flag member of the passed Buffer. Returns R_FAIL1
*					 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (char, values: {0, 1}), (char, values: {R_FAIL1}) ]
*/
char b_rflag(Buffer * const pBD){
	if (!pBD || (!pBD->r_flag && pBD->r_flag != 0)) return R_FAIL1; /* check that offset exists */

	return pBD->r_flag;
}

/* Purpose:			 Retracts the getc_offset member of the passed Buffer by 1 position.
*					 Returns R_FAIL1 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (short), (short, values: {R_FAIL1}) ]
*/
short b_retract(Buffer * const pBD){
	if (!pBD || (!pBD->getc_offset && pBD->getc_offset != 0)) return R_FAIL1; /* check that offset exists */
	if (pBD->getc_offset - 1 < 0) return R_FAIL1; /* can't go negative */

	return --pBD->getc_offset;
}

/* Purpose:			 Retracts the getc_offset member of the passed Buffer to the position of
*                    the mark_offset member of the passed Buffer. Returns R_FAIL1 if a runtime
*					 error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (short), (short, values: {R_FAIL1}) ]
*/
short b_retract_to_mark(Buffer * const pBD){
	if (pBD == NULL) return R_FAIL1;

	pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
}

/* Purpose:			 Returns the getc_offset member of the passed Buffer. Returns R_FAIL1
*					 if a runtime error occurs.
*  Parameters:		 [pBD: (Buffer * const)]
*  Return value:	 [ (short), (short, values: {R_FAIL1}) ]
*/
short b_getcoffset(Buffer * const pBD){
	if (!pBD || (!pBD->getc_offset && pBD->getc_offset != 0)) return R_FAIL1; /* check that offset exists */

	return pBD->getc_offset;
}

/* Purpose:			 Returns the pointer to the beginning of the character buffer array in the passed
*					 Buffer. Returns NULL if a runtime error occurs.
*  Parameters:       [pBD: (Buffer * const)]
*  Return value:	 [ (char *), (NULL) ]
*/
char * b_cbhead(Buffer * const pBD){
	if (pBD && pBD->cb_head) return pBD->cb_head;
	return NULL; /* returns NULL if pBD or cb_head are not defined */
}
