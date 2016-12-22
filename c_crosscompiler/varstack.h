/* File name:	varstack.h
*  Author:		Lucas Estienne
*/

#ifndef  VARSTACK_H_
#define  VARSTACK_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/* constants */
#define ST_DEF_SIZE 100

/* flags */


/* implementation */
typedef struct VarStack{
	int var_ids[ST_DEF_SIZE];
	int top;
} VarStack;

/* function prototypes */
VarStack * vs_create();
int vs_empty(VarStack * var_stack);
int vs_full(VarStack * var_stack);
int vs_peek(VarStack * var_stack);
int vs_pop(VarStack * var_stack);
int vs_push(VarStack * var_stack, int var_id);
int vs_size(VarStack * var_stack);
int vs_contains(VarStack * var_stack, int var_id);
int vs_at(VarStack * var_stack, int i);
int vs_remove(VarStack * var_stack, int i);

#endif
