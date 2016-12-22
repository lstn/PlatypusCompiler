/* File name:	varstack.c
*  Author:		Lucas Estienne, 040 819 959
*  Purpose:		Implements the functions and logic required for the variable stack to crosscompile to C.
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
#include "varstack.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* globals */
extern VarStack * var_stack;

VarStack * vs_create(){
	VarStack* new_vs = malloc(sizeof(VarStack));
	new_vs->top = 0;

	return new_vs;
}

int vs_empty(VarStack * var_stack){
	return vs_size(var_stack) <= -1;
}

int vs_full(VarStack * var_stack){
	return (vs_size(var_stack) == ST_DEF_SIZE);
}

int vs_peek(VarStack * var_stack){
	if (vs_empty(var_stack)){
		return -2;
	}
	else {
		return var_stack->var_ids[var_stack->top];
	}
}

int vs_pop(VarStack * var_stack){
	int item;
	if (vs_empty(var_stack)) {
		item = -2;
	} else {
		item = var_stack->var_ids[var_stack->top];
		var_stack->var_ids[var_stack->top] = -2;
		var_stack->top--;
	}
	return item;
}

int vs_push(VarStack * var_stack, int var_id){
	if (vs_full(var_stack)){
		return -2;
	} else {
		var_stack->top++;
		var_stack->var_ids[var_stack->top] = var_id;
		return var_stack->top;
	}
}

int vs_size(VarStack * var_stack){
	return var_stack->top;
}

int vs_contains(VarStack * var_stack, int var_id){
	int i;
	for (i = 0; i <= var_stack->top; i++){
		if (var_stack->var_ids[i] == var_id)
			return i;
	}
	return -2;
}

int vs_at(VarStack * var_stack, int i){
	if (i <= var_stack->top)
		return var_stack->var_ids[i];
	return -2;
}

int vs_remove(VarStack * var_stack, int i){
	var_stack->var_ids[i] = var_stack->var_ids[var_stack->top];
	vs_pop(var_stack);
	return -2;
}