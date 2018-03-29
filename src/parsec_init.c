#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void bd(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void bd_simp(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void linzeta(void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"bd",      (DL_FUNC) &bd,      16},
    {"bd_simp", (DL_FUNC) &bd_simp,  9},
    {"linzeta", (DL_FUNC) &linzeta,  3},
    {NULL, NULL, 0}
};

void R_init_parsec(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
