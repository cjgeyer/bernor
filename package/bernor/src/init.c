// Modified to be the way CRAN likes it now
// See package cjgeyer/foo/package/fooRegister for example
// Also, of course, read Writing R Extensions

#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "bernor.h"

R_NativePrimitiveArgType bernorTypes[17] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP, REALSXP, REALSXP, INTSXP};

R_NativePrimitiveArgType bnmargTypes[22] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP,
    INTSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP, REALSXP, REALSXP, INTSXP,
    INTSXP, INTSXP, REALSXP, REALSXP, LGLSXP};

R_NativePrimitiveArgType bnloglTypes[23] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP,
    REALSXP, REALSXP, REALSXP, INTSXP,
    INTSXP, INTSXP, REALSXP, REALSXP};

R_NativePrimitiveArgType bnbigwTypes[20] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP,
    REALSXP,
    INTSXP, INTSXP, REALSXP,
    INTSXP};

R_NativePrimitiveArgType dmissTypes[5] = {REALSXP, INTSXP, INTSXP, REALSXP,
    REALSXP};

R_NativePrimitiveArgType rmissTypes[4] = {INTSXP, INTSXP, REALSXP, REALSXP};

R_NativePrimitiveArgType i1missTypes[2] = {INTSXP, INTSXP};

R_NativePrimitiveArgType i2missTypes[4] = {INTSXP, INTSXP, INTSXP, INTSXP};

static R_CMethodDef cMethods[] = {
   {"bernor", (DL_FUNC) &bernor, 17, bernorTypes},
   {"bnmarg", (DL_FUNC) &bnmarg, 22, bnmargTypes},
   {"bnlogl", (DL_FUNC) &bnlogl, 23, bnloglTypes},
   {"bnbigw", (DL_FUNC) &bnbigw, 20, bnbigwTypes},
   {"dmiss", (DL_FUNC) &dmiss, 5, dmissTypes},
   {"rmiss", (DL_FUNC) &rmiss, 4, rmissTypes},
   {"i1miss", (DL_FUNC) &i1miss, 2, i1missTypes},
   {"i2miss", (DL_FUNC) &i2miss, 4, i2missTypes},
   {NULL, NULL, 0, NULL}
};

static R_CallMethodDef callMethods[]  = {
    {NULL, NULL, 0}
};

void attribute_visible R_init_bernor(DllInfo *info)
{
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
}

