
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
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

R_NativePrimitiveArgType bnloglTypes[22] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP, REALSXP, REALSXP, INTSXP,
    INTSXP, INTSXP, REALSXP, REALSXP};

R_NativePrimitiveArgType bnbigwTypes[19] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP,
    INTSXP, INTSXP, REALSXP,
    INTSXP};

R_NativePrimitiveArgType dmissTypes[5] = {REALSXP, INTSXP, INTSXP, REALSXP,
    REALSXP};

R_NativePrimitiveArgType rmissTypes[4] = {INTSXP, INTSXP, REALSXP, REALSXP};

static R_CMethodDef cMethods[] = {
   {"bernor", (DL_FUNC) &bernor, 1, bernorTypes},
   {"bnmarg", (DL_FUNC) &bnmarg, 2, bnmargTypes},
   {"bnlogl", (DL_FUNC) &bnlogl, 2, bnloglTypes},
   {"bnbigw", (DL_FUNC) &bnbigw, 2, bnbigwTypes},
   {"dmiss", (DL_FUNC) &dmiss, 2, dmissTypes},
   {"rmiss", (DL_FUNC) &rmiss, 2, rmissTypes},
   {NULL, NULL, 0, NULL}
};

static R_CallMethodDef callMethods[]  = {
    {NULL, NULL, 0}
};

void R_init_mutate(DllInfo *info)
{
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
}

