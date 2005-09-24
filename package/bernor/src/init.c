
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include "bernor.h"

R_NativePrimitiveArgType bernorTypes[17] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, REALSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP, REALSXP, REALSXP, INTSXP};
R_NativeArgStyle bernorStyles[17] = {R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_OUT, R_ARG_OUT, R_ARG_OUT, R_ARG_IN};

R_NativePrimitiveArgType bnmargTypes[22] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP,
    INTSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP, REALSXP, REALSXP, INTSXP,
    INTSXP, INTSXP, REALSXP, REALSXP, LGLSXP};
R_NativeArgStyle bnmargStyles[22] = {R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN,
    R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_OUT, R_ARG_OUT, R_ARG_OUT, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_OUT, R_ARG_IN};

R_NativePrimitiveArgType bnloglTypes[22] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP, REALSXP, REALSXP, INTSXP,
    INTSXP, INTSXP, REALSXP, REALSXP};
R_NativeArgStyle bnloglStyles[22] = {R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_OUT, R_ARG_OUT, R_ARG_OUT, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_OUT};

R_NativePrimitiveArgType bnbigwTypes[19] = {INTSXP, INTSXP, INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, INTSXP,
    INTSXP, REALSXP, REALSXP,
    REALSXP, REALSXP, INTSXP,
    REALSXP,
    INTSXP, INTSXP, REALSXP,
    INTSXP};
R_NativeArgStyle bnbigwStyles[19] = {R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_OUT,
    R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_IN};

R_NativePrimitiveArgType dmissTypes[5] = {REALSXP, INTSXP, INTSXP, REALSXP,
    REALSXP};
R_NativeArgStyle dmissStyles[5] = {R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_IN,
    R_ARG_OUT};

R_NativePrimitiveArgType rmissTypes[4] = {INTSXP, INTSXP, REALSXP, REALSXP};
R_NativeArgStyle rmissStyles[4] = {R_ARG_IN, R_ARG_IN, R_ARG_IN, R_ARG_OUT};

static R_CMethodDef cMethods[] = {
   {"bernor", (DL_FUNC) &bernor, 1, bernorTypes, bernorStyles},
   {"bnmarg", (DL_FUNC) &bnmarg, 2, bnmargTypes, bnmargStyles},
   {"bnlogl", (DL_FUNC) &bnlogl, 2, bnloglTypes, bnloglStyles},
   {"bnbigw", (DL_FUNC) &bnbigw, 2, bnbigwTypes, bnbigwStyles},
   {"dmiss", (DL_FUNC) &dmiss, 2, dmissTypes, dmissStyles},
   {"rmiss", (DL_FUNC) &rmiss, 2, rmissTypes, rmissStyles},
   {NULL, NULL, 0, NULL, NULL}
};

static R_CallMethodDef callMethods[]  = {
    {NULL, NULL, 0}
};

void R_init_mutate(DllInfo *info)
{
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
}

