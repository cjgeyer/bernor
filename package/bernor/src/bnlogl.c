
/* observed data log likelihood for normal random effects logistic (Bernoulli)
*  regression
*
*  arguments the same as for bnmarg except for the following
*
*  modified input arguments
*
*      y : int, zero-or-one-valued, (was vector of length leny)
*              now a leny by ncoly matrix in R, a leny * ncoly vector in C,
*              the response
*      deriv : integer, 0, 1, 2, or 3 (value three is new, means return bigv)
*      wayout is absent (allocate space ourselves)
*
*  new input arguments
*
*      ncoly : number of columns of y (see above)
*      weigh : weights for columns of y
*
*  new output arguments
*
*      bigv : double, nparm by nparm matrix in R, nparm * nparm vector in C,
*                 where nparm == lenfix + lenvar, the big V matrix 
*
*/

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include "bernor.h"

void
bnlogl(int *lenyin, int *lenfixin, int *lenranin, int *lenvarin,
    int *ncolxin, int *ncolzin,
    int *nmissin, int *ncolyin,
    int *y, double *theta, double *sigma,
    double *x, double *z, int *iv,
    double *weigh,
    double *result, double *grad, double *hess, int *derivin,
    int *modelin, int *hyper, double *parm, double *bigv)
{
    int leny = lenyin[0];
    int lenfix = lenfixin[0];
    int lenran = lenranin[0];
    int lenvar = lenvarin[0];
    int ncolx = ncolxin[0];
    int ncolz = ncolzin[0];
    int deriv = derivin[0];
    int nmiss = nmissin[0];
    int model = modelin[0];
    int ncoly = ncolyin[0];

    int nparm = lenfix + lenvar;
    int nparmsq = nparm * nparm;

    int i, j, k;

    double sumweigh;

    double *mygrad = (double *) R_alloc(nparm, sizeof(double));
    double *myhess = (double *) R_alloc(nparmsq, sizeof(double));

#ifdef DEBUG_FOO
    printf("This is bnlogl.c\n");
    printf("leny = %d\n", leny);
    printf("lenfix = %d\n", lenfix);
    printf("lenran = %d\n", lenran);
    printf("lenvar = %d\n", lenvar);
    printf("ncolx = %d\n", ncolx);
    printf("ncolz = %d\n", ncolz);
    printf("deriv = %d\n", deriv);
    printf("nmiss = %d\n", nmiss);
    printf("model = %d\n", model);
    printf("ncoly = %d\n", ncoly);
#endif /* DEBUG_FOO */

    /* NEW! deriv == 3 means output "big V" */
    if (deriv < 0 || deriv > 3)
        error("argument deriv must be 0, 1, 2, or 3");

    /* check weigh */
    sumweigh = 0.0;
    for (i = 0; i < ncoly; ++i) {
        if (weigh[i] <= 0.0)
            error("non-positive weight");
        sumweigh += weigh[i];
    }

    result[0] = 0.0;
    if (deriv >= 1)
        for (i = 0; i < nparm; i++)
            grad[i] = 0.0;
    if (deriv >= 2)
        for (i = 0; i < nparmsq; i++) {
            hess[i] = 0.0;
            if (deriv >= 3)
               bigv[i] = 0.0;
        }

    /* copied from foonew.c */
    /* see also RNG.c in main R sources */
    SEXP seeds;
    GetRNGstate();
    PutRNGstate();
    PROTECT(seeds = findVarInFrame(R_GlobalEnv, R_SeedsSymbol));
    if (seeds == R_UnboundValue)
        error(".Random.seed doesn't exist");

    for (i = 0, k = 0; i < ncoly; i++, k += leny) {
        double foo;
        int myderiv = deriv <= 2 ? deriv : 2;
        int myfalse = FALSE;
        double w = weigh[i];

        /* reinitialize .Random.seed */
        defineVar(R_SeedsSymbol, seeds, R_GlobalEnv);
        GetRNGstate();

#ifdef DEBUG_FOO
        printf("   i = %d in bnlogl\n", i);
#endif /* DEBUG_FOO */

        bnmarg(&leny, &lenfix, &lenran, &lenvar,
            &ncolx, &ncolz,
            &nmiss,
            &y[k], theta, sigma,
            x, z, iv,
            &foo, mygrad, myhess, &myderiv,
            &model, hyper, parm, NULL, &myfalse);
        result[0] += w * foo;
        if (deriv >= 1)
            for (j = 0; j < nparm; j++)
                grad[j] += w * mygrad[j];
        if (deriv >= 2) {
            int j1, j2;
            for (j1 = 0, j = 0; j1 < nparm; j1++)
                for (j2 = 0; j2 < nparm; j2++, j++) {
                    double foo = mygrad[j1] * mygrad[j2];
                    hess[j] += w * (myhess[j] - foo);
                    if (deriv >= 3)
                        bigv[j] += w * foo;
                }
        }
        PutRNGstate();
    }

    if (deriv >= 3)
    for (k = 0; k < nparmsq; k++)
        bigv[k] /= sumweigh;

    UNPROTECT(1);
}

