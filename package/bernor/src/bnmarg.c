
/* missing data log likelihood for normal random effects logistic (Bernoulli)
*  regression
*
*  arguments: same argument list as bernor except
*
*  modified input arguments
*
*      b is absent (we generate our own)
*
*  new input arguments
*
*      nmiss : int, positive, missing data sample size (number of b)
*      model = modelin[0] : model number
*      hyper : integer vector (length determined by model number)
*      parm : double vector (length and structure determined by model number
*          and hyper vector)
*      wantweigh : logical, scalar, what the name says (see below)
*
*  modified output arguments
*
*      hess is same type (double) and same dimension (nparm by nparm) but
*          is misnamed.  Not the hessian.  Rather what is called D^2 in
*          write-up.  Need to follow (after return to R) with
*
*              hess <- hess - outer(grad, grad)
*
*          to get true hessian.
*
*  new output arguments
*
*      weigh : double, nmiss vector, the weights
*          (but if wantweigh == FALSE return nothing,
*          weigh is not touched and need not be length nmiss)
*/

#include <R.h>
#include <Rmath.h>
#include "bernor.h"

#define TOO_BIG 5 /* protect against overflow in exp(x) */

void
bnmarg(int *lenyin, int *lenfixin, int *lenranin, int *lenvarin,
    int *ncolxin, int *ncolzin,
    int *nmissin,
    int *y, double *theta, double *sigma,
    double *x, double *z, int *iv,
    double *result, double *grad, double *hess, int *derivin,
    int *modelin, int *hyper, double *parm, double *weigh,
    int *wantweighin)
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
    int wantweigh = wantweighin[0];

#ifdef MYDEBUG
    printf("This is bnmarg.c\n");
#endif /* MYDEBUG */

    int nparm = lenfix + lenvar;
    int nparmsq = nparm * nparm;

    int i, j;

    double loga;
    double value;
    double *mygrad = Calloc(nparm, double);
    double *myhess = Calloc(nparmsq, double);
    double *b = Calloc(lenran, double);

#ifdef MYDEBUG
    printf("leny = %d\n", leny);
    printf("lenfix = %d\n", lenfix);
    printf("lenran = %d\n", lenran);
    printf("lenvar = %d\n", lenvar);
    printf("ncolx = %d\n", ncolx);
    printf("ncolz = %d\n", ncolz);
    printf("nmiss = %d\n", nmiss);
    printf("model = %d\n", model);
    printf("wantweigh = %d\n", wantweigh);
#endif /* MYDEBUG */

    /* initialize */
    value = 0.0;
    if (deriv >= 1)
        for (j = 0; j < nparm; j++)
            grad[j] = 0.0;
    if (deriv >= 2)
        for (j = 0; j < nparmsq; j++)
            hess[j] = 0.0;
    loga = R_NegInf;

    double sumweigh = 0.0;
    for (i = 0; i < nmiss; i++) {
        double foo, bar;

        rmiss(&model, hyper, parm, b);
        bernor(&leny, &lenfix, &lenran, &lenvar, &ncolx,
            &ncolz, y, theta, sigma, b,
            x, z, iv, &foo, mygrad, myhess, &deriv);
        dmiss(b, &model, hyper, parm, &bar);

        double foombar = foo - bar;
#ifdef MYDEBUG_OOPS
        printf("i = %d", i);
        printf(", foo = %e", foo);
        printf(", bar = %e", bar);
        printf(", foombar = %e\n", foombar);
#endif /* MYDEBUG_OOPS */

        /* adjust loga */
        if (i == 0) {
            /* special case first time through */
            loga = foombar;
        } else {
            double log_weigh_i = foombar - loga;
            if (log_weigh_i > TOO_BIG) {
                double loga_prime = log_weigh_i;
                double fudge = exp(loga - loga_prime);
                if (wantweigh)
                    for (j = 0; j < i; j++)
                        weigh[j] *= fudge;
                sumweigh *= fudge;
                if (deriv >= 1)
                    for (j = 0; j < nparm; j++)
                        grad[j] *= fudge;
                if (deriv >= 2)
                    for (j = 0; j < nparmsq; j++)
                        hess[j] *= fudge;
                loga = loga_prime;
            }
        }

        /* add in i-th terms */
        double weigh_i = exp(foombar - loga);
        sumweigh += weigh_i;
        if (wantweigh)
            weigh[i] = weigh_i;
        if (deriv >= 1)
            for (j = 0; j < nparm; j++)
                grad[j] += mygrad[j] * weigh_i;
        if (deriv >= 2) {
            int j1, j2;
            for (j1 = 0, j = 0; j1 < nparm; j1++)
                for (j2 = 0; j2 < nparm; j2++, j++)
                    hess[j] += (myhess[j] + mygrad[j1] * mygrad[j2]) * weigh_i;
        }
    }

    result[0] = loga + log(sumweigh / nmiss);

    if (! R_finite(result[0])) {
#ifdef MYDEBUG_OOPS
        printf("bnmarg: Not finite!\n");
        printf("    loga = %f\n", loga);
        printf("    sumweigh = %f\n", sumweigh);
        printf("    result[0] = %f\n", result[0]);
#endif /* MYDEBUG_OOPS */
        if (deriv >= 1)
            for (i = 0; i < nparm; i++)
                grad[i] = R_NaN;
        if (deriv == 2)
            for (i = 0; i < nparmsq; i++)
                hess[i] = R_NaN;
        goto bailout;
    }

    if (wantweigh)
        for (i = 0; i < nmiss; i++)
            weigh[i] /= sumweigh;

    if (deriv >= 1)
        for (j = 0; j < nparm; j++)
            grad[j] /= sumweigh;

    if (deriv >= 2)
        for (j = 0; j < nparmsq; j++)
            hess[j] /= sumweigh;

    bailout:
        Free(mygrad);
        Free(myhess);
        Free(b);
}

