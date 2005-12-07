
/* missing data log likelihood for normal random effects logistic (Bernoulli)
*  regression
*
*  input arguments (same argument list as bnlogl except no deriv, and ...)
*
*  new input argument
*
*      nbatch : integer, scalar, number of batch means
*      weigh  : weights for columns of y
*
*  output arguments, just one, result, which (unlike other functions in
*      this package) is an nparm * nparm matrix (like output argument hess
*      in other functions).
*
*  ---------- Notes ----------
*
*  there are three nested loops
*
*  zero result
*  save initial value of random seed
*  outer loop (over batches) {
*      zero shat
*      save current value of random seed
*      middle loop (over observed data y) {
*          set random seed to initial value
*          calculate log f_theta(y) and derivative (function bnmarg)
*          set random seed to current value
*          inner loop (over some missing data b) {
*              generate one missing data point b
*              calculate log f_theta(b, y) and derivative (function bernor)
*              add contribution to shat
*          }
*          divide shat by blen * ncoly
*      }
*      add outer(shat, shat) to result
*  }
*  multiply result by blen / nbatch
*/

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include "bernor.h"

void
bnbigw(int *lenyin, int *lenfixin, int *lenranin, int *lenvarin,
    int *ncolxin, int *ncolzin,
    int *nmissin, int *ncolyin,
    int *y, double *theta, double *sigma,
    double *x, double *z, int *iv,
    double *weigh,
    double *result,
    int *modelin, int *hyper, double *parm,
    int *nbatchin)
{
    int leny = lenyin[0];
    int lenfix = lenfixin[0];
    int lenran = lenranin[0];
    int lenvar = lenvarin[0];
    int ncolx = ncolxin[0];
    int ncolz = ncolzin[0];
    int nmiss = nmissin[0];
    int model = modelin[0];
    int ncoly = ncolyin[0];
    int nbatch = nbatchin[0];

    int nparm = lenfix + lenvar;
    int nparmsq = nparm * nparm;
    int blen = nmiss / nbatch;

    int i, j, k, ibatch, jtoo;
    int j1, j2;

    double sumweigh;

    double *mygrad = (double *) R_alloc(nparm, sizeof(double));
    double *myhess = (double *) R_alloc(nparmsq, sizeof(double));
    double *shat = (double *) R_alloc(nparm, sizeof(double));
    double *qux = (double *) R_alloc(nparm, sizeof(double));
    double *b = (double *) R_alloc(lenran, sizeof(double));

#ifdef MYDEBUG
    printf("This is bnbigw.c\n");
    printf("leny = %d\n", leny);
    printf("lenfix = %d\n", lenfix);
    printf("lenran = %d\n", lenran);
    printf("lenvar = %d\n", lenvar);
    printf("ncolx = %d\n", ncolx);
    printf("ncolz = %d\n", ncolz);
    printf("nmiss = %d\n", nmiss);
    printf("model = %d\n", model);
    printf("ncoly = %d\n", ncoly);
    printf("nparm = %d\n", nparm);
    printf("nparmsq = %d\n", nparmsq);
    printf("nbatch = %d\n", nbatch);
    printf("blen = %d\n", blen);
#endif /* MYDEBUG */

    if (nmiss <= 0)
        error("nmiss not positive");
    if (nbatch <= 0)
        error("nbatch not positive");
    if (nmiss != nbatch * blen)
        error("nmiss not multiple of nbatch");
    if (blen <= 0)
        error("nmiss less than nbatch");

    /* check weigh */
    sumweigh = 0.0;
    for (i = 0; i < ncoly; ++i) {
        if (weigh[i] <= 0.0)
            error("non-positive weight");
        sumweigh += weigh[i];
    }

    /* zero result */
    for (i = 0; i < nparmsq; i++)
        result[i] = 0.0;

    /* copied from foonew.c and bnlogl.c and modified */
    /* see also RNG.c in main R sources */
    GetRNGstate();
    PutRNGstate();
    /* save initial value of random seed */
    SEXP seeds;
    PROTECT(seeds = findVarInFrame(R_GlobalEnv, R_SeedsSymbol));
    if (seeds == R_UnboundValue)
        error(".Random.seed doesn't exist");

#ifdef BLEAT
    printf("bnbigw: seeds (first 5 elements only)");
    for (k = 0; k < 5; k++) {
        if (k % 5 == 0)
            printf("\n    ");
        printf("%13d", INTEGER(seeds)[k]);
    }
    printf("\n");
#endif /* BLEAT */

    /* outer loop (over batches) */
    for (ibatch = 0; ibatch < nbatch; ibatch++) {

        /* zero shat */
        for (k = 0; k < nparm; k++)
            shat[k] = 0.0;

        /* save current value of random seed */
        SEXP cur_seeds;
        PROTECT(cur_seeds = findVarInFrame(R_GlobalEnv, R_SeedsSymbol));
        if (cur_seeds == R_UnboundValue)
            error(".Random.seed doesn't exist");

#ifdef BLEAT
        printf("bnbigw: cur_seeds (first 5 elements only), ibatch = %d",
            ibatch + 1);
        for (k = 0; k < 5; k++) {
            if (k % 5 == 0)
                printf("\n    ");
            printf("%13d", INTEGER(cur_seeds)[k]);
        }
        printf("\n");
#endif /* BLEAT */

        /* middle loop (over observed data y) */
        for (j = 0, jtoo = 0; j < ncoly; j++, jtoo += leny) {

            /* set random seed to initial value */
            defineVar(R_SeedsSymbol, seeds, R_GlobalEnv);

            /* calculate log f_theta(y) and derivative (function bnmarg) */

            int one = 1;
            int myfalse = FALSE;
            double foo;
            double w = weigh[j];

            bnmarg(&leny, &lenfix, &lenran, &lenvar,
                &ncolx, &ncolz,
                &nmiss,
                &y[jtoo], theta, sigma,
                x, z, iv,
                &foo, qux, myhess, &one,
                &model, hyper, parm, NULL, &myfalse);

            /* set random seed to current value */
            defineVar(R_SeedsSymbol, cur_seeds, R_GlobalEnv);

            /* inner loop (over some missing data b) */
            for (i = ibatch * blen; i < (ibatch + 1) * blen; i++) {

                /* generate one missing data point b */
                rmiss(&model, hyper, parm, b);

                /* calc log f_theta(b, y) and derivative (function bernor) */

                double bar, baz;

                bernor(&leny, &lenfix, &lenran, &lenvar, &ncolx,
                    &ncolz, &y[jtoo], theta, sigma, b,
                    x, z, iv, &bar, mygrad, myhess, &one);

                /* add contribution to shat */

                dmiss(b, &model, hyper, parm, &baz);
                double exweigh = exp(bar - foo - baz);
                for (k = 0; k < nparm; k++)
                    shat[k] += (mygrad[k] - qux[k]) * exweigh * w;
            }
        }

        UNPROTECT(1);

        /* divide shat by blen * sumweigh */
        double foomp = blen * sumweigh;
        for (k = 0; k < nparm; k++)
            shat[k] /= foomp;

        /* add outer(shat, shat) to result */
        for (j1 = 0, j = 0; j1 < nparm; j1++)
            for (j2 = 0; j2 < nparm; j2++, j++)
                result[j] += shat[j1] * shat[j2];
    }

    /* multiply result by blen / nbatch */
    double foomp = ((double) blen) / ((double) nbatch);
    for (k = 0; k < nparmsq; k++)
        result[k] *= foomp;

    UNPROTECT(1);
}

