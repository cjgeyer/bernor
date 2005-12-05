
/* complete data log likelihood for normal random effects logistic (Bernoulli)
*  regression
*
*  input arguments
*
*      y : int, zero-or-one-valued, the response
*      b : double, real-valued (no constraints), the random effects
*      theta : double, real-valued (no constraints), the fixed effects
*      sigma : double, scale parameters for the random effects, requirement
*              nonnegativity removed in version 0.3-4
*      leny : number of cases
*      lenran : number of random effects (length of b)
*      lenfix : number of fixed effects (length of theta)
*      lenvar : number of variance components
*      ncolx : number of columns of x (see below)
*      ncolz : number of columns of z (see below)
*      x : double, real-valued (no constraints), a leny by ncolx matrix
*              in R, a leny * ncolx vector in C, matrix stored in
*              column order, part of the fixed effect design matrix
*      z : double, real-valued (no constraints), a leny by ncolz matrix
*              in R, a leny * ncolz vector in C, matrix stored in
*              column order, part of the fixed effect design matrix
*      iv : variance component index, vector of length lenran, taking values
*              in 1, ..., lenvar, meaning: b[i] is multiplied by sigma[iv[i]]
*              wherever it appears
*      deriv : integer, 0, 1, or 2, the number of derivatives wanted
*              if deriv == 0, then neither grad or hess are touched
*              if deriv == 1, then hess is not touched
*
*  output arguments
*
*      result : double scalar, real or -Inf valued
*                   perhaps NaN valued if bad input
*      grad : double, nparm vector, where nparm == lenfix + lenvar,
*                 the gradient
*      hess : double, nparm by nparm matrix in R, nparm * nparm vector in C,
*                 where nparm == lenfix + lenvar, the hessian
*
*  value of result (in R notation)
*
*      eta <- x %*% theta + z %*% diag(sigma[iv]) %*% b
*      p <- 1 / (1 + exp(- eta))
*      result <- sum(log(p)[y == 1]) + sum(log(1 - p)[y == 0]) +
*          sum(dnorm(b, 0, 1, log = TRUE))
*
*/

#include <R.h>
#include <Rmath.h>
#include "bernor.h"

static double loginvlogit(double theta);
static double invlogit(double theta);

void
bernor(int *lenyin, int *lenfixin, int *lenranin, int *lenvarin,
    int *ncolxin, int *ncolzin,
    int *y, double *theta, double *sigma, double *b,
    double *x, double *z, int *iv,
    double *result, double *grad, double *hess, int *derivin)
{

#ifdef MYDEBUG
    puts("This is bernor.c");
#endif /* MYDEBUG */

    int leny = lenyin[0];
    int lenfix = lenfixin[0];
    int lenran = lenranin[0];
    int lenvar = lenvarin[0];
    int ncolx = ncolxin[0];
    int ncolz = ncolzin[0];
    int deriv = derivin[0];

    int nparm = lenfix + lenvar;
#ifdef DEBUG_BAZ
    int nparmsq = nparm * nparm;
#endif /* DEBUG_BAZ */

    double *eta = Calloc(leny, double);
    double *grad_eta = Calloc(leny, double);
    double *hess_eta = Calloc(leny, double);

    int i, j, k;

#ifdef MYDEBUG
    printf("leny = %d\n", leny);
    printf("lenfix = %d\n", lenfix);
    printf("lenran = %d\n", lenran);
    printf("lenvar = %d\n", lenvar);
    printf("ncolx = %d\n", ncolx);
    printf("ncolz = %d\n", ncolz);
    printf("deriv = %d\n", deriv);
#endif /* MYDEBUG */

    for (i = 0; i < leny; i++)
        if (! (y[i] == 0 || y[i] == 1))
            error("argument y must be zero-or-one valued");

#ifdef REQUIRE_SIGMA_NONNEGATIVE
    for (i = 0; i < lenvar; i++)
        if (sigma[i] < 0.0)
            error("argument sigma must be nonnegative-valued");
#endif /* REQUIRE_SIGMA_NONNEGATIVE */

    for (i = 0; i < lenran; i++)
        if (iv[i] <= 0 || iv[i] > lenvar)
            error("argument iv must be in 1, ..., lenvar");

    if (deriv < 0 || deriv > 2)
        error("argument deriv must be 0, 1, or 2");

    for (i = 0; i < leny; i++)
        eta[i] = 0.0;

    for (j = 0, k = 0; j < ncolx; j++) {
        double foo = theta[j];
        for (i = 0; i < leny; i++, k++)
            eta[i] += x[k] * foo;
    }

    for (j = 0, k = 0; j < ncolz; j++) {
        double foo = sigma[iv[j] - 1] * b[j];
        for (i = 0; i < leny; i++, k++)
            eta[i] += z[k] * foo;
    }

    result[0] = 0.0;

    for (i = 0; i < leny; i++)
        if (y[i] == 1)
            result[0] += loginvlogit(eta[i]);
        else
            result[0] += loginvlogit(- eta[i]);

    for (i = 0; i < lenran; i++)
        result[0] += dnorm(b[i], 0.0, 1.0, TRUE);

    /* done with value */
    if (deriv == 0)
        goto bailout;

    for (i = 0; i < nparm; i++)
        grad[i] = 0.0;

    for (i = 0; i < leny; i++)
        if (y[i] == 1) {
            /* grad_eta = 1 - p = q = invlogit(- eta) */
            grad_eta[i] = invlogit(- eta[i]);
        } else /* y[i] == 0 */ {
            /* grad_eta = 0 - p = - p = - invlogit(eta) */
            grad_eta[i] = - invlogit(eta[i]);
        }

    for (j = 0, k = 0; j < ncolx; j++)
        for (i = 0; i < leny; i++, k++)
            grad[j] += x[k] * grad_eta[i];

    for (j = 0, k = 0; j < ncolz; j++) {
#ifdef DEBUG_BAZ
        if (! (0 <= j && j < lenran))
            error("DEBUG: j = %d out of range", j);
#endif /* DEBUG_BAZ */
        int ifoo = iv[j] - 1;
#ifdef DEBUG_BAZ
        if (! (0 <= ifoo && ifoo < lenvar))
            error("DEBUG: ifoo = %d out of range", ifoo);
        if (! (lenfix + ifoo < nparm))
            error("DEBUG: ifoo = %d out of range", ifoo);
#endif /* DEBUG_BAZ */
        for (i = 0; i < leny; i++, k++) {
#ifdef DEBUG_BAZ
            if (! (0 <= i && i < leny))
                error("DEBUG: i = %d out of range", i);
            if (! (0 <= k && k < leny * ncolz))
                error("DEBUG: k = %d out of range", k);
#endif /* DEBUG_BAZ */
            grad[lenfix + ifoo] += z[k] * grad_eta[i] * b[j];
        }
    }

    /* gradient done, return if hess not wanted */
    if (deriv == 1)
        goto bailout;

    for (i = 0; i < nparm * nparm; i++)
        hess[i] = 0.0;

    for (i = 0; i < leny; i++)
        hess_eta[i] = - invlogit(eta[i]) * invlogit(- eta[i]);


    /* x - x block */
    for (j = 0; j < ncolx; j++) {
        int jfoo = leny * j;
#ifdef DEBUG_BAZ
        if (! (0 <= jfoo && jfoo < (leny - 1) * ncolx))
            error("DEBUG: jfoo = %d out of range (for x)", jfoo);
#endif /* DEBUG_BAZ */
        for (k = j; k < ncolx; k++) {
            int kfoo = leny * k;
            int jkfoo = nparm * j + k;
            int kjfoo = nparm * k + j;
#ifdef DEBUG_BAZ
            if (! (0 <= kfoo && kfoo < (leny - 1) * ncolx))
                error("DEBUG: kfoo = %d out of range (for x)", kfoo);
            if (! (0 <= jkfoo && jkfoo < nparmsq))
                error("DEBUG: jkfoo = %d out of range", jkfoo);
            if (! (0 <= kjfoo && kjfoo < nparmsq))
                error("DEBUG: kjfoo = %d out of range", kjfoo);
#endif /* DEBUG_BAZ */
            for (i = 0; i < leny; i++) {
                double fooh = hess_eta[i] * x[jfoo + i] * x[kfoo + i];
                hess[jkfoo] += fooh;
                if (kjfoo != jkfoo)
                    hess[kjfoo] += fooh;
            }
        }
    }

    for (j = 0; j < ncolz; j++) {

#ifdef DEBUG_BAZ
        if (! (0 <= j && j < lenran))
            error("DEBUG: j = %d out of range (for b)", j);
#endif /* DEBUG_BAZ */

        int jfoo = leny * j;
        int jbar = lenfix + iv[j] - 1;
#ifdef DEBUG_BAZ
        if (! (0 <= jfoo && jfoo < (leny - 1) * ncolz))
            error("DEBUG: jfoo = %d out of range", jfoo);
        if (! (0 <= jbar && jbar < nparm))
            error("DEBUG: jbar = %d out of range", jbar);
#endif /* DEBUG_BAZ */

        /* z - z block */
        for (k = j; k < ncolz; k++) {
            int kfoo = leny * k;
            int kbar = lenfix + iv[k] - 1;
            int jkbar = nparm * jbar + kbar;
            int kjbar = nparm * kbar + jbar;
#ifdef DEBUG_BAZ
            if (! (0 <= kfoo && kfoo < (leny - 1) * ncolz))
                error("DEBUG: kfoo = %d out of range", kfoo);
            if (! (0 <= kbar && kbar < nparm))
                error("DEBUG: kbar = %d out of range", kbar);
            if (! (0 <= k && k < lenran))
                error("DEBUG: k = %d out of range", k);
            if (! (0 <= jkbar && jkbar < nparmsq))
                error("DEBUG: jkbar = %d out of range", jkbar);
            if (! (0 <= kjbar && kjbar < nparmsq))
                error("DEBUG: kjbar = %d out of range", kjbar);
#endif /* DEBUG_BAZ */
            for (i = 0; i < leny; i++) {
                double fooh = hess_eta[i] * z[jfoo + i] * z[kfoo + i]
                    * b[j] * b[k];
                hess[jkbar] += fooh;
                if (jbar != kbar)
                    hess[kjbar] += fooh;
            }
        }

        /* z - x block */
        for (k = 0; k < ncolx; k++) {
            int kfoo = leny * k;
            int jkfoo = nparm * jbar + k;
            int kjfoo = nparm * k + jbar;
#ifdef DEBUG_BAZ
            if (! (0 <= kfoo && kfoo < (leny - 1) * ncolx))
                error("DEBUG: kfoo = %d out of range (for x)", kfoo);
            if (! (0 <= jkfoo && jkfoo  < nparmsq))
                error("DEBUG: jkfoo = %d out of range (for hess)", jkfoo);
            if (! (0 <= kjfoo && kjfoo  < nparmsq))
                error("DEBUG: kjfoo = %d out of range (for hess)", kjfoo);
#endif /* DEBUG_BAZ */
            for (i = 0; i < leny; i++) {
                double fooh = hess_eta[i] * z[jfoo + i] * b[j] * x[kfoo + i];
                hess[jkfoo] += fooh;
                hess[kjfoo] += fooh;
            }
        }
    }

    bailout:
        Free(eta);
        Free(grad_eta);
        Free(hess_eta);
}

static double
loginvlogit(double theta)
{
    if (theta <= 0.0) {
        /* use p = exp(theta) / (1 + exp(theta))
        *      log(p) = theta - log(1 + exp(theta))
        *             = theta - log1p(exp(theta))
        */
        return theta - log1p(exp(theta));
    } else {
        /* use p = 1 / (1 + exp(- theta))
        *      log(p) = - log(1 + exp(- theta))
        *             = - log1p(exp(- theta))
        */
        return - log1p(exp(- theta));
    }
}

static double
invlogit(double theta)
{
    if (theta <= 0.0) {
        /* use p = exp(theta) / (1 + exp(theta)) */
        double foo = exp(theta);
        return foo / (1.0 + foo);
    } else {
        /* use p = 1 / (1 + exp(- theta)) */
        double foo = exp(- theta);
        return 1.0 / (1.0 + foo);
    }
}

