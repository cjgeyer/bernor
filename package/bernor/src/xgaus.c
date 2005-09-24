
/* IID standard normal -- see xmiss.c for more info */

#include <R.h>
#include <Rmath.h>
#include "bernor.h"

void
rgaus(int *hyper, double *parm, double *result)
{
    int d = hyper[0];
    int i;

    GetRNGstate();

    for (i = 0; i < d; i++)
        result[i] = parm[0] * norm_rand();

    PutRNGstate();
}

void
dgaus(double *x, int *hyper, double *parm, double *result)
{
    int d = hyper[0];
    int i;

    result[0] = 0.0;

    for (i = 0; i < d; i++)
        result[0] += dnorm(x[i], 0.0, parm[0], TRUE);
}

void
i1gaus(int *nhyper)
{
    nhyper[0] = 1;
}

void
i2gaus(int *hyper, int *nparm, int *nstate)
{
    nparm[0] = 1;
    nstate[0] = hyper[0];
}

