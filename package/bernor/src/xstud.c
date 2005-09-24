
/* IID Student t -- see xmiss.c for more info */

#include <R.h>
#include <Rmath.h>
#include "bernor.h"

void
rstud(int *hyper, double *parm, double *result)
{
    int d = hyper[0];
    double scale = parm[0];
    double df = parm[1];
    int i;

    if (scale <= 0.0)
        error("scale parameter not positive");
    if (df <= 0.0)
        error("degrees of freedom not positive");

    GetRNGstate();

    for (i = 0; i < d; i++)
        result[i] = scale * rt(df);

    PutRNGstate();
}

void
dstud(double *x, int *hyper, double *parm, double *result)
{
    int d = hyper[0];
    double scale = parm[0];
    double df = parm[1];
    int i;

    if (scale <= 0.0)
        error("scale parameter not positive");
    if (df <= 0.0)
        error("degrees of freedom not positive");

    result[0] = 0.0;

    for (i = 0; i < d; i++)
        result[0] += dt(x[i] / scale, df, TRUE) - log(scale);
}

void
i1stud(int *nhyper)
{
    nhyper[0] = 1;
}

void
i2stud(int *hyper, int *nparm, int *nstate)
{
    nparm[0] = 2;
    nstate[0] = hyper[0];
}

