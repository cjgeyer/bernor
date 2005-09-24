
/* missing data simulation and density
*
*  simulation distribution is arbitrary
*
*  input arguments (for simulation function: rmiss)
*
*      model = modelin[0] : model number
*      hyper : integer vector (length determined by model number -- see
*          function i1miss)
*      parm : double vector (length and structure determined by model number
*          and hyper vector -- see function i2miss)
*
*  output arguments (for simulation function: rmiss)
*
*      result : double vector (length and structure determined by model number
*          and hyper vector -- see function i2miss)
*
*  input arguments (for evaluation function: dmiss)
*
*      same as input arguments for simulation function except new argument
*
*      x : double vector (length and structure determined by model number
*          and hyper vector and the same length and structure as argument
*          result of the simulation function -- see function i2miss)
*
*  output arguments (for evaluation function: dmiss)
*
*      result : double vector of length 1, the value of the log density
*
*  info functions
*
*  input arguments (for first info function: i1miss)
*
*      model = modelin[0] : model number
*
*  output arguments (for first info function: i1miss)
*
*      nhyper : integer vector of length 1, the required length of hyper
*
*  input arguments (for second info function: i2miss)
*
*      model = modelin[0] : model number
*      hyper : integer vector (length determined by model number -- see
*          function i1miss)
*
*  output arguments (for second info function: i2miss)
*
*      nparm : integer vector of length 1, the required length of parm
*      nstate : integer vector of length 1, the required length of x in dmiss
*          and of result in rmiss
*/

#include <R.h>
#include "bernor.h"

typedef void (* Fp)();
typedef struct entry { Fp d; Fp r; Fp i1; Fp i2; } Entry;

static Entry FTab[] =
{
    {dgaus, rgaus, i1gaus, i2gaus},
    {dstud, rstud, i1stud, i2stud},
    {NULL, NULL, NULL, NULL},
};

void
rmiss(int *modelin, int *hyper, double *parm, double *result)
{
    int model = modelin[0];
    int i;

    for (i = 0; FTab[i].r != NULL; i++)
        if (i == model) {
            FTab[i].r(hyper, parm, result);
            return;
        }
    error("no such model");
}

void
dmiss(double *x, int *modelin, int *hyper, double *parm, double *result)
{
    int model = modelin[0];
    int i;

    for (i = 0; FTab[i].d != NULL; i++)
        if (i == model) {
            double myresult;
            FTab[i].d(x, hyper, parm, &myresult);
            result[0] = myresult;
            return;
        }
    error("no such model");
}

void
i1miss(int *modelin, int *nhyper)
{
    int model = modelin[0];
    int i;

    for (i = 0; FTab[i].i1 != NULL; i++)
        if (i == model) {
            int mynhyper;
            FTab[i].i1(&mynhyper);
            nhyper[0] = mynhyper;
            return;
        }
    error("no such model");
}

void
i2miss(int *modelin, int *hyper, int *nparm, int *nstate)
{
    int model = modelin[0];
    int i;

    for (i = 0; FTab[i].i2 != NULL; i++)
        if (i == model) {
            int mynparm;
            int mynstate;
            FTab[i].i2(hyper, &mynparm, &mynstate);
            nparm[0] = mynparm;
            nstate[0] = mynstate;
            return;
        }
    error("no such model");
}

