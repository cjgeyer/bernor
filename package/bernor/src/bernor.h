
void
bernor(int *lenyin, int *lenfixin, int *lenranin, int *lenvarin,
    int *ncolxin, int *ncolzin,
    int *y, double *theta, double *sigma, double *b,
    double *x, double *z, int *iv,
    double *result, double *grad, double *hess, int *derivin);

void
dmiss(double *x, int *modelin, int *hyper, double *parm, double *result);

void
rmiss(int *modelin, int *hyper, double *parm, double *result);

void
bnmarg(int *lenyin, int *lenfixin, int *lenranin, int *lenvarin,
    int *ncolxin, int *ncolzin,
    int *nmissin,
    int *y, double *theta, double *sigma,
    double *x, double *z, int *iv,
    double *result, double *grad, double *hess, int *derivin,
    int *modelin, int *hyper, double *parm, double *wayout, int *wantweighin);

void
bnlogl(int *lenyin, int *lenfixin, int *lenranin, int *lenvarin,
    int *ncolxin, int *ncolzin,
    int *nmissin, int *ncolyin,
    int *y, double *theta, double *sigma,
    double *x, double *z, int *iv,
    double *weigh,
    double *result, double *grad, double *hess, int *derivin,
    int *modelin, int *hyper, double *parm, double *bigv);

void
bnbigw(int *lenyin, int *lenfixin, int *lenranin, int *lenvarin,
    int *ncolxin, int *ncolzin,
    int *nmissin, int *ncolyin,
    int *y, double *theta, double *sigma,
    double *x, double *z, int *iv,
    double *weigh,
    double *result,
    int *modelin, int *hyper, double *parm,
    int *nbatchin);

void
rgaus(int *hyper, double *parm, double *result);

void
dgaus(double *x, int *hyper, double *parm, double *result);

void
i1gaus(int *nhyper);

void
i2gaus(int *hyper, int *nparm, int *nstate);

void
rstud(int *hyper, double *parm, double *result);

void
dstud(double *x, int *hyper, double *parm, double *result);

void
i1stud(int *nhyper);

void
i2stud(int *hyper, int *nparm, int *nstate);

void
i1miss(int *modelin, int *nhyper);

void
i2miss(int *modelin, int *hyper, int *nparm, int *nstate);

