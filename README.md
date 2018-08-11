
R package to do logit-normal generalized linear mixed models.

Method: ordinary Monte Carlo with importance sampling.

The main point of the package was to provide the computing backing the paper

> Sung, Y. J. and Geyer, C. J. (2007).\
> Monte Carlo likelihood inference for missing data models.\
> *Annals of Statistics*, **35**, 990--1011.

The package works fine (although is a bit complicated to use) and does
random effects models of arbitrary complexity.  However, its importance
sampling scheme is not ideal and it does not give very good answers for
very complicated models.  Hence we have not put the package on CRAN.

Should install with no problem.  Passed R CMD check under R-3.5.1.
The latest version uses all of the native routine registration recommended
by Sections 5.4 and 6.1 of *Writing R Extensions*
(https://cran.r-project.org/doc/manuals/r-release/R-exts.html).

To install using devtools

    library(devtools)
    install_github("cjgeyer/bernor", subdir = "package/bernor")

