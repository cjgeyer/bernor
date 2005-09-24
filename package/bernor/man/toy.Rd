\name{toy}
\docType{data}
\alias{toy}
\title{Toy Mating Data}
\description{
  This data set contains a simulated data similar to the salamander mating
  experiment in McCullagh and Nelder.  It has larger sample size and is
  better behaved statistically.
}
\usage{data(toy)}
\format{
  A list containing four components:
  \tabular{ll}{
    \code{y} \tab response matrix \cr
    \code{x} \tab fixed effect model matrix \cr
    \code{z} \tab random effect model matrix \cr
    \code{i} \tab random effect index vector \cr
    \code{mu0} \tab \dQuote{simulation truth} fixed effect parameter \cr
    \code{sigma0} \tab \dQuote{simulation truth} random effect parameter \cr
    \code{theta0} \tab \dQuote{simulation truth} parameter vector
        \code{c(mu0, sigma0)}
  }
  The structure of these objects is described in the documentation
  for \code{\link{bnlogl}}.
}
\seealso{
  \code{\link{salam}},
  \code{\link{bnlogl}}.
}
\keyword{datasets}
