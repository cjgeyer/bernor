\name{salam}
\docType{data}
\alias{salam}
\title{Salamander Mating Data}
\description{
  This data set contains the data for the salamander mating
  experiment in McCullagh and Nelder.
}
\usage{data(salam)}
\format{
  A list containing four components:
  \tabular{ll}{
    \code{y} \tab response matrix \cr
    \code{x} \tab fixed effect model matrix \cr
    \code{z} \tab random effect model matrix \cr
    \code{i} \tab random effect index vector
  }
  The structure of these objects is described in the documentation
  for \code{\link{bnlogl}}.
}
\details{
  The data involve two salamander populations, called rough butt (R)
  and whiteside (W), and each Bernoulli response is the success or
  failure of a mating between two salamanders.  The four possible
  mating types are denoted R/R, R/W, W/R, and W/W (female/male).
  The parameterization used is as follows.  The four fixed effect parameters
  are level parameters for these mating types (column labels for \code{x}
  give the order).  The random effect parameters are the scale parameters
  for female and male, in that order.  Column labels for \code{z} indicate
  the animal associated with the random effect, \code{"fR1"} meaning female
  named R1, and so forth.
}
\source{McCullagh and Nelder (1989), pages 439-450.}
\references{
  McCullagh, P. and Nelder, J. A. (1989).
  Generalized Linear Models, second edition.
  London: Chapman & Hall.
}
\seealso{
  \code{\link{bernor}},
  \code{\link{bnmarg}},
  \code{\link{bnlogl}}.
}
\keyword{datasets}
