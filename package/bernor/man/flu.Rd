\name{flu}
\docType{data}
\alias{flu}
\title{Flu Data}
\description{
  This data set contains data on influenza infection used by Coull and
  Agresti (paper referenced below).
}
\usage{data(flu)}
\format{
  A data frame containing five components:
  \tabular{ll}{
    \code{y1} \tab response for year 1\cr
    \code{y2} \tab response for year 2\cr
    \code{y3} \tab response for year 3\cr
    \code{y4} \tab response for year 4\cr
    \code{nobs} \tab number of individuals having each response pattern \cr
  }
}
\seealso{
  \code{\link{bnlogl}}.
}
\references{
  Coull, B. A. and Agresti, A. (2000).
  Random effects modeling of multiple binomial responses using
    the multivariate binomial logit-normal distribution.
  \emph{Biometrics}, 56, 73--80.
}
\keyword{datasets}
