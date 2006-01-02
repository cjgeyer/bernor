\name{booth}
\docType{data}
\alias{booth}
\title{Toy Data from Booth and Hobert}
\description{
  This data set contains a simulated data from the paper of Booth and
  Hobert referenced below.
}
\usage{data(booth)}
\format{
  A list containing components:
  \tabular{ll}{
    \code{y} \tab response matrix \cr
    \code{x} \tab fixed effect model matrix \cr
    \code{z} \tab random effect model matrix \cr
    \code{i} \tab random effect index vector \cr
    \code{mu0} \tab \dQuote{simulation truth} fixed effect parameter \cr
    \code{sigma0} \tab \dQuote{simulation truth} random effect parameter \cr
    \code{theta0} \tab \dQuote{simulation truth} parameter vector
        \code{c(mu0, sigma0)} \cr
    \code{theta.hat.exact} \tab The maximum likelihood parameter vector
        calculated using the exact likelihood (calculated by numerical
        integration, not Monte Carlo) \cr
    \code{info0} \tab the expected Fisher information matrix for sample
        size one (one column of \code{y}) at the
        \dQuote{simulation truth} parameter vector \cr
    \code{bigw0} \tab the expected \dQuote{big W} matrix for sample
        size one at the \dQuote{simulation truth} parameter vector \cr
    \code{info.hat.exact} \tab the expected Fisher information matrix for
        sample size one (one column of \code{y}) at the
        \code{theta.hat.exact} parameter vector \cr
    \code{bigw.hat.exact} \tab the expected \dQuote{big W} matrix for sample
        size one at the \code{theta.hat.exact} parameter vector
  }
  The structure of these objects is described in the documentation
  for \code{\link{bnlogl}} and \code{\link{bnbigw}}.
}
\seealso{
  \code{\link{bnlogl}}, \code{\link{bnbigw}}.
}
\references{
  Booth, J. G. and Hobert, J. P. (1999).
  Maximizing generalized linear mixed model likelihoods with
    an automated Monte Carlo EM algorithm.
  \emph{Journal of the Royal Statistical Society
    Series B (Statistical Methodology)} 61, 265--285.
}
\keyword{datasets}
