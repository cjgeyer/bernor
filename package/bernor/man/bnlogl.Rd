\name{bnlogl}
\alias{bnlogl}
\title{Observed Data Log Likelihood for Bernoulli-Normal Random Effects Model}
\description{
  Evaluate by Monte Carlo the observed data log likelihood for
  Bernoulli regression model with normal random effects.
}
\usage{
bnlogl(y, beta, sigma, nmiss, x, z, i, model, deriv = 0, weigh)
}
\arguments{
  \item{y}{a zero-one-valued (Bernoulli) matrix, the response.}
  \item{beta}{the fixed effect vector.}
  \item{sigma}{the scale parameter vector for the random effects.}
  \item{nmiss}{integer, the number of simulations of the missing data.}
  \item{x}{the model matrix for fixed effects.}
  \item{z}{the model matrix for random effects.}
  \item{i}{the index vector for random effects.}
  \item{model}{the model for the importance sampling distribution,
    an object of class model produced by the \code{\link{model}} function.}
  \item{deriv}{the number of derivatives wanted.  No more than 3.  Zero,
     the default, means no derivatives.  Three is a kludge.  It doesn't
     mean third derivatives but to output ``big V hat''.}
  \item{weigh}{weights.  Positive integer valued vector of
     length \code{ncol(y)}.  May be missing in which case all weights one
     is assumed.}
}
\details{
  evaluates by good old-fashioned (IID) Monte Carlo observed data
  log density as if doing the R statements
  \preformatted{
    logf <- rep(NA, nmiss)
    nobs <- ncol(y) 
    save.Random.seed <- .Random.seed
    for (j in 1:nmiss) {
        .Random.seed <<- save.Random.seed
        logf[j] <- bnmarg(y[ , j], beta, b, sigma, x, z, i)$value
    }
    sum(weigh * logf)
  }
}
\value{
  A list containing some of the following components:
  \item{value}{the function value.}
  \item{gradient}{the gradient vector.  The length is
    \code{nparm}, which is \code{length(beta) + length(mu)}.}
  \item{hessian}{the hessian matrix.  The dimension is
    \code{nparm} by \code{nparm}.}
  \item{bigv}{the ``big V hat'' matrix.  The dimension is
    \code{nparm} by \code{nparm}.}
}
\seealso{
  \code{\link{bernor}},
  \code{\link{bnmarg}}.
}
\examples{
data(salam)
attach(salam)
beta <- c(0.91, -3.01, -0.49, 3.54)
sigma <- c(1.18, 0.98)
moo <- model("gauss", length(i), 1)
nmiss <- 100
bnlogl(y, beta, sigma, nmiss, x, z, i, moo, deriv = 3)
}
\keyword{misc}
