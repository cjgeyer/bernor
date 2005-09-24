\name{bernor}
\alias{bernor}
\title{Complete Data Log Density for Bernoulli-Normal Random Effects Model}
\description{
  Evaluate the complete data log density for Bernoulli regression model with
  normal random effects.
}
\usage{
bernor(y, beta, b, sigma, x, z, i, deriv = 0)
}
\arguments{
  \item{y}{a zero-one-valued (Bernoulli) vector, the response.}
  \item{beta}{the fixed effect vector.}
  \item{b}{the random effect vector.}
  \item{sigma}{the scale parameter vector for the fixed effects.}
  \item{x}{the model matrix for fixed effects.}
  \item{z}{the model matrix for random effects.}
  \item{i}{the index vector for random effects.}
  \item{deriv}{the number of derivatives wanted.  No more than 2.  Zero,
     the default, means no derivatives.}
}
\details{
  evaluates the function given by the R statements
  \preformatted{
    eta <- x \%*\% beta + z \%*\% (sigma[i] * b)
    p <- 1 / (1 + exp(- eta))
    sum(dbinom(y, 1, p, log = TRUE)) + sum(dnorm(b, log = TRUE))
  }
}
\value{
  A list containing some of the following components:
  \item{value}{the function value.}
  \item{gradient}{the gradient vector.  The length is
    \code{nparm}, which is \code{length(beta) + length(mu)}.}
  \item{hessian}{the hessian matrix.  The dimension is
    \code{nparm} by \code{nparm}.}
}
\seealso{
  \code{\link{bnmarg}},
  \code{\link{bnlogl}}.
}
\examples{
data(salam)
attach(salam)
beta <- rnorm(ncol(x))
sigma <- rgamma(length(unique(i)), 5, 5)
b <- rnorm(ncol(z))
bernor(y[ , 1], beta, b, sigma, x, z, i)
bernor(y[ , 1], beta, b, sigma, x, z, i, deriv = 2)
}
\keyword{misc}
