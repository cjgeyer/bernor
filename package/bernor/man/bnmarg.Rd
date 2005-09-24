\name{bnmarg}
\alias{bnmarg}
\title{Observed Data Log Density for Bernoulli-Normal Random Effects Model}
\description{
  Evaluate by Monte Carlo the observed data log density for
  Bernoulli regression model with normal random effects.
}
\usage{
bnmarg(y, beta, sigma, nmiss, x, z, i, model, deriv = 0, want.weights = FALSE)
}
\arguments{
  \item{y}{a zero-one-valued (Bernoulli) vector, the response.}
  \item{beta}{the fixed effect vector.}
  \item{sigma}{the scale parameter vector for the fixed effects.}
  \item{nmiss}{integer, the number of simulations of the missing data.}
  \item{x}{the model matrix for fixed effects.}
  \item{z}{the model matrix for random effects.}
  \item{i}{the index vector for random effects.}
  \item{model}{the model for the importance sampling distribution,
    an object of class model produced by the \code{\link{model}} function.}
  \item{deriv}{the number of derivatives wanted.  No more than 2.  Zero,
     the default, means no derivatives.}
  \item{want.weights}{logical, \code{TRUE} if want weights returned.}
}
\details{
  evaluates by good old-fashioned (IID) Monte Carlo observed data
  log density as if doing the R statements
  \preformatted{
    logf <- rep(NA, nmiss)
    logh <- rep(NA, nmiss)
    for (j in 1:nmiss) {
        b <- rmiss(model)
        out <- bernor(y, beta, b, sigma, x, z, i)
        logf[j] <- out$value
        logh[j] <- dmiss(b, model)
    }
    mean(exp(logf / logh))
  }
  or, more precisely, the same thing done so as to avoid overflow.
}
\value{
  A list containing some of the following components:
  \item{value}{the function value.}
  \item{gradient}{the gradient vector.  The length is
    \code{nparm}, which is \code{length(beta) + length(mu)}.}
  \item{hessian}{the hessian matrix.  The dimension is
    \code{nparm} by \code{nparm}.}
  \item{weigh}{the vector of normalized importance weights.}
}
\seealso{
  \code{\link{bernor}},
  \code{\link{bnlogl}}.
}
\examples{
data(salam)
attach(salam)
beta <- c(0.91, -3.01, -0.49, 3.54)
sigma <- c(1.18, 0.98)
moo <- model("gauss", length(i), 1)
nmiss <- 100
bnmarg(y[ , 1], beta, sigma, nmiss, x, z, i, moo, deriv = 2)
}
\keyword{misc}
