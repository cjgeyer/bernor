\name{bnbigw}
\alias{bnbigw}
\title{Big W Matrix}
\description{
  Evaluate the big W matrix for Monte Carlo maximum likelihood for
  Bernoulli regression model with normal random effects.
}
\usage{
bnbigw(y, beta, sigma, nmiss, x, z, i, model, nbatch = 100)
}
\arguments{
  \item{y}{a zero-one-valued (Bernoulli) matrix, the response.}
  \item{beta}{the fixed effect vector.}
  \item{sigma}{the scale parameter vector for the fixed effects.}
  \item{nmiss}{integer, the number of simulations of the missing data.}
  \item{x}{the model matrix for fixed effects.}
  \item{z}{the model matrix for random effects.}
  \item{i}{the index vector for random effects.}
  \item{model}{the model for the importance sampling distribution,
    an object of class model produced by the \code{\link{model}} function.}
  \item{nbatch}{the number of batch means.}
}
\details{
  \code{.Random.seed} must be the same as before the \code{bnlogl}
  calculation for this calculation to be relevant.
}
\value{
  the big W matrix
}
\seealso{
  \code{\link{bnlogl}}.
}
\examples{
data(salam)
attach(salam)
beta <- c(0.91, -3.01, -0.49, 3.54)
sigma <- c(1.18, 0.98)
moo <- model("gauss", length(i), 1)
nmiss <- 100
set.seed(42)
bnlogl(y, beta, sigma, nmiss, x, z, i, moo, deriv = 3)
set.seed(42)
bnbigw(y, beta, sigma, nmiss, x, z, i, moo)
}
\keyword{misc}
