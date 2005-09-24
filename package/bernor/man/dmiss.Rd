\name{dmiss}
\alias{dmiss}
\title{Log Density for Importance Sampling Model}
\description{
  Evaluate the log density for importance sampling model.
}
\usage{
dmiss(x, model)
}
\arguments{
  \item{x}{a numeric vector, the state.}
  \item{model}{the model, an object of class model produced by the
  \code{\link{model}} function.}
}
\value{
   the log density
}
\seealso{
  \code{\link{bnmarg}},
  \code{\link{bnlogl}}.
}
\examples{
moo <- model("gauss", 42, 1.2)
x <- rmiss(moo)
dmiss(x, moo)
}
\keyword{misc}
