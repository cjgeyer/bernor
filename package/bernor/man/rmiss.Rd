\name{rmiss}
\alias{rmiss}
\title{Random Variates from Importance Sampling Model}
\description{
  Simulate realizations from importance sampling model.
}
\usage{
rmiss(model)
}
\arguments{
  \item{model}{the model, an object of class model produced by the
  \code{\link{model}} function.}
}
\value{
   a single realization from the model.
}
\seealso{
  \code{\link{bnmarg}},
  \code{\link{bnlogl}}.
}
\examples{
moo <- model("gauss", 7, 1.2)
rmiss(moo)
}
\keyword{misc}
