\name{model}
\alias{model}
\title{Make a Model Object}
\description{
  create an object of class \code{"model"} specifying an importance
  sampling distribution to be used by by \code{\link{bnmarg}}
  and \code{\link{bnlogl}}.
}
\usage{
model(name, hyper, parm = double(0))
}
\arguments{
  \item{name}{a character string, uses partial matching
    (see \code{\link{match.arg}}) to find match in \code{models()}.}
  \item{hyper}{a integer vector, that specifies any dimension information
    about the model, in particular, in conjunction with the model name,
    specifies the dimension and structure of the parameter vector and
    state vector.}
  \item{parm}{the parameter vector.}
}
\value{
  An object of class \code{"model"}.
}
\seealso{
  \code{\link{bnmarg}},
  \code{\link{bnlogl}}.
}
\examples{
model("gauss", 42, 1.2)
}
\keyword{misc}
