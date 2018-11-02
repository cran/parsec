\name{obsprof}
\alias{obsprof}
\alias{obsprof.wprof}

\title{
Remove unobserved profiles.
}
\description{
The function removes, from the set of possible profiles \code{prof} derived from the multi-indicator system, those unobserved in the input dataset (i.e. profiles with associated frequency equal to zero). It returns an object of class S3 \code{wprof}
comprising the observed profiles and their frequencies.
}
\usage{
obsprof(prof)
\method{obsprof}{wprof}(prof)
}

\arguments{
  \item{prof}{
object of S3 class \code{wprof}.
}
}

\examples{
prf <- var2prof(varlen = c(3, 3, 3))
prf$freq <- sample(c(0, 1), 3*3*3, replace = TRUE)
prf <- obsprof(prf)
plot(prf, shape = "equispaced")
}