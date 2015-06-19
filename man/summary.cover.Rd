\name{summary.cover}
\alias{summary.cover}
\alias{summary.incidence}

\title{
Summary method for cover and incidence objects
}
\description{
The function returns a summary of cover and incidence S3 objects. At the moment, the
function returns only the number of profiles and the number of comparabilities.
}
\usage{
\method{summary}{cover}(object, ...)
\method{summary}{incidence}(object, ...)
}

\arguments{
  \item{object}{
a cover matrix or an incidence matrix.
}
  \item{\dots}{
added for consistency with the generic method.
}
}

\examples{
vl <- c(2, 3, 3)
prf <- var2prof(varlen = vl)
Z <- getzeta(prf)
summary(Z)
C <- incidence2cover(Z)
summary(C)
}