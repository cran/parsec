\name{maximal}
\alias{maximal}
\alias{maximal.cover}
\alias{maximal.incidence}

\title{
Maximal elements of a poset.
}
\description{
The function returns a boolean vector identifying the maximal elements of the poset.
}
\usage{
maximal(z)
}

\arguments{
  \item{z}{
an object of class \code{cover} or \code{incidence}.
}
}
\examples{
vl <- c(3, 2, 4)
prof <- var2prof(varlen = vl)
Z <- getzeta(prof)

maximal(Z)
}