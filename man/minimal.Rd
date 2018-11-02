\name{minimal}
\alias{minimal}
\alias{minimal.cover}
\alias{minimal.incidence}

\title{
Minimal elements of a poset
}
\description{
The function returns a boolean vector identifying the minimal elements of the poset.
}
\usage{
minimal(z)
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

minimal(Z)
}