\name{heights}
\alias{heights}
\alias{heights.cover}
\alias{heights.incidence}
\alias{height.poset}

\title{
Heights
}
\description{
The function computes the vector of heights of poset elements.
}
\usage{
heights(z)
}

\arguments{
  \item{z}{
an object of class \code{cover}, \code{incidence} or \code{poset}.
}
}

\examples{
vl <- c(3, 2, 4)
prof <- var2prof(varlen = vl)
Z <- getzeta(prof)

heights(Z)
}