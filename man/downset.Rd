\name{downset}
\alias{downset}
\alias{downset.incidence}
\alias{downset.cover}

\title{
downset
}
\description{
The function computes a boolean vector identifying the poset elements below (or equal to) at least one element of the input subset \code{Q}.
}
\usage{
downset(z, ...)
\method{downset}{cover}(z, ...)
\method{downset}{incidence}(z, Q = NULL, ...)
}

\arguments{
  \item{z}{
a cover, or an incidence, matrix of S3 class \code{cover} or \code{incidence} respectively
}
  \item{Q}{
vector of indices identifying a subset of poset profiles
}
  \item{\dots}{
any of the above.
}
}
\examples{
z <- getzeta(var2prof(varlen = c(2, 2, 2)))

plot(z, col = 1 + c(1, 1, 0, 0, 1, 0, 0, 0) + c(0, 0, 0, 2, 0, 0, 2, 2), lwd = 2)

Q <- c(4, 7, 8)
rownames(z)[Q]
downset(z, Q)

Q <- c("211", "112", "111")
downset(z, Q)
}