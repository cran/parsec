\name{upset}
\alias{upset}
\alias{upset.incidence}
\alias{upset.cover}

\title{
upset
}
\description{
The function returns a boolean vector indicating which poset elements are above (or equal to) at least an element of subset \code{Q}.
}
\usage{
upset(z, ...)
\method{upset}{cover}(z, ...)
\method{upset}{incidence}(z, Q = NULL, ...)
}

\arguments{
  \item{z}{
a cover or an incidence matrix, of S3 classes \code{cover} or \code{incidence}, respectively.
}
  \item{Q}{
vector to select a subset of the profiles in the poset defined through \code{z}.
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
upset(z, Q)

Q <- c("211", "112", "111")
upset(z, Q)
}
