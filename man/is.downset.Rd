\name{is.downset}
\alias{is.downset}

\title{
is.downset
}
\description{
The function checks whether the input set of poset elements \code{Q} is a downset of the poset represented by the incidence matrix \code{z}.
}
\usage{
is.downset(z, Q = 1)
}

\arguments{
  \item{z}{
incidence matrix
}
  \item{Q}{
vector identifying the input set of profiles.
}
}
\examples{
z <- getzeta(var2prof(varlen = c(2, 2, 2)))

plot(z, col = 1 + c(1, 1, 0, 0, 1, 0, 0, 0) + c(0, 0, 0, 2, 0, 0, 2, 2), lwd = 2)

Q <- c(4, 7, 8)
rownames(z)[Q]
is.downset(z, Q)

Q <- c("211", "112", "111")
is.downset(z, Q)
}