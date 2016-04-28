\name{is.upset}
\alias{is.upset}

\title{
is.upset
}
\description{
The function returns whether the input set of profiles \code{Q} is an upset of the poset defined by the incidence matrix \code{z}.
}
\usage{
is.upset(z, Q = 1)
}

\arguments{
  \item{z}{
an incidence matrix.
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
is.upset(z, Q)

Q <- c("211", "112", "111")
is.upset(z, Q)
}