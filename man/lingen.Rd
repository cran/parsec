\name{lingen}
\alias{lingen}

\title{
lingen
}
\description{
The function returns a vector of ranks identifying a linear extension of the poset defined by incidence matrix \code{z}.
}
\usage{
lingen(z)
}
\arguments{
  \item{z}{
an incidence matrix.
}
}
\examples{
Z <- getzeta(var2prof(varlen = c(3, 3)))
lingen(Z)
}
