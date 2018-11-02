\name{colevels}
\alias{colevels}
\alias{colevels.cover}
\alias{colevels.incidence}

\title{
Colevels of a poset
}
\description{
The function returns colevels associated to poset elements.
}
\usage{
colevels(y)
}

\arguments{
  \item{y}{
an object of class \code{cover} or \code{incidence}.
}
}

\examples{
vl <- c(3, 2, 4)
prof <- var2prof(varlen = vl)
Z <- getzeta(prof)

colevels(Z)
}