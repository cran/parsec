\name{depths}
\alias{depths}
\alias{depths.cover}
\alias{depths.incidence}

\title{
Depths
}
\description{
The function computes the depths of poset elements.
}
\usage{
depths(z)
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

depths(Z)
}