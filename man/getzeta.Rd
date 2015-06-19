\name{getzeta}
\alias{getzeta}
\alias{getzeta.wprof}

\title{
Incidence matrix generation
}
\description{
The function computes the incidence matrix from the set of profiles \code{y}.
The output is a boolean matrix of S3 class \code{incidence}.
}
\usage{
getzeta(y)
\method{getzeta}{wprof}(y)
}

\arguments{
  \item{y}{
the set of profiles, an object of S3 class \code{wprof}.
}
}

\examples{
prf <- var2prof(varlen = c(2, 3))
getzeta(prf)
}
