\name{incomparability}
\alias{incomp}
\alias{incomp.incidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Incomparability between profiles}
\description{
The function computes the set of pairwise incomparabilities between poset elements.
}
\usage{
incomp(z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
an incidence matrix.
}
}
\value{
A boolean matrix whose  element ij is \code{TRUE} when profiles i and j
are incomparable.
}

\seealso{
\code{\link{getzeta}}
}
\examples{
vl <- c(2, 2, 2)
pr <- var2prof(varlen = vl)
Z <- getzeta(pr)
incomp(Z)
}

