\name{is.linext}
\alias{is.linext}

\title{
is.linext
}
\description{
The function checks whether the input argument \code{order} is a linear extension of the poset represented by the incidence matrix \code{z}.
}
\usage{
is.linext(order, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{order}{
indexes of the poset elements (as rows and columns of \code{z} matrix) specifying the candidate linear order.
}
  \item{z}{
incidence matrix.
}
}
\examples{
Z <- getzeta(var2prof(varlen = c(3, 3)))
ranks <- c(1, 4, 2, 3, 5, 7, 6, 8, 9)
names(ranks) <- rownames(Z)
ranks
is.linext(order = ranks, z = Z)
}
