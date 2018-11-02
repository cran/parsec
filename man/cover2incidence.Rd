\name{cover2incidence}
\alias{cover2incidence}
\alias{cover2incidence.cover}

\title{
cover2incidence
}
\description{
The function computes the incidence matrix of a poset from its cover matrix.
}
\usage{
cover2incidence(g)
}

\arguments{
  \item{g}{
a cover matrix, an object of class \code{cover}
}
}
\value{
The function returns the corresponding incidence matrix, an object of class
\code{incidence}.
}
\seealso{
\code{\link{incidence2cover}}
}