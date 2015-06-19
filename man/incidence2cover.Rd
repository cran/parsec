\name{incidence2cover}
\alias{incidence2cover}
\alias{incidence2cover.incidence}

\title{
incidence2cover
}
\description{
The function computes the cover matrix associated to the input incidence matrix (i.e. the cover matrix whose transitive closure is the input incidence matrix).
}
\usage{
incidence2cover(z)
}

\arguments{
  \item{z}{
an incidence matrix, an object of class \code{incidence}.
}
}
\value{
Cover matrix, an object of class
\code{cover}.
}
\seealso{
\code{\link{cover2incidence}}
}