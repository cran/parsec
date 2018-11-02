\name{transitiveClosure}
\alias{transitiveClosure}
\alias{transitiveClosure.default}
\alias{transitiveClosure.cover}
\alias{transitiveClosure.incidence}

\title{
Transitive Closure
}
\description{
The function computes the transitive closure of a reflexive and antisymmetric binary relation.
}
\usage{
transitiveClosure(m)
}

\arguments{
  \item{m}{
a generic square boolean matrix representing a reflexive and antisymmetric binary relation,
an object of class \code{cover} or an object of class \code{incidence}.
}
}

\value{
Incidence matrix of the transitive closure of the input matrix \code{m}.
}

\seealso{
\code{\link{is.partialorder}}
}
\examples{
m <- c(1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1)
m <- matrix(m, 4, 4)

transitiveClosure(m)
}