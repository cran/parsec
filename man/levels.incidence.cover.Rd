\name{levels.incidence and levels.cover}
\alias{levels.incidence}
\alias{levels.cover}

\title{
Levels of a poset
}
\description{
The methods return a vector associating each profile with the corresponding
level.
The behaviour of these methods for objects of classes \code{incidence} and
\code{cover} is different from the behaviour of function
\code{\link{levels}} for factors.
}
\usage{
\method{levels}{incidence}(x)
\method{levels}{cover}(x)
}

\arguments{
  \item{x}{
an object of class \code{cover} or \code{incidence}.
}
}

\seealso{
the function \code{\link{levels}} for objects of type factor
}

\examples{
vl <- c(3, 2, 4)
prof <- var2prof(varlen = vl)
Z <- getzeta(prof)

levels(Z)
}