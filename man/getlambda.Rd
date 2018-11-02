\name{getlambda}
\alias{getlambda}

\title{
Object constructor for the incidence matrix representing a partial order on variables.
}
\description{
The function creates an object of class \code{incidence} representing a partial order on the set of variables.
}
\usage{
getlambda(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Cover relations between variable pairs.
}
}
\details{
Cover relations between pair of ariables are defined by the names of the two variables and the symbols \code{<} and \code{>}. For instance, if variable A is covered by variable B, write the cover relation as \code{A < B} or \code{B > A}. If a variable is not comparable to the others, write the name of the variable alone.
}
\value{
an object of class \code{incidence}.
}
\seealso{
\code{\link{plot.cover}}
}
\author{
Alberto Arcagni
}

\examples{
    Lambda <- getlambda(BOTTOM < A, B > BOTTOM, INCOMP)
    plot(Lambda)
}
