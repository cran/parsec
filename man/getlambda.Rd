\name{getlambda}
\alias{getlambda}

\title{
Object constructor for incidence matrix between the variables
}
\description{
The function creates an object of class \code{incidence} that defines the partial order between the variables.
}
\usage{
getlambda(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
The relations between the variables that define the structure of the poset, see details below.
}
}
\details{
The relations are defined for couple of variables. They are defined by the names of the two variables and the symbols \code{<} and \code{>}. For instance, if the variable A is covered by the variable B, write the relation as \code{A < B} or \code{B > A}. If a variable is not comparable with the others, write the name of the variable without comparisons.
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
