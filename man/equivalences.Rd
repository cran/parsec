\name{equivalences}
\alias{equivalences}
\alias{equivalences.cover}
\alias{equivalences.incidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Equivalence classes in a poset.
}
\description{
The function computes the set of poset elements sharing the same upset and downset.
}
\usage{
equivalences(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{incidence} or \code{cover}.
}
}
\value{
The function computes a vector assigning an equivalence class to each profile. The vector is of class \code{factor}.
}

\author{
Arcagni A.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
Lmbd <- getlambda(A > B, A > C, B > D, A > E, B > E, C > F, C > G)
res <- equivalences(Lmbd)

equivalence_classes <- levels(res)
colrs <- sapply(res, function(x) which(equivalence_classes == x)) + 1
plot(Lmbd, col = colrs, lwd = 2)
}