\name{LE}
\alias{LE}
\title{
Linear extensions
}
\description{
The function generates all of the linear extensions of the partial order defined by the incidence matrix \code{Lambda}.
}
\usage{
LE(Lambda)
}
\arguments{
  \item{Lambda}{
incidence matrix.
}
}
\value{
a list of vectors representing all linear orders compatible with the \code{Lambda} incidence matrix.
}
\seealso{
\code{\link{getlambda}}
}
\author{
Alberto Arcagni
}
\examples{
Lambda <- getlambda(A < B, A < C, D < C)
LE(Lambda)
}
%# cover matrix of an antichain of the variables A, B, C and D
%vnames <- c("A", "B", "C", "D")
%Lambda <- matrix(FALSE, 4, 4)
%rownames(Lambda) <- colnames(Lambda) <- vnames
%Lambda["A", "B"] <- TRUE # A is covered by B
%Lambda["A", "C"] <- TRUE # A is covered by C
%Lambda["D", "C"] <- TRUE # D is covered by C