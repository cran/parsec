\name{lexLE}
\alias{lexLE}
\title{
Lexicographic linear extensions
}
\description{
The function generates all the lexicographic linear extensions of the partial order between the variables defined by the cover matrix \code{Lambda}.
}
\usage{
lexLE(Lambda)
}
\arguments{
  \item{Lambda}{
cover matrix related to the variables.
}
}
\value{
a list of vetors representing all variables orders compatible with the \code{Lambda} cover matrix.
}
\seealso{
\code{\link{getlambda}}
}
\author{
Alberto Arcagni
}
\examples{
Lambda <- getlambda(A < B, A < C, D < C)
lexLE(Lambda)
}
%# cover matrix of an antichain of the variables A, B, C and D
%vnames <- c("A", "B", "C", "D")
%Lambda <- matrix(FALSE, 4, 4)
%rownames(Lambda) <- colnames(Lambda) <- vnames
%Lambda["A", "B"] <- TRUE # A is covered by B
%Lambda["A", "C"] <- TRUE # A is covered by C
%Lambda["D", "C"] <- TRUE # D is covered by C