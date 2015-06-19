\name{antisymmetry}
\alias{antisymmetry}

\title{
antisymmetry
}
\description{
The function checks whether boolean square matrix \code{m} represents an antisymmetric binary relation.
}
\usage{
antisymmetry(m)
}

\arguments{
  \item{m}{
a square matrix.
}
}
\seealso{
\code{\link{transitivity}}, \code{\link{binary}}, \code{\link{reflexivity}},

\code{\link{is.preorder}}, \code{\link{is.partialorder}},

\code{\link{validate.partialorder.incidence}}
}
\examples{
M <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
FALSE, TRUE, TRUE, TRUE, TRUE)
M <- matrix(M, 4, 4)
rownames(M) <- colnames(M) <- LETTERS[1:4]

antisymmetry(M)
}