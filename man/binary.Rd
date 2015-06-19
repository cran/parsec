\name{binary}
\alias{binary}

\title{
binary
}
\description{
The function checks whether square matrix \code{m} represents a binary relation.
}
\usage{
binary(m)
}

\arguments{
  \item{m}{
a square matrix.
}
}
\seealso{
\code{\link{transitivity}}, \code{\link{reflexivity}},

\code{\link{antisymmetry}}, \code{\link{is.preorder}},

\code{\link{is.partialorder}}, \code{\link{validate.partialorder.incidence}}
}
\examples{
M <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
FALSE, TRUE, TRUE, TRUE, TRUE)
M <- matrix(M, 4, 4)
rownames(M) <- colnames(M) <- LETTERS[1:4]

binary(M)
}