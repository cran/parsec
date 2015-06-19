\name{reflexivity}
\alias{reflexivity}

\title{
reflexivity
}
\description{
The function checks whether the input boolean square matrix \code{m} represents a reflexive binary relation.
}
\usage{
reflexivity(m)
}

\arguments{
  \item{m}{
a boolean square matrix.
}
}
\seealso{
\code{\link{transitivity}}, \code{\link{binary}}, \code{\link{antisymmetry}},

\code{\link{is.preorder}}, \code{\link{is.partialorder}},

\code{\link{validate.partialorder.incidence}}
}
\examples{
M <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
FALSE, TRUE, TRUE, TRUE, TRUE)
M <- matrix(M, 4, 4)
rownames(M) <- colnames(M) <- LETTERS[1:4]

reflexivity(M)
}